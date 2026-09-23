# Render a Quarto revealjs .qmd into a normal ("online") HTML and a fully
# self-contained ("offline") HTML that needs neither internet nor any
# companion file (fonts/css/js/images all inlined as data URIs / <style> /
# <script>).
#
# Works on any revealjs .qmd unmodified -- it does not rely on any
# project-specific params or helper functions inside the .qmd. Quarto's own
# `embed-resources: true` inlines most local assets, but empirically still
# leaves the revealjs core/plugin JS+CSS (and any remote-hosted image/css,
# e.g. a title-slide background image hosted elsewhere) as separate
# <script src>/<link href> references into the "<name>_files/" folder or a
# remote URL. This script does a second pass over the rendered HTML and
# inlines *everything* still referenced that way, local or remote, so the
# offline file has zero external dependencies.
#
# Usage (from repo root, in R/Positron):
#   source("codes/render_offline.R")
#   renderDual("slides/conference/organizedParticipation.qmd")

library(pacman)
p_load(stringr, readr, purrr, base64enc)

.mimeFromExt <- function(ext) {
  switch(
    tolower(ext),
    png = "image/png",
    jpg = ,
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    webp = "image/webp",
    "application/octet-stream"
  )
}

.isRemote <- function(url) grepl("^https?://", url)

.readBinResource <- function(url, base_dir) {
  if (.isRemote(url)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    readBin(tmp, "raw", file.info(tmp)$size)
  } else {
    path <- file.path(base_dir, utils::URLdecode(url))
    readBin(path, "raw", file.info(path)$size)
  }
}

.readTextResource <- function(url, base_dir) {
  if (.isRemote(url)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    readr::read_file(tmp)
  } else {
    readr::read_file(file.path(base_dir, utils::URLdecode(url)))
  }
}

# a resource reference worth inlining: not already a data: URI, and either a
# remote http(s) URL or a local relative path (never an absolute path/anchor)
.isInlinable <- function(url) {
  !startsWith(url, "data:") &&
    !startsWith(url, "#") &&
    !startsWith(url, "mailto:") &&
    (.isRemote(url) || !grepl("^([a-zA-Z]:)?/", url))
}

.inlineOneCss <- function(m, base_dir) {
  url <- stringr::str_match(m, 'href="([^"]+)"')[, 2]
  if (!.isInlinable(url)) {
    return(m)
  }
  css <- tryCatch(.readTextResource(url, base_dir), error = \(e) NA)
  if (is.na(css)) {
    warning("Could not inline stylesheet, left as-is: ", url)
    return(m)
  }
  paste0("<style>\n", css, "\n</style>")
}

.inlineOneScript <- function(m, base_dir) {
  url <- stringr::str_match(m, 'src="([^"]+)"')[, 2]
  if (!.isInlinable(url)) {
    return(m)
  }
  js <- tryCatch(.readTextResource(url, base_dir), error = \(e) NA)
  if (is.na(js)) {
    warning("Could not inline script, left as-is: ", url)
    return(m)
  }
  paste0("<script>\n", js, "\n</script>")
}

.inlineOneImg <- function(m, attr, base_dir) {
  url <- stringr::str_match(m, paste0(attr, '="([^"]+)"'))[, 2]
  if (!.isInlinable(url)) {
    return(m)
  }
  ext <- tools::file_ext(sub("[?#].*$", "", url))
  bin <- tryCatch(.readBinResource(url, base_dir), error = \(e) NULL)
  if (is.null(bin)) {
    warning("Could not inline image, left as-is: ", url)
    return(m)
  }
  uri <- paste0(
    "data:",
    .mimeFromExt(ext),
    ";base64,",
    base64enc::base64encode(bin)
  )
  paste0(attr, '="', uri, '"')
}

# inline every stylesheet link, script tag, and image reference (src /
# data-src / data-background-image / href) that isn't already a data: URI --
# whether it points at a remote URL or a local relative path/folder.
.inlineAllResources <- function(html, base_dir) {
  css_pattern <- '<link(?=[^>]*\\brel="stylesheet")(?=[^>]*\\bhref="[^"]+")[^>]*>'
  html <- stringr::str_replace_all(html, css_pattern, \(m) {
    purrr::map_chr(m, .inlineOneCss, base_dir = base_dir)
  })

  script_pattern <- '<script(?=[^>]*\\bsrc="[^"]+")[^>]*></script>'
  html <- stringr::str_replace_all(html, script_pattern, \(m) {
    purrr::map_chr(m, .inlineOneScript, base_dir = base_dir)
  })

  img_attrs <- c("src", "data-src", "data-background-image", "href")
  html <- purrr::reduce(
    img_attrs,
    \(acc, attr) {
      img_pattern <- paste0(
        attr,
        '="[^"]+\\.(?:png|jpe?g|gif|svg|webp)(?:[?#][^"]*)?"'
      )
      stringr::str_replace_all(
        acc,
        stringr::regex(img_pattern, ignore_case = TRUE),
        \(m) {
          purrr::map_chr(m, .inlineOneImg, attr = attr, base_dir = base_dir)
        }
      )
    },
    .init = html
  )

  html
}

# recursively copy a directory's contents into `to` (created if needed),
# overwriting anything already there -- base R has no direct equivalent
.copyDirContents <- function(from, to) {
  rel <- list.files(from, recursive = TRUE)
  if (length(rel) == 0) {
    return(invisible())
  }
  dest <- file.path(to, rel)
  for (d in unique(dirname(dest))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(file.path(from, rel), dest, overwrite = TRUE)
}

.ensureGitignored <- function(repo_root, pattern) {
  gi <- file.path(repo_root, ".gitignore")
  lines <- if (file.exists(gi)) readLines(gi, warn = FALSE) else character()
  if (!pattern %in% lines) {
    writeLines(c(lines, pattern), gi)
    message("Added '", pattern, "' to .gitignore")
  }
}

# walk upward from `path` looking for a .Rproj file or a .git folder
.findRepoRoot <- function(path) {
  dir <- normalizePath(dirname(path))
  repeat {
    if (
      length(Sys.glob(file.path(dir, "*.Rproj"))) > 0 ||
        dir.exists(file.path(dir, ".git"))
    ) {
      return(dir)
    }
    parent <- dirname(dir)
    if (parent == dir) {
      stop("Could not find repo root (.Rproj or .git) above ", path)
    }
    dir <- parent
  }
}

#' Render both the online and offline HTML for a revealjs .qmd
#'
#' @param qmd_path path to the .qmd file
#' @param seafile_dir destination to move the offline HTML to (overwritten
#'   if a file with the same name already exists there); the file no longer
#'   exists in the repo afterward
#' @param repo_root repo root, used to locate/update .gitignore
#' @param keep_offline_files keep the "<name>_files/" support folder (the
#'   one quarto produces for the document, reused by both the online and
#'   offline renders) instead of deleting it once inlined. Defaults to TRUE
#'   because the *online* HTML (which stays in the repo) references this
#'   folder by relative path rather than embedding it -- deleting it breaks
#'   the online render (e.g. reveal.js plugin scripts like the speaker-notes
#'   timer 404 and silently fail to load). Only pass FALSE if you don't care
#'   about the online HTML working, or will regenerate the folder before
#'   using it.
renderDual <- function(
  qmd_path,
  seafile_dir = "D:/Seafile/WW_share",
  repo_root = .findRepoRoot(qmd_path),
  keep_offline_files = TRUE
) {
  qmd_path <- normalizePath(qmd_path, mustWork = TRUE)
  dir <- dirname(qmd_path)
  base <- tools::file_path_sans_ext(basename(qmd_path))

  online_html <- file.path(dir, paste0(base, ".html"))
  offline_html <- file.path(dir, paste0(base, "_offline.html"))
  # Quarto names a document's supporting-resources folder after the *source*
  # .qmd's stem, not after -o's output basename -- so both the online and
  # offline renders below write into this same folder. The offline
  # (embed-resources: true) pass inlines what it can from it and then
  # deletes it, which can leave dangling references (e.g. via the lightbox
  # extension) to images that no longer exist on disk by the time we get to
  # the fallback-inlining step. We back it up after the online render (whose
  # figures are deterministic thanks to set.seed()) and restore it before
  # inlining so those files are still there to embed.
  resource_files_dir <- file.path(dir, paste0(base, "_files"))
  resource_files_backup <- file.path(
    tempdir(),
    paste0(base, "_files_backup_", as.integer(Sys.time()))
  )

  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)

  message("Rendering online version...")
  system2(
    "quarto",
    c(
      "render",
      shQuote(qmd_path),
      "--to",
      "revealjs",
      "-M",
      "embed-resources:false",
      "-o",
      shQuote(basename(online_html))
    )
  )

  if (dir.exists(resource_files_dir)) {
    dir.create(resource_files_backup, recursive = TRUE, showWarnings = FALSE)
    .copyDirContents(resource_files_dir, resource_files_backup)
  }

  message("Rendering offline base (embed-resources: true)...")
  system2(
    "quarto",
    c(
      "render",
      shQuote(qmd_path),
      "--to",
      "revealjs",
      "-M",
      "embed-resources:true",
      "-o",
      shQuote(basename(offline_html))
    )
  )

  if (dir.exists(resource_files_backup)) {
    .copyDirContents(resource_files_backup, resource_files_dir)
    unlink(resource_files_backup, recursive = TRUE)
  }

  message("Inlining every remaining local/remote resource...")
  html <- offline_html |>
    readr::read_file() |>
    .inlineAllResources(base_dir = dir)
  readr::write_file(html, offline_html)

  leftover <- stringr::str_extract_all(
    html,
    paste0(base, "(_offline)?_files/[^\"'\\s]*")
  )[[1]] |>
    unique()
  if (length(leftover) > 0) {
    warning(
      "Offline HTML still references ",
      length(leftover),
      " local file(s) that could not be inlined -- it is NOT fully standalone:\n  ",
      paste(leftover, collapse = "\n  ")
    )
  }

  if (!keep_offline_files && dir.exists(resource_files_dir)) {
    unlink(resource_files_dir, recursive = TRUE)
  }

  if (!dir.exists(seafile_dir)) {
    stop("Seafile path not found: ", seafile_dir)
  }
  seafile_html <- file.path(seafile_dir, basename(offline_html))
  message("Moving offline HTML to ", seafile_dir, " ...")
  if (file.exists(seafile_html)) unlink(seafile_html)
  moved <- file.rename(offline_html, seafile_html)
  if (!moved) {
    # file.rename can fail across filesystems/drives; fall back to copy + delete
    file.copy(offline_html, seafile_html, overwrite = TRUE)
    unlink(offline_html)
  }

  .ensureGitignored(repo_root, "*_offline.html")

  invisible(list(
    online = online_html,
    offline = seafile_html,
    self_contained = length(leftover) == 0
  ))
}
