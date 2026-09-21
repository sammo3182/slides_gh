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
#   source("scripts/render_offline.R")
#   renderDual("slides/conference/organizedParticipation.qmd")

library(pacman)
p_load(stringr, readr, purrr, base64enc)

.mimeFromExt <- function(ext) {
  switch(tolower(ext),
    png  = "image/png",
    jpg  = ,
    jpeg = "image/jpeg",
    gif  = "image/gif",
    svg  = "image/svg+xml",
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
  !startsWith(url, "data:") && !startsWith(url, "#") && !startsWith(url, "mailto:") &&
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
  uri <- paste0("data:", .mimeFromExt(ext), ";base64,", base64enc::base64encode(bin))
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
  html <- purrr::reduce(img_attrs, \(acc, attr) {
    img_pattern <- paste0(attr, '="[^"]+\\.(?:png|jpe?g|gif|svg|webp)(?:[?#][^"]*)?"')
    stringr::str_replace_all(acc, stringr::regex(img_pattern, ignore_case = TRUE), \(m) {
      purrr::map_chr(m, .inlineOneImg, attr = attr, base_dir = base_dir)
    })
  }, .init = html)

  html
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
    if (length(Sys.glob(file.path(dir, "*.Rproj"))) > 0 || dir.exists(file.path(dir, ".git"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find repo root (.Rproj or .git) above ", path)
    dir <- parent
  }
}

#' Render both the online and offline HTML for a revealjs .qmd
#'
#' @param qmd_path path to the .qmd file
#' @param seafile_dir destination to copy the offline HTML to (overwritten
#'   if a file with the same name already exists there)
#' @param repo_root repo root, used to locate/update .gitignore
#' @param keep_offline_files keep the "<name>_offline_files/" support folder
#'   (if quarto produced one) instead of deleting it once inlined
renderDual <- function(qmd_path,
                        seafile_dir = "D:/Seafile/WW_share",
                        repo_root = .findRepoRoot(qmd_path),
                        keep_offline_files = FALSE) {
  qmd_path <- normalizePath(qmd_path, mustWork = TRUE)
  dir  <- dirname(qmd_path)
  base <- tools::file_path_sans_ext(basename(qmd_path))

  online_html  <- file.path(dir, paste0(base, ".html"))
  offline_html <- file.path(dir, paste0(base, "_offline.html"))
  offline_files_dir <- file.path(dir, paste0(base, "_offline_files"))

  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)

  message("Rendering online version...")
  system2("quarto", c(
    "render", shQuote(qmd_path), "--to", "revealjs",
    "-M", "embed-resources:false",
    "-o", shQuote(basename(online_html))
  ))

  message("Rendering offline base (embed-resources: true)...")
  system2("quarto", c(
    "render", shQuote(qmd_path), "--to", "revealjs",
    "-M", "embed-resources:true",
    "-o", shQuote(basename(offline_html))
  ))

  message("Inlining every remaining local/remote resource...")
  html <- offline_html |>
    readr::read_file() |>
    .inlineAllResources(base_dir = dir)
  readr::write_file(html, offline_html)

  leftover <- stringr::str_extract_all(html, paste0(base, "(_offline)?_files/[^\"'\\s]*"))[[1]] |>
    unique()
  if (length(leftover) > 0) {
    warning(
      "Offline HTML still references ", length(leftover),
      " local file(s) that could not be inlined -- it is NOT fully standalone:\n  ",
      paste(leftover, collapse = "\n  ")
    )
  }

  if (!keep_offline_files && dir.exists(offline_files_dir)) {
    unlink(offline_files_dir, recursive = TRUE)
  }

  if (!dir.exists(seafile_dir)) {
    stop("Seafile path not found: ", seafile_dir)
  }
  message("Copying offline HTML to ", seafile_dir, " ...")
  file.copy(offline_html, file.path(seafile_dir, basename(offline_html)), overwrite = TRUE)

  .ensureGitignored(repo_root, "*_offline.html")

  invisible(list(online = online_html, offline = offline_html, self_contained = length(leftover) == 0))
}
