# Publish the local figures a slide-deck .qmd references to S:/figure (served at
# https://drhuyue.site:10002/sammo3182/figure/<file>) under a project prefix,
# then relink them in the qmd. Figures already on a URL are left alone.
# Run interactively; prompts fill in anything not supplied as an argument.
#
# Usage:
#   source("codes/publish_figures.R")
#   publishFigures("slides/guestLecture/language_book_cn.qmd", prefix = "languageBook")

pacman::p_load(fs, stringr, purrr, cli, readr, dplyr, tibble)

regex_image <- "\\.(png|jpe?g|gif|svg|pdf)$"
url_base <- "https://drhuyue.site:10002/sammo3182/figure/"

# Prompt wrapper: lower-cased answer, "" when non-interactive
askLower <- \(msg) str_to_lower(readline(msg))

escapeRegex <- \(x) str_replace_all(x, "[[:punct:]]", "\\\\\\0")

# Local image files referenced by `![](...)` tags in a qmd (remote URLs and
# data URIs are ignored; links whose file is missing are reported and dropped).
findLocalFigures <- function(qmd_file) {
  dir_qmd <- path_dir(path_abs(qmd_file))

  link_all <- read_lines(qmd_file) |>
    str_match_all("!\\[[^\\]]*\\]\\(\\s*([^)\\s]+)") |>
    map(\(m) m[, 2]) |>
    list_c() |>
    unique() |>
    str_remove("^\\./")

  # Keep relative paths to image files only (drop http(s)://, data:, //host)
  link_local <- link_all[
    !str_detect(link_all, "^([a-zA-Z][a-zA-Z0-9+.-]*:|//)") &
      str_detect(str_to_lower(link_all), regex_image)
  ]

  file_local <- path_norm(path(dir_qmd, link_local))
  is_missing <- !file_exists(file_local)
  if (any(is_missing)) {
    cli_alert_warning("Referenced but not found, skipped:")
    walk(link_local[is_missing], cli_li)
  }
  unique(file_local[!is_missing])
}

# Rewrite each image tag that points at a published local file to
# ![<caption>](<url>){<attrs>}. Existing captions and {...} attributes are kept
# (heights are tuned per slide); `attr_default` is added only when a tag has none.
relinkQmd <- function(
  qmd_file,
  file_plot,
  url_new,
  attr_default = '{fig-align="center" height=600}'
) {
  text_old <- read_lines(qmd_file)

  # Links as written in the qmd, relative to its folder (e.g. images/x/a.svg)
  link_old <- path_rel(file_plot, start = path_dir(path_abs(qmd_file))) |>
    as.character()

  # Fold over the links so each substitution builds on the previous text
  text_new <- reduce2(link_old, url_new, .init = text_old, \(text, old, url) {
    pattern <- str_c(
      "!\\[([^\\]]*)\\]\\(\\s*(?:\\./)?",
      escapeRegex(old),
      "\\s*\\)(\\{[^}]*\\})?"
    )
    str_replace_all(text, pattern, \(m) {
      parts <- str_match(m, pattern)
      attr_use <- if_else(is.na(parts[, 3]), attr_default, parts[, 3])
      str_c("![", parts[, 2], "](", url, ")", attr_use)
    })
  })

  n_changed <- sum(text_new != text_old)
  if (n_changed > 0) {
    write_lines(text_new, qmd_file)
    cli_alert_success("Updated {n_changed} line(s) in {path_file(qmd_file)}")
  }
  invisible(n_changed)
}

# Copy `file_plot` to `dir_target` under `prefix`, optionally delete the local
# copies, then relink the qmd. Returns a tibble of what happened.
publishFiles <- function(file_plot, prefix, dir_target, qmd_file) {
  prefix <- str_c(str_remove(prefix, "_$"), "_")

  name_new <- path_file(file_plot) |>
    (\(x) if_else(str_starts(x, fixed(prefix)), x, str_c(prefix, x)))()
  if (anyDuplicated(name_new) > 0) {
    cli_abort(
      "Different sources would be published under the same name: {unique(name_new[duplicated(name_new)])}"
    )
  }
  path_dest <- path(dir_target, name_new)

  # Per-file decision; existing files are overwritten unless the user skips
  action <- rep("copy", length(path_dest)) |> set_names(path_dest)
  path_exist <- path_dest[file_exists(path_dest)]

  if (length(path_exist) > 0) {
    cli_alert_warning("These files already exist in {dir_target}:")
    walk(path_exist, \(f) cli_li(path_file(f)))

    choice <- askLower(
      "Overwrite (o), skip (s), or decide file-by-file (d)? [o/s/d]: "
    )
    if (choice == "s") {
      action[path_exist] <- "skip"
    } else if (choice == "d") {
      answer_each <- map_chr(path_exist, \(f) {
        askLower(str_glue("  {path_file(f)} - overwrite (o) or skip (s)? [o/s]: "))
      })
      action[path_exist] <- if_else(answer_each == "s", "skip", "copy")
    }
  }

  is_copy <- action == "copy"
  dir_create(dir_target)
  file_copy(file_plot[is_copy], path_dest[is_copy], overwrite = TRUE)

  cli_alert_success("Copied {sum(is_copy)} file(s) to {dir_target}")
  if (any(!is_copy)) {
    cli_alert_info("Skipped {sum(!is_copy)} file(s) already present")
  }

  if (askLower("Delete the local copies that were published? [y/N]: ") == "y") {
    file_delete(file_plot[is_copy])
    cli_alert_success("Deleted {sum(is_copy)} local file(s)")
  } else {
    cli_alert_info("Local copies kept")
  }

  url_new <- str_c(url_base, name_new)
  relinkQmd(qmd_file, file_plot, url_new)

  tibble(
    source = file_plot,
    published = path_dest,
    url = url_new,
    action = action
  )
}

# Publish the local images that `qmd_file` references. Prefix defaults to the
# qmd's file name.
publishFigures <- function(
  qmd_file = NULL,
  prefix = NULL,
  dir_target = "S:/figure"
) {
  if (is.null(qmd_file) || !file_exists(qmd_file)) {
    qmd_file <- readline("Path to the .qmd file: ")
  }
  if (!file_exists(qmd_file)) {
    cli_abort("File not found: {qmd_file}")
  }

  file_plot <- findLocalFigures(qmd_file)
  if (length(file_plot) == 0) {
    cli_alert_info("No unpublished local figures in {path_file(qmd_file)}")
    return(invisible(NULL))
  }
  cli_alert_info(
    "{length(file_plot)} local figure(s) in {path_file(qmd_file)} not yet published:"
  )
  walk(path_rel(file_plot, start = path_dir(path_abs(qmd_file))), cli_li)

  if (is.null(prefix) || prefix == "") {
    prefix_default <- path_ext_remove(path_file(qmd_file))
    prefix <- readline(
      str_glue("Prefix to prepend to each filename [{prefix_default}]: ")
    )
    if (prefix == "") prefix <- prefix_default
  }

  publishFiles(file_plot, prefix, dir_target, qmd_file)
}
