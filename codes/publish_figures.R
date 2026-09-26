# Rename slide-deck plots with a project prefix and publish them to S:/figure,
# the folder served at https://drhuyue.site:10002/sammo3182/figure/<file>.
# Run interactively; prompts fill in anything not supplied as an argument.

library(fs)
library(stringr)
library(purrr)
library(cli)
library(readr)

# Rewrite local image links (e.g. "images/foo.png") in the .qmd file(s) that
# sit alongside `source_dir` so they point at the published URL instead.
update_qmd_links <- function(qmd_dir, folder_name, filename_url) {
  qmd_files <- dir_ls(qmd_dir, type = "file", regexp = "\\.qmd$")
  if (length(qmd_files) == 0) {
    cli_alert_info("No .qmd files found in {qmd_dir}; link update skipped")
    return(invisible(NULL))
  }

  old_links <- path(folder_name, names(filename_url)) |> as.character()

  walk(qmd_files, \(qmd_file) {
    text <- read_lines(qmd_file)

    n_replaced <- 0
    walk2(old_links, filename_url, \(old_link, url) {
      hits <- str_count(text, fixed(old_link))
      n_replaced <<- n_replaced + sum(hits)
      text <<- str_replace_all(text, fixed(old_link), url)
    })

    if (n_replaced > 0) {
      write_lines(text, qmd_file)
      cli_alert_success(
        "Updated {n_replaced} link(s) in {path_file(qmd_file)}"
      )
    }
  })
}

publish_figures <- function(
  source_dir = NULL,
  prefix = NULL,
  target_dir = "S:/figure"
) {
  if (is_null(source_dir) || !dir_exists(source_dir)) {
    source_dir <- readline("Folder containing the plots to publish: ")
  }
  if (!dir_exists(source_dir)) {
    cli_abort("Folder not found: {source_dir}")
  }

  plot_files <- dir_ls(
    source_dir,
    type = "file",
    regexp = "\\.(png|jpe?g|svg|pdf)$"
  )
  if (length(plot_files) == 0) {
    cli_abort("No image files (png/jpg/jpeg/svg/pdf) found in {source_dir}")
  }

  if (is_null(prefix) || prefix == "") {
    prefix <- readline("Prefix to prepend to each filename (e.g. dangtuan_): ")
  }
  prefix <- str_remove(prefix, "_$") |> (\(x) paste0(x, "_"))()

  new_names <- path_file(plot_files) |>
    (\(x) ifelse(str_starts(x, fixed(prefix)), x, paste0(prefix, x)))()
  dest_paths <- path(target_dir, new_names)

  already_present <- dest_paths[file_exists(dest_paths)]
  decision <- rep("copy", length(dest_paths)) |> set_names(dest_paths)

  if (length(already_present) > 0) {
    cli_alert_warning("These files already exist in {target_dir}:")
    walk(already_present, \(f) cli_li(path_file(f)))

    choice <- readline(
      "Overwrite (o), skip (s), or decide file-by-file (d)? [o/s/d]: "
    ) |>
      str_to_lower()

    if (choice == "s") {
      decision[already_present] <- "skip"
    } else if (choice == "d") {
      per_file <- map_chr(already_present, \(f) {
        readline(str_glue(
          "  {path_file(f)} - overwrite (o) or skip (s)? [o/s]: "
        )) |>
          str_to_lower()
      })
      decision[already_present] <- if_else(per_file == "s", "skip", "copy")
    }
    # choice == "o" (or anything else): default stays "copy", i.e. overwrite
  }

  to_copy <- decision == "copy"
  dir_create(target_dir)
  walk2(plot_files[to_copy], dest_paths[to_copy], \(from, to) {
    file_copy(from, to, overwrite = TRUE)
  })

  cli_alert_success("Copied {sum(to_copy)} file(s) to {target_dir}")
  if (any(!to_copy)) {
    cli_alert_info("Skipped {sum(!to_copy)} file(s) already present")
  }

  delete_choice <- readline(
    "Delete the local copies that were published? [y/N]: "
  ) |>
    str_to_lower()
  if (delete_choice == "y") {
    file_delete(plot_files[to_copy])
    cli_alert_success("Deleted {sum(to_copy)} local file(s) from {source_dir}")
  } else {
    cli_alert_info("Local copies kept in {source_dir}")
  }

  urls <- paste0("https://drhuyue.site:10002/sammo3182/figure/", new_names)

  update_qmd_links(
    qmd_dir = path_dir(source_dir),
    folder_name = path_file(source_dir),
    filename_url = set_names(urls, path_file(plot_files))
  )

  tibble::tibble(
    source = plot_files,
    published = dest_paths,
    url = urls,
    action = decision
  )
}

# Example:
# source("codes/publish_figures.R")
# publish_figures(
#   source_dir = "slides/guestLecture/inequality_comparison_plots",
#   prefix     = "inequalityCompare"
# )
