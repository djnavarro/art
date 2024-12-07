
options(
  arttools.bucket.remote = "https://storage.googleapis.com/djnavarro-art",
  arttools.bucket.local = "~/Buckets/djnavarro-art",
  arttools.repos.remote = "https://github.com/djnavarro",
  arttools.repos.local = "~/GitHub"
)

galleries <- readr::read_csv(
  here::here("_galleries.csv"),
  show_col_types = FALSE
)

add_image_html <- function(dat, preview = "preview", target = "target") {
  lines <- paste0(
    '   ',
    '<div class="g-col-12 g-col-md-4">',
    '<div class="card h-100">',
    '<a href="',
    dat[[target]],
    '"><img src="',
    dat[[preview]],
    '" class="card-img-top"></a>',
    '</div>',
    '</div>',
    '   '
  )
  cat(lines, sep="\n\n")
}

build_series <- function(series, seed = NULL) {
  ind <- which(galleries$series == series)
  preview <- galleries$preview_dir[ind]
  target <- galleries$target_dir[ind]
  format <- galleries$format[ind]

  manifest <- arttools::manifest_read(series)

  dat <- manifest |>
    dplyr::filter(folder %in% c(preview, target), file_format == format) |>
    dplyr::mutate(
      type = dplyr::case_when(
        folder == preview ~ "preview",
        folder == target ~ "target"
      ),
      path = arttools::bucket_remote_url(series, path)
    ) |>
    dplyr::select(type, path, file_name) |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = path
    )

  if (!is.null(seed)) {
    withr::with_seed(seed, dat <- dplyr::slice_sample(dat, prop = 1))
  }

  add_image_html(dat)
}

make_gallery <- function(series, force = FALSE) {

  ind <- which(galleries$series == series)
  if (!galleries$include[ind]) return()

  # ---- write yaml header ----
  repospec <- gsub("https://github.com/", "", galleries$repo[ind])
  lines <- c(
    '---',
    paste0('title: "', galleries$name[ind], '"'),
    paste0('date: ', galleries$date[ind]),
    paste0('repo: ', repospec),
    paste0('license-type: "', galleries$license[ind], '"'),
    'appendix-style: none',
    'page-layout: full'
  )
  img <- paste(
      galleries$bucket[ind],
      galleries$preview_image[ind],
      sep = "/"
  )
  lines <- c(lines, paste0('image: "', img, '"'))
  lines <- c(lines, paste0('image-alt: "', galleries$preview_alt[ind], '"'))
  lines <- c(lines, '---', '   ')

  # ---- write code chunk ----
  lines <- c(lines,
    '::: {.grid}',
    '  ',
    '```{r}',
    '#| echo: false',
    paste0('#| label: build-', galleries$series[ind]),
    '#| message: false',
    '#| results: asis',
    'source(here::here("_common.R"))',
    paste0(
      'build_series("',
      galleries$series[ind],
      '", seed = ',
      galleries$seed[ind],
      ')'
    ),
    '```',
    '   ',
    ':::',
    '   '
  )

  # --- write the quarto document to file ---
  series_short_name <- gsub("^series-", "", galleries$series[ind])
  gallery_dir <- here::here("gallery", series_short_name)
  gallery_qmd <- fs::path(gallery_dir, "index.qmd")
  if (force & fs::file_exists(gallery_qmd)) fs::file_delete(gallery_qmd)
  fs::dir_create(gallery_dir)
  if (!fs::file_exists(gallery_qmd)) {
    brio::write_lines(lines, gallery_qmd)
    cli::cli_alert_success(paste("gallery created:", series))
  }
}

make_all_galleries <- function(force = FALSE) {
  for(series in galleries$series) make_gallery(series, force = force)
}


