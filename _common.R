
galleries <- readr::read_csv(
  here::here("_galleries.csv"),
  show_col_types = FALSE
)

build_url <- function(page, base_url) {
  paste0(base_url, "/", page, "/")
}

add_image_html <- function(dat, thumbnail = "thumbnail", target = "target") {
  lines <- paste0(
    '   ',
    '<div class="g-col-12 g-col-md-4">',
    '<div class="card h-100">',
    '<a href="',
    dat[[target]],
    '"><img src="',
    dat[[thumbnail]],
    '" class="card-img-top"></a>',
    '</div>',
    '</div>',
    '   '
  )
  cat(lines, sep="\n\n")
}

read_manifest <- function(url,
                          manifest = "manifest.csv",
                          preview_size = 800,
                          image_size = 4000,
                          image_format = "png") {

  readr::read_csv(paste0(url, manifest)) |>
    dplyr::filter(
      resolution %in% c(preview_size, image_size),
      format == image_format
    ) |>
    dplyr::mutate(
      type = dplyr::case_when(
        resolution == preview_size ~ "thumbnail",
        resolution == image_size ~ "target"
      ),
      path = paste0(url, path)
    ) |>
    dplyr::select(-resolution) |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = path
    )
}

build_series <- function(series) {

  ind <- which(galleries$series == series)

  dat <- paste0("series-", series) |>
    build_url(galleries$base_url[ind]) |>
    read_manifest(
      preview_size = galleries$preview_size[ind],
      image_size = galleries$image_size[ind],
      image_format = galleries$format[ind]
    ) |>
    add_image_html()
}

make_gallery <- function(series, force = FALSE) {

  ind <- which(galleries$series == series)
  if(galleries$include[ind] && !is.na(galleries$manifest[ind])) {

    repo <- paste0("djnavarro/series-", series)

    lines <- c(
      '---',
      paste0('title: "', galleries$name[ind], '"'),
      paste0('date: ', galleries$date[ind]),
      paste0('repo: ', repo),
      'page-layout: full'
    )

    if(!is.na(galleries$preview_image[ind])) {
      page <- paste0("series-", series)
      url <- build_url(page, galleries$base_url[ind])
      img <- paste0(url, galleries$preview_image[ind])
      lines <- c(lines, paste0('image: "', img, '"'))
    }
    if(!is.na(galleries$preview_alt[ind])) {
      lines <- c(lines, paste0('image-alt: "', galleries$preview_alt[ind], '"'))
    }

    lines <- c(lines,
      '---',
      '   ',
      '::: {.grid}',
      '  ',
      '```{r}',
      '#| echo: false',
      '#| message: false',
      '#| results: asis',
      'source(here::here("_common.R"))',
      paste0('build_series("', galleries$series[ind], '")'),
      '```',
      '   ',
      ':::',
      '   '
    )

    gallery_dir <- here::here("gallery", galleries$series[ind])
    gallery_qmd <- fs::path(gallery_dir, "index.qmd")

    if (force) {
      if (fs::file_exists(gallery_qmd)) {
        fs::file_delete(gallery_qmd)
      }
    }
    fs::dir_create(gallery_dir)
    if (!fs::file_exists(gallery_qmd)) {
      brio::write_lines(lines, gallery_qmd)
      cli::cli_alert_success(paste("gallery created:", series))
    }
  }
}

make_all_galleries <- function(force = FALSE) {
  for(series in galleries$series) {
    make_gallery(series, force = force)
  }
}


