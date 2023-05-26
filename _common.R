
library(readr)
library(dplyr)
library(tidyr)

galleries <- read_csv(here::here("_galleries.csv"))

build_url <- function(page, base_url = "https://djnavarro.net") {
  paste0(base_url, "/", page, "/")
}

add_image_html <- function(dat, thumbnail = "thumbnail", target = "target") {
  tags <- paste0("<a href='", dat[[target]], "'><img src='", dat[[thumbnail]], "'><a>")
  cat(tags, sep = "\n\n")
}

read_manifest <- function(url,
                          manifest = "manifest.csv",
                          preview_size = 800,
                          image_size = 4000,
                          format = "png") {

  read_csv(paste0(url, manifest)) |>
    filter(
      resolution %in% c(preview_size, image_size),
      format == format
    ) |>
    mutate(
      type = case_when(
        resolution == preview_size ~ "thumbnail",
        resolution == image_size ~ "target"
      ),
      path = paste0(url, path)
    ) |>
    select(-resolution) |>
    pivot_wider(
      names_from = type,
      values_from = path
    )
}

build_series <- function(series) {

  ind <- which(galleries$series == series)

  dat <- paste0("series-", series) |>
    build_url() |>
    read_manifest(
      preview_size = galleries$preview_size[ind],
      image_size = galleries$image_size[ind],
      format = galleries$format[ind]
    ) |>
    add_image_html()
}

make_gallery <- function(series) {

  ind <- which(galleries$series == series)
  if(!is.na(galleries$manifest[ind])) {

    lines <- c(
      '---',
      paste0('title: "', galleries$name[ind], '"'),
      paste0('date: ', galleries$date[ind]),
      '---',
      '  ',
      '```{r}',
      '#| echo: false',
      '#| message: false',
      '#| layout-ncol: 4',
      '#| column: screen-inset',
      '#| results: asis',
      'source(here::here("_common.R"))',
      paste0('build_series("', galleries$series[ind], '")'),
      '```'
    )

    gallery_dir <- here::here("gallery", galleries$series[ind])
    if(!dir.exists(gallery_dir)) {
      dir.create(gallery_dir)
      brio::write_lines(lines, file.path(gallery_dir, "index.qmd"))
    }

  }
}

make_all_galleries <- function() {
  for(series in galleries$series) {
    make_gallery(series)
  }
}


