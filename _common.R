
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

#' Determine the seeded display order for a series' images
#'
#' Shared by the gallery grid (build_series()) and the per-image detail
#' pages (make_image_pages_fast()), so that "previous"/"next" on a
#' detail page always matches the order a visitor saw on the gallery
#' grid, rather than drifting to e.g. alphabetical file-name order.
#'
#' @param manifest Optionally pass an already-loaded manifest (as
#'   returned by arttools::manifest_read()) to avoid re-fetching it when
#'   the caller already has a copy.
series_image_order <- function(series, seed = NULL, manifest = NULL) {
  ind <- which(galleries$series == series)
  preview <- galleries$preview_dir[ind]
  target <- galleries$target_dir[ind]
  format <- galleries$format[ind]

  if (is.null(manifest)) manifest <- arttools::manifest_read(series)

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

  dat
}

build_series <- function(series, seed = NULL) {
  dat <- series_image_order(series, seed) |>
    dplyr::mutate(
      # link thumbnails to the per-image page rather than the raw file
      target = paste0(tools::file_path_sans_ext(file_name), "/")
    )

  add_image_html(dat)
}

#' Build the per-image detail pages for every series
#'
#' Per-image pages are plain HTML, generated directly into `docs/` by
#' string substitution rather than by rendering one `.qmd` per image
#' through Quarto/Pandoc. Routing ~4,500 near-identical pages through the
#' full Quarto pipeline is pure overhead: two pages differ only in a
#' handful of values (title, image url, prev/next links), while the
#' surrounding nav/head/footer chrome is byte-identical. Instead, one
#' small scaffold page per series *is* rendered through Quarto proper (so
#' chrome, theme, and date/license formatting stay correct and in sync
#' with the live site), and its output is used as a template into which
#' the per-image values are substituted.
#'
#' A sentinel-token scheme (`{{TOKEN}}`) is used rather than real-looking
#' placeholder values, because it's robust to coincidental matches and
#' because Pandoc otherwise mangles things like `@token` (citation
#' syntax) or treats a token used as a markdown image destination as a
#' literal (non-existent) relative file and rewrites/percent-encodes it.
#' Raw HTML and an absolute-looking placeholder URL sidestep both issues.
#'
#' @param force If TRUE, regenerate pages that already exist. Since this
#'   whole pipeline runs in seconds even for the full image corpus, the
#'   normal case is to always regenerate (force = TRUE) rather than try
#'   to detect what's stale.
make_all_image_pages <- function(force = TRUE) {

  included <- galleries$series[galleries$include]
  templates <- build_image_page_templates(included)

  for (series in included) {
    ind <- which(galleries$series == series)
    make_image_pages_fast(
      series,
      templates[[series]],
      seed = galleries$seed[ind],
      force = force
    )
  }

  invisible(NULL)
}

#' Render one scaffold page per series through Quarto, to capture that
#' series' chrome (nav/theme/head) plus its real date/license/repo
#' metadata (which Quarto formats correctly), leaving title/image/prev/
#' next as `{{TOKEN}}` placeholders for later substitution.
#'
#' Returns a named list of HTML strings, one per series. The scaffold
#' source and output are deleted before returning, so none of it is
#' left behind for `quarto render .` to pick up as real site content.
build_image_page_templates <- function(series_vec) {

  # NOTE: must not be underscore-prefixed. Quarto treats underscore-led
  # directories as excluded from the *project itself* (not just from the
  # default render list), so such a path would lose the project's
  # theme/nav/output-dir and render as a bare standalone document.
  scaffold_root <- here::here("gallery", "build-scaffold-image-template")
  if (fs::dir_exists(scaffold_root)) fs::dir_delete(scaffold_root)
  fs::dir_create(scaffold_root)
  withr::defer(if (fs::dir_exists(scaffold_root)) fs::dir_delete(scaffold_root))

  docs_scaffold_root <- here::here("docs", "gallery", "build-scaffold-image-template")
  withr::defer(if (fs::dir_exists(docs_scaffold_root)) fs::dir_delete(docs_scaffold_root))

  for (series in series_vec) {
    ind <- which(galleries$series == series)
    repospec <- gsub("https://github.com/", "", galleries$repo[ind])
    page_dir <- fs::path(scaffold_root, series)
    fs::dir_create(page_dir)

    lines <- c(
      '---',
      'title: "{{TITLE}}"',
      paste0('date: ', galleries$date[ind]),
      paste0('repo: ', repospec),
      paste0('license-type: "', galleries$license[ind], '"'),
      'appendix-style: none',
      'page-layout: full',
      'image: "https://PLACEHOLDER.invalid/{{IMAGE_URL}}"',
      'image-alt: "{{TITLE}}"',
      '---',
      '   ',
      '<figure class="figure">',
      '<img src="{{IMAGE_URL}}" class="img-fluid figure-img" alt="{{TITLE}}">',
      '<figcaption>{{TITLE}}</figcaption>',
      '</figure>',
      '   ',
      ':::: {.grid}',
      '',
      ':::{.g-col-4}',
      '{{PREV_LINK}}',
      ':::',
      '',
      ':::{.g-col-4 .text-center}',
      '[back to gallery](../)',
      ':::',
      '',
      ':::{.g-col-4 .text-end}',
      '{{NEXT_LINK}}',
      ':::',
      '',
      '::::',
      '   '
    )
    brio::write_lines(lines, fs::path(page_dir, "index.qmd"))
  }

  # Rendering within the project updates docs/sitemap.xml and
  # docs/search.json to include whatever was just rendered, including
  # the scaffold pages. Snapshot and restore them so the (deleted)
  # scaffold never leaves stale entries behind.
  sitemap_path <- here::here("docs", "sitemap.xml")
  search_path <- here::here("docs", "search.json")
  sitemap_before <- if (fs::file_exists(sitemap_path)) readBin(sitemap_path, "raw", fs::file_size(sitemap_path)) else NULL
  search_before <- if (fs::file_exists(search_path)) readBin(search_path, "raw", fs::file_size(search_path)) else NULL
  withr::defer({
    if (!is.null(sitemap_before)) writeBin(sitemap_before, sitemap_path)
    if (!is.null(search_before)) writeBin(search_before, search_path)
  })

  quarto::quarto_render(input = scaffold_root, quiet = TRUE, as_job = FALSE)

  templates <- lapply(series_vec, function(series) {
    html_path <- fs::path(docs_scaffold_root, series, "index.html")
    paste(readLines(html_path, warn = FALSE), collapse = "\n")
  })
  names(templates) <- series_vec

  templates
}

#' Generate the per-image HTML pages for one series by substituting into
#' a pre-rendered template (see build_image_page_templates()). This
#' performs no Quarto/Pandoc render of its own; it's pure string
#' substitution, which is what makes it fast at ~4,500-image scale.
#'
#' @param seed Passed to series_image_order() so prev/next links follow
#'   the same shuffled order shown on the series' gallery grid page.
make_image_pages_fast <- function(series, template, seed = NULL, force = TRUE) {

  ind <- which(galleries$series == series)
  if (!galleries$include[ind]) return(invisible(NULL))

  target <- galleries$target_dir[ind]
  format <- galleries$format[ind]
  series_short_name <- gsub("^series-", "", galleries$series[ind])

  manifest <- arttools::manifest_read(series)

  meta <- manifest |>
    dplyr::filter(folder == target, file_format == format) |>
    dplyr::mutate(
      slug = tools::file_path_sans_ext(file_name),
      full_url = arttools::bucket_remote_url(series, path),
      title = dplyr::if_else(
        !is.na(image_short_title),
        image_short_title,
        paste(galleries$name[ind], paste(system_version, image_id, sep = "-"))
      )
    ) |>
    dplyr::select(file_name, slug, full_url, title)

  # same seeded shuffle used for the gallery grid (build_series()), so
  # prev/next here matches the order a visitor saw there. Any file
  # present in the target folder but missing a matching preview (so it
  # wouldn't appear in the shuffle) is still given a page, just appended
  # after the shuffled ones, so nothing silently goes missing.
  shuffled_names <- series_image_order(series, seed, manifest = manifest)$file_name
  extra_names <- setdiff(sort(meta$file_name), shuffled_names)
  display_order <- c(shuffled_names, extra_names)

  dat <- meta[match(display_order, meta$file_name), ]
  unmatched <- sum(is.na(dat$file_name))
  if (unmatched > 0) {
    # Seen e.g. for series-tangle, whose declared target_dir doesn't
    # match any folder in its manifest -- a pre-existing data issue, not
    # something to paper over silently.
    cli::cli_alert_warning(paste(
      series, "- preview/target manifest mismatch:", unmatched,
      "shuffled image(s) have no matching target-folder file and will",
      "not get a page"
    ))
    dat <- dat[!is.na(dat$file_name), ]
  }

  n <- nrow(dat)
  out_root <- here::here("docs", "gallery", series_short_name)

  for (i in seq_len(n)) {

    row <- dat[i, ]
    out_dir <- fs::path(out_root, row$slug)
    out_file <- fs::path(out_dir, "index.html")
    if (!force && fs::file_exists(out_file)) next

    prev_link <- if (i > 1) {
      paste0('<p><a href="../', dat$slug[i - 1], '/">&larr; previous</a></p>')
    } else {
      ''
    }
    next_link <- if (i < n) {
      paste0('<p><a href="../', dat$slug[i + 1], '/">next &rarr;</a></p>')
    } else {
      ''
    }

    page <- template |>
      # longer, more specific token first: see note in
      # build_image_page_templates() about the placeholder image url
      stringr::str_replace_all(
        stringr::fixed('https://PLACEHOLDER.invalid/{{IMAGE_URL}}'),
        row$full_url
      ) |>
      stringr::str_replace_all(stringr::fixed('{{IMAGE_URL}}'), row$full_url) |>
      stringr::str_replace_all(stringr::fixed('{{TITLE}}'), row$title) |>
      stringr::str_replace_all(stringr::fixed('<p>{{PREV_LINK}}</p>'), prev_link) |>
      stringr::str_replace_all(stringr::fixed('<p>{{NEXT_LINK}}</p>'), next_link)

    fs::dir_create(out_dir)
    writeLines(page, out_file)
  }

  cli::cli_alert_success(paste("image pages generated:", series, "(", n, "pages )"))
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


