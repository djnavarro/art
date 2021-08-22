flush_posts <- function() {
  posts <- list.files(here::here("content", "gallery"), full.names = TRUE)
  purrr::walk(posts, ~ file.remove(file.path(.x, "_index.md")))
}
build_outdated <- function() {
  outdated <- hugodown::site_outdated()
  purrr::walk(outdated, ~ rmarkdown::render(.x))
}

hugodown::hugo_stop()
flush_posts()
build_outdated()
hugodown::hugo_start()
