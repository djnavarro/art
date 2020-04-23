# https://github.com/djnavarro/jasmines

library(jasmines)
library(dplyr)
library(patchwork)

simple_plot <- function(octaves) {
  
  cat(",")
  
  dat <- use_seed(259) %>%
    scene_rows(
      n = 5, 
      grain = 1000, 
      vertical = TRUE
    ) %>%
    mutate(ind = 1:n()) %>%
    unfold_breeze(
      iterations = 200,
      scale = .0005,
      drift = 0,
      fractal = ambient::billow,
      octaves = octaves
    )  %>% 
    mutate(val = ind + id * 50)
}

dat <- purrr::map_dfr(1:4, simple_plot)

valkyrie <- function(background = "ghostwhite", palette = "rainbow", adjust = .7) {
  cat(".")
  dat %>% style_ribbon(
    palette = palette_adjust(
      name = palette,
      prefix = NULL,
      red.f = adjust,
      blue.f = adjust,
      green.f = adjust
    ),
    colour = "val",
    alpha = c(.25,.02),
    background = background,
    type = "segment",
    size = .4
  ) 
}

base <-
  valkyrie("yellow", "plasma", 1.5) +
  valkyrie("pink", "rainbow", 1.3) +
  valkyrie("orange", "grayC", 1.5) +
  valkyrie("deepskyblue", "viridis", 1.5)



base %>% export_image(
  "~/Desktop/valkyrie_pop.png"
)
