# https://github.com/djnavarro/jasmines

library(jasmines)
library(dplyr)

simple_plot <- function(octaves) {
  
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

dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "rainbow",
      prefix = NULL,
      red.f = .7,
      blue.f = .7,
      green.f = .7
    ),
    colour = "val",
    alpha = c(.25,.02),
    background = "ghostwhite",
    type = "segment",
    size = .4
  ) %>%
  export_image(
    "~/Desktop/valkyrie.png"
  )
