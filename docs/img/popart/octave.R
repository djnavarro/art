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
  
  dat %>%
    style_ribbon(
      palette = palette_adjust(
        name = "magma",
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
      paste0("~/Desktop/oct", octaves, ".png")
    )
  
}

for(i in 1:4) simple_plot(i)

