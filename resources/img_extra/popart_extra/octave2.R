# https://github.com/djnavarro/jasmines

library(jasmines)
library(dplyr)
iteration <- 2

simple_plot <- function(octaves) {
  
  dat <- use_seed(259) %>%
    scene_delaunay(
      n = 30, 
      grain = 200
    ) %>%
    filter(id %in% sample(1:30, 10)) %>% 
    mutate(ind = 1:n()) %>%
    unfold_breeze(
      iterations = 200,
      scale = .001,
      drift = 0,
      fractal = ambient::billow,
      octaves = octaves
    )  %>% 
    mutate(val = ind + id * 200)
  
  x <- .9
  
  dat %>%
    style_ribbon(
      palette = palette_adjust(
        name = "batlow",
        prefix = NULL,
        red.f = x,
        blue.f = x,
        green.f = x
      ),
      colour = "val",
      alpha = c(.25,.02),
      background = "azure",
      type = "segment",
      size = .4
    ) %>%
    export_image(
      paste0("~/Desktop/oct_", iteration, "_", octaves, ".png")
    )
  
}

for(i in 1:4) simple_plot(i)

