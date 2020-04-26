# https://github.com/djnavarro/jasmines

library(jasmines)
library(dplyr)
iteration <- 4

simple_plot <- function(octaves) {
  
  dat <- use_seed(259) %>%
    scene_rows(n = 5, grain = 1000) %>%
    mutate(x = x*5, y = y*5, ind = 1:n()) %>% 
    unfold_breeze(
      iterations = 200,
      scale = .001,
      drift = .1,
      fractal = ambient::ridged,
      gain = ambient::spectral_gain(),
      octaves = octaves
    )  %>% 
    mutate(val = ind + id * 200)
  
  x <- .7
  
  dat %>%
    style_ribbon(
      palette = palette_adjust(
        name = "rainbow",
        prefix = NULL,
        red.f = x,
        blue.f = x,
        green.f = x
      ),
      colour = "val",
      alpha = c(.3,.01),
      background = "black",
      type = "segment",
      size = .4
    ) %>%
    export_image(
      paste0("~/Desktop/oct_", iteration, "_", octaves, ".png")
    )
  
}

for(i in 1:4) simple_plot(i)

