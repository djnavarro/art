# https://github.com/djnavarro/jasmines

library(jasmines)
library(dplyr)
iteration <- 2

simple_plot <- function(octaves, background, palette) {
  
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
  
  pic <- dat %>%
    style_ribbon(
      palette = palette_adjust(
        name = palette,
        prefix = NULL,
        red.f = x,
        blue.f = x,
        green.f = x
      ),
      colour = "val",
      alpha = c(.25,.02),
      background = background,
      type = "segment",
      size = .4
    ) 
  
  return(pic)
}

base <-
  simple_plot(1, "azure", "batlow") + 
  simple_plot(2, "antiquewhite3", "plasma") + 
  simple_plot(3, "grey20", "rainbow") +
  simple_plot(4, "ghostwhite", "grayC")
  
base %>% export_image(
  "~/Desktop/blah2.png"
)



