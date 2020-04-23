# https://github.com/djnavarro/jasmines

library(jasmines)
library(dplyr)
library(ggplot2)
library(patchwork)

simple_plot <- function(octaves, background = "ghostwhite") {
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
    mutate(
      val = ind + id * 50,
      octaves = octaves
    )
  
  pic <- dat %>% style_ribbon(
    palette = palette_adjust(
      name = "magma",
      prefix = NULL,
      red.f = .7,
      blue.f = .7,
      green.f = .7
    ),
    colour = "val",
    alpha = c(.25,.02),
    background = background,
    type = "segment",
    size = .4
  ) 
  
  return(pic)
}

base <- simple_plot(1, "#00000322") + 
  simple_plot(2, "#50165A22") + 
  simple_plot(3, "#A9434122") + 
  simple_plot(4, "#B0B18622")

base %>% export_image(
  "~/Desktop/blah.png"
)
