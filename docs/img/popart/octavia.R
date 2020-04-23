library(jasmines)
library(dplyr)
library(patchwork)

octaves <- 4

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

pic1 <- dat %>%
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
  ) 

pic2 <- dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "magma",
      prefix = NULL,
      red.f = x,
      blue.f = x,
      green.f = x
    ),
    colour = "val",
    alpha = c(.3,.01),
    background = "steelblue4",
    type = "segment",
    size = .4
  ) 

pic3 <- dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "bamako",
      prefix = NULL,
      red.f = x,
      blue.f = x,
      green.f = x
    ),
    colour = "val",
    alpha = c(.3,.01),
    background = "chocolate4",
    type = "segment",
    size = .4
  ) 



pic4 <- dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "vik",
      prefix = NULL,
      red.f = x,
      blue.f = x,
      green.f = x
    ),
    colour = "val",
    alpha = c(.3,.01),
    background = "darkgoldenrod",
    type = "segment",
    size = .4
  ) 



base <- pic1 + pic2 + pic3 + pic4

base %>% export_image("~/Desktop/octavia.png")

