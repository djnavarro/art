library(jasmines)
library(dplyr)

dat <- use_seed(259) %>%
  scene_bubbles(
    n = 5, 
    grain = 5000
  ) %>%
  mutate(
    ind = 1:n(), 
    x = x + rnorm(n())/50,
    y = y + rnorm(n())/50
  ) %>%
  unfold_breeze(
    iterations = 100,
    scale = .001,
    drift = 0,
    fractal = ambient::billow,
    octaves = 16
  ) %>%
  unfold_inside() %>%
  mutate(val = 1 + (inside==0)*(ind + id*50))

dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "imola",
      prefix = "ghostwhite",
      red.f = .7,
      blue.f = .7,
      green.f = .7
    ),
    colour = "val",
    alpha = c(.25,.02),
    background = "ghostwhite",
    type = "point",
    size = .4
  ) %>%
  export_image(
    "~/Desktop/sw11.png",
    xlim = c(.1,.9),
    ylim = c(.1,.9)
  )
