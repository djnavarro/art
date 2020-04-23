library(jasmines)
library(dplyr)

dat <- use_seed(1) %>%
  scene_delaunay(n = 10, grain = 100) %>%
  unfold_breeze(
    iterations = 200,
    scale = .0008,
    drift = .03,
    fractal = ambient::billow,
    octaves = 16
  ) %>%
  unfold_inside() %>%
  mutate(val = 1 + (inside>0)*id)

dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "base",
      prefix = "grey70",
      red.f = .7,
      blue.f = .7,
      green.f = .7
    ),
    colour = "val",
    alpha = c(.4,.02),
    background = "azure",
    type = "point",
    size = .2
  ) %>%
  export_image(
    "~/Desktop/sw9.png",
    xlim = c(.12,.82),
    ylim = c(.12,.82)
  )
