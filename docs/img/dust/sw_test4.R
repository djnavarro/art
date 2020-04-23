use_seed(1) %>%
  scene_grid(
    x = 1:2,
    y = 1:2,
    entity = "circle",
    grain = 1000
  ) %>%
  mutate(
    x = x + rnorm(n())/50,
    y = y + rnorm(n())/50,
    ind = 1:n()
  ) %>%
  unfold_breeze(
    iterations = 100,
    scale = .001,
    drift = .02,
    fractal = ambient::billow
  ) %>%
  mutate(val = ind + time) %>%
  style_ribbon(
    palette = palette_adjust(
      name = "vik",
      prefix = NULL,
      red.f = 1.05,
      blue.f = .9,
      green.f = .95
    ),
    colour = "val",
    alpha = c(.2,.03),
    background = "antiquewhite2",
    type = "point",
    size = .2
  ) %>%
  export_image(
    "~/Desktop/sw4.png"
#    xlim = c(.15,.85),
#    ylim = c(.15,.85)
  )
