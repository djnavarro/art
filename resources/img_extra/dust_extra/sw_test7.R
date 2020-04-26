dat <- use_seed(1) %>%
  entity_heart(grain = 3000, size = 3) %>%
  unfold_slice(3,1, output1 = "id") %>%
  mutate(
    x = x + rnorm(n())/50,
    y = y + rnorm(n())/50
  ) %>%
  unfold_breeze(
    iterations = 200,
    scale = .0008,
    drift = .03,
    fractal = ambient::billow,
    octaves = 16
  ) %>%
  unfold_inside() %>%
  mutate(val = 1 + (inside>0)*ind)

dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "rainbow",
      prefix = "grey10",
      red.f = .7,
      blue.f = .7,
      green.f = .7
    ),
    colour = "val",
    alpha = c(.2,.02),
    background = "antiquewhite2",
    type = "point",
    size = .2
  ) %>%
  export_image(
    "~/Desktop/sw7.png"
    #xlim = c(.1,.85),
    #ylim = c(.1,.85)
  )
