dat <- use_seed(1) %>%
  entity_heart(grain = 1000, size = 3) %>%
  unfold_warp(1,1, output = "id") %>%
  mutate(
    x = x + rnorm(n())/50,
    y = y + rnorm(n())/50
  ) %>%
  unfold_breeze(
    iterations = 100,
    scale = .001,
    drift = .03,
    fractal = ambient::ridged,
    octaves = 16
  ) %>%
  unfold_inside() %>%
  mutate(val = 1 + (inside>0)*ind)

dat %>%
  style_ribbon(
    palette = palette_adjust(
      name = "blood",
      prefix = "antiquewhite"
    ),
    colour = "val",
    alpha = c(.1,.01),
    background = "antiquewhite",
    type = "point",
    size = .4
  ) %>%
  export_image(
    "~/Desktop/sw6.png"
    #xlim = c(.1,.85),
    #ylim = c(.1,.85)
  )
