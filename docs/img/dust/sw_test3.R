use_seed(1) %>%
  entity_heart(grain = 20000) %>%
  unfold_breeze(
    iterations = 200,
    scale = .001,
    drift = .02,
    fractal = ambient::ridged
  ) %>%
  style_ribbon(
    palette = "grayC",
    alpha = c(.2,.03),
    background = "grey10"
  ) %>%
  export_image(
    "~/Desktop/sw3.png",
    xlim = c(.1,.85),
    ylim = c(.1,.85)
  )
