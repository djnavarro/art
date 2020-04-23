use_seed(1) %>%
  entity_circle(grain = 5000) %>%
  unfold_breeze(
    iterations = 100,
    scale = .001,
    drift = .01
  ) %>%
  style_ribbon(
    palette = "plasma",
    alpha = c(.2,.02),
    background = "antiquewhite"
  ) %>%
  export_image(
    "~/Desktop/sw2.png",
    xlim = c(.1,.85),
    ylim = c(.1,.85)
  )
