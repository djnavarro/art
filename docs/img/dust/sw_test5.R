use_seed(1) %>%
  entity_circle(grain = 5000) %>%
  unfold_warp(2,1, output = "id") %>%
  unfold_breeze(
    iterations = 100,
    scale = .001,
    drift = .03,
    fractal = ambient::ridged,
    octaves = 16
  ) %>%
  style_ribbon(
    palette = "magma",
    alpha = c(.1,.01),
    background = "antiquewhite",
    type = "point",
    size = .4
  ) %>%
  style_overlay(fill = "#faebd755") %>%
  export_image(
    "~/Desktop/sw5.png"
    #xlim = c(.1,.85),
    #ylim = c(.1,.85)
  )
