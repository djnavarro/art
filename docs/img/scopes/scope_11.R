library(ambient) # may need dev version: https://github.com/thomasp85/ambient
library(viridis)
library(scico)

# specify the seed and grain
seed <- 272
grain <- 5000

# specify the circle
origin_x <- .35
origin_y <- .5
radius   <- .2
lmb <- 1

# parameters for the inner & outer textures
inner <- list(
  noise = gen_worley,
  frequency = 10,
  fractal = ridged,
  octaves = 10,
  value = "distance"
)
outer <- list(
  noise = gen_worley,
  frequency = 10,
  fractal = ridged,
  octaves = 4,
  value = "distance"
)

# adjuster
adjust_palette <- function(pal, r = 1, g = 1, b = 1, ...) {
  function(n, alpha = 1) {
    cols <- pal(n, ...)
    return(adjustcolor(cols, red.f = r, green.f = g, blue.f = b))
  }
}

# paletting function
#color_scheme <- adjust_palette(scico, g = .8, b = 1.1, r = 1.1, palette = "hawaii")
color_scheme <- jasmines::palette_named("grayC")

# filename 
fname <- paste0("~/Desktop/scope_11_", seed, ".png")


# generate the grid -------------------------------------------------------

# reset the RNG state
set.seed(seed)

# create a grid with coordinates
grid <- long_grid(
  x = seq(from = 0, to = 1,length.out = grain),
  y = seq(from = 0, to = 1,length.out = grain)
)

# distance from the circle
grid$distance <- sqrt(abs((grid$x - origin_x)^2 + (grid$y - origin_y)^2 - radius^2))
grid$sign <- sign((grid$x - origin_x)^2 + (grid$y - origin_y)^2 - radius^2)


# define textures ---------------------------------------------------------

# convenience functions
fracture_outer <- function(...) {
  fracture(
    x = grid$x * exp(-lmb * grid$distance^2) * (grid$sign > 0),
    y = grid$y * exp(-lmb * grid$distance^2) * (grid$sign > 0),
    seed = seed,
    ...
  )
}
fracture_inner <- function(...) {
  fracture(
    x = grid$x * exp(-lmb * grid$distance) * (grid$sign < 0),
    y = grid$y * exp(-lmb * grid$distance) * (grid$sign < 0),
    seed = seed,
    ...
  )
}

# generate textures
grid$outer <- do.call(fracture_outer, outer)
grid$inner <- do.call(fracture_inner, inner)


# add colors --------------------------------------------------------------

# palette index
grid$index <- round(
  1 + (grain - 1) * normalise(grid$inner + grid$outer)
)

# colors
cols <- color_scheme(n = grain)
grid$color <- cols[grid$index]


# create and print raster -------------------------------------------------

rast <- as.raster(grid, value = color)
png(filename = fname, width = grain, height = grain)
op <- par(mar = c(0,0,0,0))
plot(rast)
dev.off()
par(op)

