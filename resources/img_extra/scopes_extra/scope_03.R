library(ambient)

s <- 1
grain <- 5000
regrid <- FALSE

radians <- function(grain, closed = TRUE) {
  if(closed) return(seq(0, 2*pi, length.out = grain))
  return(seq(0, 2*pi, length.out = grain + 1)[1:grain])
}
inside <- function(x, y, polygon) {
  inside <- sp::point.in.polygon(x, y, polygon$x, polygon$y)
  return(inside == 0)
}
circle <- function(grain, x, y, d) {
  tibble::tibble(
    th = radians(grain),
    x = x + d * cos(th)/2,
    y = y + d * sin(th)/2
  )
}

if(regrid) {
  grid <- long_grid(
    x = seq(from = 0, to = 1,length.out = grain),
    y = seq(from = 0, to = 1,length.out = grain)
  )
  loop <- circle(grain/10, x = .25, y = .5, d = .5)
  grid$inside <- inside(grid$x, grid$y, loop)
}


grid$outer <- fracture(
  noise = gen_worley,
  fractal = billow,
  x = grid$x * grid$inside,
  y = grid$y * grid$inside,
  frequency = 5,
  value = 'cell',
  octaves = 10
)
grid$inner <- fracture(
  noise = gen_worley,
  x = grid$x * (1 - grid$inside),
  y = grid$y * (1 - grid$inside),
  frequency = 20,
  fractal = ridged,
  value = 'cell',
  octaves = 10
)

grid$inner <- normalise(grid$inner)
grid$outer <- normalise(grid$outer)
grid$inner <- 1 - grid$inner

grid$index <- round(1 + (grain - 1) * normalise(grid$inner + grid$outer))
palette <- viridis::viridis(n = grain)
grid$color <- palette[grid$index]

rast <- as.raster(grid, value = color)

png(
  filename = "~/Desktop/scope_03.png",
  width = grain,
  height = grain,
)
op <- par(mar = c(0,0,0,0))
plot(rast)
dev.off()
par(op)
