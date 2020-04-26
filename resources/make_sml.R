library(magick)

img <- dir(
  path = here::here("img"), 
  pattern = "png$",
  recursive = TRUE
)

for(path in img) {
  input <- here::here("img", path)
  output <- here::here("sml", path)
  if(!file.exists(output)) {
    cat(input, "\n")
    im <- magick::image_read(input)
    im <- magick::image_resize(im, geometry = "400x")
    magick::image_write(im, output)
    Sys.sleep(1)
  }
}
