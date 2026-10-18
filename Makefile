
all: build render images

build: _galleries.csv
	Rscript _build.R

render: gallery
	quarto render .

# Per-image detail pages are generated directly into docs/ by string
# substitution rather than one .qmd-per-image Quarto render (see
# make_all_image_pages() in _common.R for why). Must run after `render`,
# since the main project render is what produces docs/site_libs etc.
images: render
	Rscript _build_images.R

clean:
	@rm -rf docs
