
all: build render

build: _galleries.csv
	Rscript _build.R

render: gallery
	quarto render .

clean:
	@rm -rf docs
