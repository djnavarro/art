---
output: hugodown::md_document
title: Home
summary: "Calade: A Hugo theme and R package by Danielle Navarro"
rmd_hash: 414012c32dba0a29

---

Calade
======

### A Hugo theme and R package by [Danielle Navarro](https://twitter.com/djnavarro)

<img src="header/caladown.png" width="150px" style="float:right; padding:15px;">

<br>

The goal of calade and caladown is to allow R users to create lightweight websites powered by Hugo and is designed to be compatible with both [hugodown](https://hugodown.r-lib.org/) and [blogdown](https://bookdown.org/yihui/blogdown/), and allows users to generate graphics that match the visual style of the site via the [thematic](https://rstudio.github.io/thematic/) package. There are two components to caladown, a [Hugo theme](https://github.com/djnavarro/hugo-calade) and an [R package](https://github.com/djnavarro/caladown). The simplest way to get started is simply to download the R package from GitHub using the following command:

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'><span class='k'>remotes</span>::<span class='nf'><a href='https://remotes.r-lib.org/reference/install_github.html'>install_github</a></span>(<span class='s'>"djnavarro/caladown"</span>)</code></pre>

</div>

Once the caladown package is installed, the tutorial in the [posts](/post/) section of this site will walk you through the rest of the process. For folks who already know what they are doing, here's the quick guide to setting up a Hugodown site on GitHub Pages:

<div class="highlight">

<pre class='chroma'><code class='language-r' data-lang='r'><span class='k'>hugodown</span>::<span class='nf'><a href='https://rdrr.io/pkg/hugodown/man/hugo_install.html'>hugo_install</a></span>(<span class='s'>"0.66.0"</span>) 
<span class='k'>caladown</span>::<span class='nf'><a href='https://rdrr.io/pkg/caladown/man/create_hugodown_calade.html'>create_hugodown_calade</a></span>(<span class='s'>"my-caladown-site"</span>)
<span class='k'>hugodown</span>::<span class='nf'><a href='https://rdrr.io/pkg/hugodown/man/hugo_build.html'>hugo_build</a></span>(dest = <span class='s'>"docs"</span>)
<span class='k'>usethis</span>::<span class='nf'><a href='https://usethis.r-lib.org/reference/use_git.html'>use_git</a></span>()
<span class='k'>usethis</span>::<span class='nf'><a href='https://usethis.r-lib.org/reference/use_github.html'>use_github</a></span>() <span class='c'># then enable GH Pages with /docs folder</span></code></pre>

</div>

Note that there is a known issue with knitting the R markdown files for some versions of pandoc: at present the earliest known version that works is pandoc 2.1

