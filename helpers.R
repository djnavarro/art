make_gallery <- function(id, name, dir, text) {
  
  images <- list.files(here::here("img", dir), pattern = "png$")
  links <- paste0('<a href = "img/', dir, "/" ,images, '"><img src = "sml/', dir, "/", images, '"></a>')
  
  paste(
    '<!-- Start gallery Area -->',
    paste0('<section class="gallery-area section-gap" id="', id, '">'),
    '<div class="container">',
    '<div class="row justify-content-center">',
    '<div class="col-md-8 pb-30 header-text">',
    paste0('<h1 class="text-white">', name, '</h1>'),
    '<p>',
    text,
    '</p>',
    '</div>',
    '</div>',
    '<div class="gal">',
    paste(links, collapse = "\n"),
    '</div>',
    '</div>',
    '</section>',
    sep = "\n"
  )
}


make_menu <- function() {
  
  paste(
    '    <!-- Start Header Area -->',
    '      <header class="default-header">',
    '      <div class="container">',
    '      <div class="header-wrap">',
    '      <div class="header-top d-flex justify-content-between align-items-center">',
    '      <div class="logo">',
    '      <a href="#home"><img src="img/logo.png" alt=""></a>',
    '      </div>',
    '      <div class="main-menubar d-flex align-items-center">',
    '      <nav class="hide">',
    '      <a href="#home">Home</a>',
    '      <a href="#voronoise">Voronoise</a>',
    '      <a href="#flametree">Flametree</a>',
    '      <a href="#scrawl">Scrawl</a>',
    '      <a href="#scope">Scope</a>',
    '      <a href="#popart">Pop Art</a>',
    '      <a href="#dust">Dust</a>',
    '      <a href="#nye2019">NYE 2019</a>',
    '      <a href="#rosemary">Rosemary</a>',
    '      </nav>',
    '      <div class="menu-bar"><span class="lnr lnr-menu"></span></div>',
    '      </div>',
    '      </div>',
    '      </div>',
    '      </div>',
    '      </header>',
    '      <!-- End Header Area -->',
    sep = "\n"
  )
  
}



make_carousel <- function() {
  
  paste(
    
    '    <!-- start banner Area -->',
    '      <section class="banner-area relative" id="home">',
    '      <div class="slider"><div id="carouselExampleIndicators" class="carousel slide" data-ride="carousel">',
    '      ',
    '      <div class="carousel-inner" role="listbox">',
    '      <div class="carousel-item active" style="background-image: url(\'img/scopes/scope_03.png\')"></div>',
    '      <div class="carousel-item" style="background-image: url(\'img/flametree/007_ft_298d.png\')"></div>',
    '      <div class="carousel-item" style="background-image: url(\'img/flametree/001_flametree_20_13.png\')"></div>',
    '      </div>',
    '      ',
    '      <div class="carousel-caption d-md-block">',
    '      <p style="font-size:150%">art by</p>',
    '      <h2 style="font-size:200%" class="text-uppercase">Danielle Navarro</h2>',
    '      </div>',
    '      ',
    '      <a class="carousel-control-prev" href="#carouselExampleIndicators" role="button" data-slide="prev">',
    '      <span class="carousel-control-prev-icon" aria-hidden="true"></span>',
    '      <span class="sr-only">Previous</span>',
    '      </a>',
    '      ',
    '      <a class="carousel-control-next" href="#carouselExampleIndicators" role="button" data-slide="next">',
    '      <span class="carousel-control-next-icon" aria-hidden="true"></span>',
    '      <span class="sr-only">Next</span>',
    '      </a>',
    '      </div></div>',
    '      </section>',
    '      <!-- End banner Area -->',
    sep = "\n"
  )
}


make_profile <- function() {
  paste(
    
    '    <!-- Start About Area -->',
    '      <section class="gallery-area section-gap" id="about">',
    '      <div class="container">',
    '      <div class="row d-flex align-items-center">',
    '      <div class="col-lg-3 about-left">',
    '      <img class="img-fluid" src="img/about-img-smol.jpg" alt="">',
    '      </div>',
    '      <div class="col-lg-9 about-right">',
    '      <p style="color: #666666">',
    '      <span aria-hidden="true">Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin rutrum molestie est sed lacinia. Curabitur quis sodales augue. Integer ultrices nunc tortor, non lacinia arcu fermentum faucibus. Cras odio dolor, pellentesque at </span><span class="text-white">generative artwork created with R</span><span aria-hidden="true"> vehicula est. Nulla in finibus est. In hac habitasse platea dictumst. Vivamus nec sem sit amet ante efficitur elementum a vitae sapien. Integer sed ante tincidunt, rutrum enim at, imperdiet turpis. In imperdiet faucibus neque a tincidunt. Sed mollis consequat lorem. Phasellus consectetur, nisl in sagittis vehicula, est nisl eleifend </span> <a href="https://djnavarro.net"><span class="text-white">https://djnavarro.net</span></a> <span aria-hidden="true">arcu, eget pellentesque nunc sem non mi. Quisque quis scelerisque dolor. Donec scelerisque purus quis ex condimentum blandit. Vivamus non lorem eros. Aliquam ac dui at ante vehicula cursus nec et lectus. Integer et </span> <a href="https://twitter.com/djnavarro"><span class="text-white">https://twitter.com/djnavarro</span></a> <span aria-hidden="true">mauris posuere, faucibus orci vel, facilisis urna. Vivamus vitae porta </span> <a href="https://github.com/djnavarro"><span class="text-white">https://github.com/djnavarro</span></a> <span aria-hidden="true"> lorem, eleifend imperdiet risus. Aliquam sit amet justo id turpis euismod ultrices. Duis id dui velit. Nullam non purus ut tortor blandit faucibus sit amet non lacus. Sed commodo pulvinar ipsum in tristique. Maecenas finibus, enim vel sagittis faucibus, lectus lectus varius neque, eu cursus felis diam in quam.</span>', 
    '    </p>',
    '      </div>',
    '      </div>',
    '      </div>	',
    '      </section>',
    '      <!-- End About Area -->',
    sep = "\n"
    
  )
}


make_footer <- function() {
  
  paste(
    '<!-- start footer Area -->',
    '<footer class="footer-area">',
    '<div class="container">',
    '<div class="row footer-bottom d-flex justify-content-between">',
    '<p class="col-lg-8 col-sm-12 footer-text m-0 text-white">artwork by <a href="https://djnavarro.net">danielle navarro</a>, template by <a href="https://colorlib.com/wp/themes/philosophy/">colorlib</a></p>',
    '<div class="col-lg-4 col-sm-12 footer-social">',
    '<a href="https://art.djnavarro.net"><i class="fa fa-home"></i></a>',
    '<a href="https://twitter.com/djnavarro"><i class="fa fa-twitter"></i></a>',
    '<a href="mailto:d.navarro@unsw.edu.au"><i class="fa fa-paper-plane"></i></a>',
    '<a href="https://github.com/djnavarro"><i class="fa fa-github"></i></a>',
    '</div>',
    '</div>',
    '</div>',
    '</footer>',
    '<!-- End footer Area -->',
    sep = "\n"
  )
}


make_scripts <- function() {
  
  paste0(
    '<script src="js/vendor/jquery-2.2.4.min.js"></script>',
    '<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.11.0/umd/popper.min.js" integrity="sha384-b/U6ypiBEHpOf/4+1nzFpr53nxSS+GLCkfwBdFNTxtclqqenISfwAzpKaMNFNmj4" crossorigin="anonymous"></script>',
    '<script src="js/vendor/bootstrap.min.js"></script>',
    '<script src="js/jquery.ajaxchimp.min.js"></script>',
    '<script src="js/owl.carousel.min.js"></script>',
    '<script src="js/jquery.nice-select.min.js"></script>',
    '<script src="js/jquery.sticky.js"></script>',
    '<script src="js/parallax.min.js"></script>',
    '<script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"></script>',
    '<script type="text/javascript" src="js/simple-lightbox.min.js"></script>',
    '<script src="js/main.js"></script>',
    sep = "\n"
  )  
}
