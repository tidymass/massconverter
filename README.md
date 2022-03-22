<!-- README.md is generated from README.Rmd. Please edit that file -->

# massconverter <img src="man/figures/massconverter_logo.png" align="right" alt="" width="120" />

[![](https://www.r-pkg.org/badges/version/massconverter?color=green)](https://cran.r-project.org/package=massconverter)
[![](https://img.shields.io/github/languages/code-size/tidymass/massconverter.svg)](https://github.com/tidymass/massconverter)
[![Dependencies](https://tinyverse.netlify.com/badge/massconverter)](https://cran.r-project.org/package=massconverter)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`massconverter` is a part of [tidymass](https://www.tidymass.org/).


# About

`massconverter` can be used to convert mass spectrometry data to other format data based on `msconvert` docker image. 

More information can be found [here](https://hub.docker.com/r/chambm/pwiz-skyline-i-agree-to-the-vendor-licenses)

<img src="man/figures/Screen Shot 2022-01-08 at 6.12.58 PM.png" align="middle" alt="" width = "100%"/>

# Installation

You can install `massconverter` from [GitLab](https://gitlab.com/jaspershen/massconverter)

``` r
if(!require(remotes)){
install.packages("remotes")
}
remotes::install_gitlab("jaspershen/massconverter")
```

or [GitHub](https://github.com/tidymass/massconverter)

``` r
remotes::install_github("tidymass/massconverter")
```

More installation information can be found [here](https://massconverter.tidymass.org/articles/install.html).

# Get started

Please see the "Help documents".

# Need help?

If you have any questions about `massconverter`, please don’t hesitate to
email me (<shenxt@stanford.edu>) or reach out me via the social medias below.

<i class="fa fa-weixin"></i>
[shenxt1990](https://www.shenxt.info/files/wechat_QR.jpg)

<i class="fa fa-envelope"></i> <shenxt@stanford.edu>

<i class="fa fa-twitter"></i>
[Twitter](https://twitter.com/JasperShen1990)

<i class="fa fa-map-marker-alt"></i> [M339, Alway Buidling, Cooper Lane,
Palo Alto, CA
94304](https://www.google.com/maps/place/Alway+Building/@37.4322345,-122.1770883,17z/data=!3m1!4b1!4m5!3m4!1s0x808fa4d335c3be37:0x9057931f3b312c29!8m2!3d37.4322345!4d-122.1748996)

# Citation

If you use `massconverter` in your publications, please cite this paper:

TidyMass: An Object-oriented Reproducible Analysis Framework for LC-MS Data.

Xiaotao Shen, Hong Yan, Chuchu Wang, Peng Gao, Caroline H. Johnson, Michael P. Snyder.

[Web Link](https://www.biorxiv.org/content/10.1101/2022.03.15.484499v1).

Thanks very much!
