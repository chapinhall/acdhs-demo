# Background 

The code in this repository builds a dashboard--downloadable at [this link](https://github.com/chapinhall/acdhs-demo/raw/master/acdhs-dashboard-demo.html)--demonstrating for the Allegheny County Department of Human Service's RFP on unstructured data analytics. These materials are offered under the permissive [BSD-3 software license](https://opensource.org/licenses/BSD-3-Clause).

# Tools

The data work, mapping, visualization creation, and dashboard generation is entirely built using the free and open source [R programming language](https://www.r-project.org/about.html). The R packages we use here and in related work include:

* [data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html) for high subsetting, merging and handling of big data
* [dplyr](https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html) for readable syntax in manipulating non-"big" data
* [ggplot2](https://www.statmethods.net/advgraphs/ggplot2.html) for generating attractive static plots
* [plotly](https://plot.ly/r/) for creating attractive, dynamic visualizations in interactive documents
* [leaflet](https://rstudio.github.io/leaflet/) for generating dynamic maps that permit panning, zooming, and popup annotations
* [rmarkdown](https://rmarkdown.rstudio.com/), [knitr](https://yihui.name/knitr/), and [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/) to automate generation of dynamic, attractive, data-driven documents
* [reticulate](https://rstudio.github.io/reticulate/articles/introduction.html) which provides an R interface to Python modules, classes and functions

All of these packages are free, have broad user bases and community support, and are continually developed for enhanced features.

# Contributing

If you are interested in reusing or contributing to this repository, you will need to know how to use GitHub and the git version control system. For free and interactive ways to learn, we recommend the [GitHub learning lab](https://lab.github.com/), and [DataCamp's course on git for data science](https://www.datacamp.com/courses/introduction-to-git-for-data-science).

