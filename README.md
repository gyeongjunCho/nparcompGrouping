# nparcompGrouping

Summarize on nparcomp results by grouping.

Only supported when there is one independent variable.

### Install

```r
devtools::install_github("Gyeongjuncho/nparcompGrouping")
```


### Usage

Example data.frame is `iris`

Independent variable : `Species`

Dependent variable : `Petal.Width`

```r
library(nparcomp)
library(nparcompGrouping)

Tukey <- nparcomp(formula = Petal.Width ~ Species, data = iris, type="Tukey")

Tukey

nparcompGrouping(Tukey)
```
