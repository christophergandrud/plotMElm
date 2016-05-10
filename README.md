plotMElm
======================

A simple R package to plot marginal effects from interactions estimated
from linear models.

[![Build Status](https://travis-ci.org/christophergandrud/plotMElm.svg?branch=master)](https://travis-ci.org/christophergandrud/plotMElm)

# Examples

## Continuous Term 2

The package contains one simply function: `plot_me` for plotting marginal
effects from interactions estimated from models estimated with the
`lm` function in base R. For example, when the second term is continuous:


```r
# Load package
library(plotMElm)

# Estimate model
states <- as.data.frame(state.x77)
m1 <- lm(Murder ~ Income * Population, data = states)

# Plot marginal effect of Income across the observed range of Population
plot_me(m1, 'Income', 'Population')
```

![plot of chunk murder-me-example](figure/murder-me-example-1.png)

## Categorical Term 2 

When the second term in the interaction is categorical (detected by having 5 or fewer categories) then point-ranges are plotted:


```r
# Estimate model
m2 <- lm(mpg ~ wt * cyl, data = mtcars)

# Plot marginal effect of Weight across the Number of Cylinders
plot_me(m2, 'wt', 'cyl')
```

![plot of chunk cars-example](figure/cars-example-1.png)

## See also 

The [interplot](https://cran.r-project.org/package=interplot) package also has many of the same capabilities as *plotMElm*.

