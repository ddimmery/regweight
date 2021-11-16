
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regweight

<!-- badges: start -->

[![codecov](https://codecov.io/gh/ddimmery/regweight/branch/main/graph/badge.svg?token=OG7J5YE3JB)](https://codecov.io/gh/ddimmery/regweight)
[![R-CMD-check](https://github.com/ddimmery/regweight/actions/workflows/check-full.yaml/badge.svg)](https://github.com/ddimmery/regweight/actions/workflows/check-full.yaml)
[![lint](https://github.com/ddimmery/regweight/actions/workflows/lint.yaml/badge.svg)](https://github.com/ddimmery/regweight/actions/workflows/lint.yaml)
<!-- badges: end -->

The goal of regweight is to make it easy to diagnose a model using
[Aronow and Samii
(2015)](https://onlinelibrary.wiley.com/doi/abs/10.1111/ajps.12185)
regression weights.

## Installation

You can install the development version of regweight like so:

``` r
# install.packages("devtools")
devtools::install_github("ddimmery/regweight")
```

## Example

This is a basic example which shows you how to analyze the implicit
regression weights in a simple problem:

``` r
library(regweight)
data(penguins, package = "palmerpenguins")

model <- lm(body_mass_g ~ ., penguins)
summary(model)
#> 
#> Call:
#> lm(formula = body_mass_g ~ ., data = penguins)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -809.70 -180.87   -6.25  176.76  864.22 
#> 
#> Coefficients:
#>                    Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)       84087.945  41912.019   2.006  0.04566 *  
#> speciesChinstrap   -282.539     88.790  -3.182  0.00160 ** 
#> speciesGentoo       890.958    144.563   6.163 2.12e-09 ***
#> islandDream         -21.180     58.390  -0.363  0.71704    
#> islandTorgersen     -58.777     60.852  -0.966  0.33482    
#> bill_length_mm       18.964      7.112   2.667  0.00805 ** 
#> bill_depth_mm        60.798     20.002   3.040  0.00256 ** 
#> flipper_length_mm    18.504      3.128   5.915 8.46e-09 ***
#> sexmale             378.977     48.074   7.883 4.95e-14 ***
#> year                -42.785     20.949  -2.042  0.04194 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 286.5 on 323 degrees of freedom
#>   (11 observations deleted due to missingness)
#> Multiple R-squared:  0.8768, Adjusted R-squared:  0.8734 
#> F-statistic: 255.4 on 9 and 323 DF,  p-value: < 2.2e-16
```

Let’s say that we want to explore the effect of `flipper_length_mm` on
`body_mass_g`. Which units have high implict weight in estimating this
effect?

It’s very easy to use `regweight` to answer this question:

``` r
rw_model <- calculate_weights(model, "flipper_length_mm")
hist(rw_model)
#> Warning: Removed 11 rows containing non-finite values (stat_bin).
```

<img src="man/figures/README-regweight-1.png" width="100%" />

We can see how the distribution of weights over islands varies:

``` r
plot(rw_model, penguins$island)
```

<img src="man/figures/README-discrete-1.png" width="100%" />

We can similarly see the implicit distribution of `bill_length_mm` in
the nominal (unweighted) and implicit (regression weighted) sample:

``` r
plot(rw_model, penguins$bill_length_mm)
```

<img src="man/figures/README-cts-1.png" width="100%" />
