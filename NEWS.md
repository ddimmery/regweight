# regweight 0.0.1

* Added a `NEWS.md` file to track changes to the package.

* Also added everything else for the initial commit of the package.

* Added histogram of weights accessible with `hist`.

* Added a choropleth plot if the covariate passed to `plot` is of type `sf::sfc`. Due to complications of installing `sf` on Mac OS X, I've used soft requirements to keep this dependency as a "Suggest".