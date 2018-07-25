# ggDiagnose

This package tries to fill the space where students get taught to examine diagnostics with visuals built in base `R` (the examples that come to mind are `plot.lm`, `plot.gam`, etc), and instead proposes `ggDiagnose` to do the same thing, allowing students to pull out graphics from the standard diagnostic plots to update / use on their own, which is easy to do with the grammar of graphics paradigm with `ggplot2 ` graphics.

The hope is that introductory `R` coding and data science in `R` classes would start with `ggplot2` based graphics and `tidyverse` based synatax (or at least also include presentation of `tidyverse` tools).

# Philosophy on the package

## Commentary

All functions need an `x`,  `show_plot`, and `return` parameters. `x` is the object of interest, and the other 2 are logical values if the individual wishes to show the plot(s) and return the data frame that created the plot(s) and a list of plots / the plot object.

## Imports vs Suggest:
Because this package will continue to grow and create diagnostics and visuals for more and more complicated objects we will only be requiring a limited number of objects and will be using the function `look_for_missing_packages` from `utilities.R` to see if additional packages are needed to download. See `ggDiagnose.lm`'s first couple of lines for recommended approach.  

# TODO:

`ggDiagnose`:

- [x] 1. lm, glm (from `stats` package): `ggDiagnose.lm`
- [ ] 2. gam 
- [x] 3. glmnet (from `glmnet` packages): `ggDiagnose.glmnet`, `ggDiagnose.cv.glmnet`
    - [ ] plot.mrelnet, plot.multnet needed
- [ ] 4. trees
- [ ] 5. randomForest

`ggVis`:
- [ ] 1. sp
- [ ] 2. dendrogram
- [ ] 3. matrix (for heatmap?)

clean-up:
- [ ] 1. `ggDiagnose.lm`: try to make code cleaner in terms of creation of data (back away from direct replication)

teaching:
- [ ] 1. In examples for each function provide code to create some / all of the plots in a more basic manner with straight use of `tidyverse`

*Not sure I should work on `ggVis.__` (just `ggplot2` version of plot) for objects that are not models. If the goal is to help starting data scientists / statisticians be able to do everything with `ggplot2` style graphics it probably isn't that helpful/ useful.*
