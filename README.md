# Commentary

All functions need an `x`,  `show_plot`, and `return` parameters. `x` is the object of interest, and the other 2 are logical values if the individual wishes to show the plot(s) and return the data frame that created the plot(s) and a list of plots / the plot object.

# Imports vs Suggest:
Because this package will continue to grow and create diagnostics and visuals for more and more complicated objects we will only be requiring a limited number of objects and will be using the function `look_for_missing_packages` from `utilities.R` to see if additional packages are needed to download. See `ggDiagnose.lm`'s first couple of lines for recommended approach.  

# TODO:

`ggDiagnose`:

- [x] 1. lm, glm (from `stats` package)
- [ ] 2. gam 
- [x] 3. glmnet (from `glmnet` packages)
    - [ ] plot.mrelnet, plot.multnet needed
- [ ] 4. trees
- [ ] 5. randomForest

`ggVis`:
- [ ] 1. sp
- [ ] 2. dendrogram
- [ ] 3. matrix (for heatmap?)

*Not sue I should work on `ggVis.__` (just `ggplot2` version of plot) for objects that are not models. If the goal is to help starting data scientists / statisticians be able to do everything with `ggplot2` style graphics it probably isn't that helpful/ useful.
