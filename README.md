# ggDiagnose

## Philosophy and Purpose

This package started with the idea that one should be able to quickly be able to create `ggplot` based graphics for diagnostics of basic models similar to that of the base `plot` functionality (like `plot.lm`, `plot.gam`, etc). But it's purpose quickly expanded into a tool for data scientists to quickly make pretty `ggplot` diagnostic graphics combined with providing students and data scientists the data frames to create modifications of the graphics and more. 

We hope that this package can help introductory `R` coding and data science in `R` classes would start with `ggplot2` based graphics and `tidyverse` based syntax (or at least also include presentation of `tidyverse` tools especially `mutate`, `group_by/summarize`, `pull/select`, `melt`, and `dcast`).

We note that the "creation of desired data frame" follows along the ideas of the `broom` package. If we can use the `broom` version we will, but otherwise we're create a new function for our analysis.

## Philosophical Package Setup

The user of this package is encouraged to think of the package as consisting of only 3 functions.

+ `ggDiagnose`
+ `dfCompile`
+ `ggVis`

Each function will check the provided object (`x`) and assess whether there is a function that can work with that object. 

Both `ggDiagnose` and `ggVis` can be thought of as `ggplot` versions of `plot` but `ggDiagnose` is for models and `ggVis` is for other objects. `dfCompile` creates the main data frame used to create the graphics for the given object.

## Installation:

```{r}
library(devtools)
devtools::install_github("benjaminleroy/ggDiagnose")
```

This package **requires** very few packages and will tell you when you don't have the necessary additional packages depending upon what objects you're dealing with.

## Thoughts on package development and request for contributions

We hope that this continues to grow and add value to more and more individuals by continuing to expand the number of objects it is able to assess. 

### Commentary

All `ggDiagnose` functions need an `x`,  `show_plot`, and `return` parameters. `x` is the object of interest, and the other 2 are logical values if the individual wishes to show the plot(s) and return the data frame that created the plot(s) and a list of plots / the plot object. 

All `dfCompile` functions need an `x` object, and should return a data frame that can help easily create most if not all of the graphic in `ggDiagnose` of `ggVis`.

### Imports vs Suggest:
Because this package will continue to grow and create diagnostics and visuals for more and more complicated objects we will only be requiring a limited number of objects and will be using the function `look_for_missing_packages` from `utilities.R` to see if additional packages are needed to download. See `ggDiagnose.lm`'s first couple of lines for recommended approach.  

## TODO:

overarching:

- [ ] 1. check what broom does for each object. Document when broom doesn't create what we need for the visualizations.

`ggDiagnose` (models):

- [x] 1. lm, glm (from `stats` package): `ggDiagnose.lm`
- [ ] 2. Gam (from the original `gam` package - not `mgcv` - or at least not first round) 
- [x] 3. glmnet (from `glmnet` packages): `ggDiagnose.glmnet`, `ggDiagnose.cv.glmnet`
    - [ ] plot.mrelnet, plot.multnet needed
- [ ] 4. trees
- [ ] 5. randomForest

`ggVis` (other objects):

- [ ] 1. sp
- [ ] 2. dendrogram
- [ ] 3. matrix (for heatmap?)

clean-up:

- [ ] 1. `ggDiagnose.lm`: try to make code cleaner in terms of creation of data (back away from direct replication)

teaching:

- [ ] 1. In examples for each function provide code to create some / all of the plots in a more basic manner with straight use of `tidyverse`

best coding practices:

- [ ] decide which parameters are passed to the visualization functions and how they differ / are the same of the `plot` implimentation.

## Examples

### `ggDiagnose.lm` (for an `lm` object.)

```{r}
lm_object <- lm(Sepal.Length ~., data = iris)
```


The original visualization:

```{r}
par(mfrow = c(2,3))
plot(lm_object, which = 1:6)
```

![](images/base_lm.jpeg)

The updated visualization:

```{r}
ggDiagnose(lm_object, which = 1:6)
```
![](images/ggDiagnose_lm.jpeg)

`ggDiagnose.lm` allows for similar parameter inputs as `plot.lm` but also includes additional ones. This may changes as the package evolves. 

### `ggDiagnose.glmnet`

```{r}
library(glmnet)
glmnet_object <- cv.glmnet(y = iris$Sepal.Length, 
                           x = model.matrix(Sepal.Length~., data = iris))
```

The original visualization:

```{r}
plot(glmnet_object)
```

![](images/base_glmnet.jpeg)

```{r}
ggDiagnose(glmnet_object)
```

![](images/ggDiagnose_glmnet.jpeg)

### `ggDiagnose.cv.glmnet`

```{r}
cv_glmnet_object <- cv.glmnet(y = iris$Sepal.Length, 
                              x = model.matrix(Sepal.Length~., data = iris))
```

The original visualization:

```{r}
plot(cv_glmnet_object)
```

![](images/base_cv_glmnet.jpeg)

```{r}
ggDiagnose(cv_glmnet_object)
```

![](images/ggDiagnose_cv_glmnet.jpeg)

## things to look into:

S3 methods? - should the whole thing be a S3 method?

write a blog post about putting a changing axis on top, link to the one where they transform the equation. Mention Hadley's thoughts on the matter.

## ideas for tests:

1. for all check the data frame coming out of `ggDiagnose` vs `dfCompile`, 
2. look at the names of the data frame returned by dfCompile
3. check return options for ggDiagnose


## needs for documentation:

pearson residuals (lm.R)

create a new file for package documentation.

see: http://r-pkgs.had.co.nz/man.html#man-packages

## readme needs:

probably should be showcasing `dfCompile` as well... how to do so in `.md`...
