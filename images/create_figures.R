library(ggDiagnose)


## ggDiagnose.lm
lm.object <- lm(Sepal.Length ~., data = iris)

jpeg(filename = paste0("base_lm.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
par(mfrow = c(2,3))
plot(lm.object, which = 1:6)
dev.off()

jpeg(filename = paste0("ggDiagnose_lm.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(lm.object, which = 1:6)
dev.off()

# "1234567890123456789012345678901234567890123456789012345678901234567890123456"
# dfCompile(lm.object) %>% head(2)
# dfCompile(lm.object) %>% names

## ggDiagnose.glmnet

library(glmnet)

glmnet.object <- glmnet(y = iris$Sepal.Length,
               x = model.matrix(Sepal.Length~., data = iris))

jpeg(filename = paste0("base_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
plot(glmnet.object)
dev.off()

jpeg(filename = paste0("ggDiagnose_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(glmnet.object)
dev.off()

# dfCompile(glmnet.object) %>% head(2)
# dfCompile(glmnet.object) %>% names


## ggDiagnose.cv.glmnet

cv.glmnet.object <- cv.glmnet(y = iris$Sepal.Length,
                              x = model.matrix(Sepal.Length~., data = iris))

jpeg(filename = paste0("base_cv_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
plot(cv.glmnet.object)
dev.off()

jpeg(filename = paste0("ggDiagnose_cv_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(cv.glmnet.object)
dev.off()

# dfCompile(cv.glmnet.object) %>% names
# dfCompile(cv.glmnet.object) %>% head(2)



## ggDiagnose.Gam
library(gam)
gam.object <- gam::gam(Sepal.Length ~ gam::s(Sepal.Width) + Species,
                  data = iris)

jpeg(filename = paste0("base_Gam.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
par(mfrow = c(1,2))
plot(gam.object, se = TRUE, residuals = TRUE)
dev.off()

jpeg(filename = paste0("ggDiagnose_Gam.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(gam.object, residuals = TRUE) # se = TRUE by default
dev.off()

# dfCompile(gam.object) %>% names
# dfCompile(gam.object) %>% head(2)


## ggDiagnose.tree
library(tree)

tree.object <- tree(Sepal.Length ~., data = iris)

jpeg(filename = paste0("base_tree.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
plot(tree.object)
text(tree.object)
dev.off()

jpeg(filename = paste0("ggDiagnose_tree.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(tree.object, split.labels = FALSE) # check out quick labeling options
# with ?ggDiagnose.tree
dev.off()

jpeg(filename = paste0("ggDiagnose_tree_labels.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(tree.object, split.labels = TRUE,
           leaf.labels = TRUE)
dev.off()

# dfCompile(tree.object) %>% length
# dfCompile(tree.object)$segments %>% head
# dfCompile(tree.object)$labels %>% head
# dfCompile(tree.object)$leaf_labels %>% head


## ggDiagnose.matrix

# heatmap
data(iris)
iris_c <- iris %>% select(-Species) %>% cor %>% as.matrix 


jpeg(filename = paste0("base_matrix_heatmap.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
heatmap(iris_c)
dev.off()

jpeg(filename = paste0("ggDiagnose_matrix_heatmap.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose.matrix(iris_c, type = "heatmap",return = T, show.plot = F)$ggout + 
  scale_fill_gradientn(colours = grDevices::heat.colors(10))
dev.off()

# image
myurl = "http://stat.cmu.edu/~bpleroy/images/me.jpg"
data = jpeg::readJPEG(RCurl::getURLContent(myurl))[,,1]
data = t(data)[,rev(1:nrow(data))]

jpeg(filename = paste0("base_matrix_image.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
image(data, col = grey(seq(0, 1, length = 256)))
dev.off()


jpeg(filename = paste0("ggDiagnose_matrix_image.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose.matrix(data, type = "image",return =T, show.plot=F)$ggout + 
  scale_fill_gradient(high = "white",low = "black") 
dev.off()

dfCompile(iris_c, type = "heatmap")$df %>% head
# > dfCompile(iris_c, type = "heatmap")$df %>% head
#          .var1        .var2      value
# 1  Sepal.Width  Sepal.Width  1.0000000
# 2 Sepal.Length  Sepal.Width -0.1175698
# 3 Petal.Length  Sepal.Width -0.4284401
# 4  Petal.Width  Sepal.Width -0.3661259
# 5  Sepal.Width Sepal.Length -0.1175698
# 6 Sepal.Length Sepal.Length  1.0000000


dfCompile(iris_c, type = "image")$df %>% head
# > dfCompile(iris_c, type = "image")$df %>% head
#          .var1        .var2      value
# 1 Sepal.Length Sepal.Length  1.0000000
# 2  Sepal.Width Sepal.Length -0.1175698
# 3 Petal.Length Sepal.Length  0.8717538
# 4  Petal.Width Sepal.Length  0.8179411
# 5 Sepal.Length  Sepal.Width -0.1175698
# 6  Sepal.Width  Sepal.Width  1.0000000

