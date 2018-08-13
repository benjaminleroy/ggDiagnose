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
