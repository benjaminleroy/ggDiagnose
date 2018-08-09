library(ggDiagnose)


## ggDiagnose.lm
lm_object <- lm(Sepal.Length ~., data = iris)

jpeg(filename = paste0("base_lm.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
par(mfrow = c(2,3))
plot(lm_object, which = 1:6)
dev.off()

jpeg(filename = paste0("ggDiagnose_lm.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(lm_object, which = 1:6)
dev.off()

## ggDiagnose.glmnet

library(glmnet)

glmnet_object <- glmnet(y = iris$Sepal.Length,
               x = model.matrix(Sepal.Length~., data = iris))

jpeg(filename = paste0("base_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
plot(glmnet_object)
dev.off()

jpeg(filename = paste0("ggDiagnose_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(glmnet_object)
dev.off()


## ggDiagnose.cv.glmnet

cv_glmnet_object <- cv.glmnet(y = iris$Sepal.Length,
                              x = model.matrix(Sepal.Length~., data = iris))

jpeg(filename = paste0("base_cv_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
plot(cv_glmnet_object)
dev.off()

jpeg(filename = paste0("ggDiagnose_cv_glmnet.jpeg"),
     width = 10, height = 6.5, units = "in", res = 100)
ggDiagnose(cv_glmnet_object)
dev.off()


## ggDiagnose.gam

gam_object <- gam(Sepal.Length ~ s(Sepal.Width) + Species,
                  data = iris)
