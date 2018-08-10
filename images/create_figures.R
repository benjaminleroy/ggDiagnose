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


## ggDiagnose.Gam

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

