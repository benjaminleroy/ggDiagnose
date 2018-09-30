context("Linear Regression, dfCompile")
library(ggDiagnose)

test_that("lm.dfCompile returns correct data frame for basic lm",
          {
          lm_basic = lm(Sepal.Length ~., data = iris)
          df_lm = dfCompile(lm_basic)
          
          #nrows
          expect_equal(nrow(df_lm),nrow(iris), 
                       info = "nrows dimensions incorrect")
          
          # 
          expect_equal(df_lm[,1:ncol(iris)], iris, 
                       info = paste("lm.dfCompile not correctly acting",
                                    "like broom::attach"))
          
          })

test_that("lm.dfCompile returns correct data frame for non-basic lm",
          {
            lm_basic = lm(log(Sepal.Length) ~ I(Species == "setosa"), 
                          data = iris)
            df_lm = dfCompile(lm_basic)
            
            #names for model
            expect_equal(names(df_lm)[1:2],
                         c("log(Sepal.Length)", "I(Species == \"setosa\")"), 
                         info = paste0("names of models fit the ",
                                       "broom::attach framework"))
            #nrows
            expect_equal(nrow(df_lm),nrow(iris), 
                         info = "nrows dimensions incorrect")
          })
