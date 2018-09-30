context("Linear Regression, ggDiagnose")
library(ggDiagnose)

test_that(paste("lm.dfCompile returns correct objects for basic lm",
                "based on different requests"),
          {
            lm_basic = lm(Sepal.Length ~., data = iris)
            out <- ggDiagnose(lm_basic)
            
            expect_equal(out, NULL, 
                         info = paste0("default lm.ggDiagnose should",
                                       "return NULL (nothing)"))
            #nrows
            out <- ggDiagnose(lm_basic,return = TRUE, show.plot = FALSE)
            
            # 
            expect_equal(length(out),2, 
                         info = paste("lm.ggDiagnose should return 2 objects",
                                      "when return is requested"))
            expect_equal(class(out[[2]]), "list",
                         info = paste("lm.ggDiagnose second object should be a",
                                      "list of graphics"))
            expect_equal(length(out[[2]]), 4,
                         info = paste("lm.ggDiagnose second object should be a",
                                      "list of graphics"))
            expect_equal(class(out[[1]]), "data.frame",
                         info = paste("lm.ggDiagnose first object should be a",
                                      "a data frame"))
            
            out_df <- dfCompile(lm_basic)
            expect_equal(out[[1]], out_df,
                         info = paste("lm.ggDiagnose first object should be a",
                                      "a data frame  (same as lm.dfCompile)"))
            # ^ may change someday
           
          })
            