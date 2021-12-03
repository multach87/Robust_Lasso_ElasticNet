preds <- c("V23" , "V26" , "V30" , "V04" , "V32" , "V35" , 
           "V37" , "V08" , "V16" , "V36" , "V42" , "V45" , 
           "V46" , "V49" , "V50" , "V07" , "V01" , "V15" , 
           "V18" , "V25" , "V03" , "V31" , "V05" , 
           "V21" , "V22" , "V44")
sort(preds)
coefs.true2 <- coefs.true[coefs.V1 > 0 , coefnames]
for(i in 1:length(coefs.true2)) {
        split.temp <- strsplit(as.character(coefs.true2[i]) , 
                                split = "")[[1]]
        if(length(split.temp) == 2) {
                coefs.true2[i] <- paste(c(split.temp[1] , "0" , split.temp[2]) , collapse = "")
        }
}
sort(preds)
coefs.true2

missed <- c("V11" , "V29")
wrong <- c("V44")

coefs.true <- data.table("coefs" = t(wt.data[["betas"]]) , 
                         "coefnames" = row.names(t(wt.data[["betas"]])))

length(strsplit(as.character(coefs.true[10 , 2 , with = F]) , split = "")[[1]])

for(i in 1:nrow )

splittest <- strsplit(as.character(coefs.true[1 , 2 , with = F]) , split = "")[[1]]

paste(c(splittest[1] , "0" , splittest[2]) , collapse = "")

View(coefs.true[coefs.V1 > 0 , coefnames])

vars <- c("nums")

coefs.true[ , (vars) := rnorm(nrow(coefs.true))]

temp <- row.names(coefs.true)
coefs.true[ , ':=' (coefnames = temp)]


coefs.true[.SD , ('coefnames') := c(temp)]
row.names(coefs.true)

coefs


which(t(wt.data[["betas"]]) > 0)


actual <- t(wt.data[["betas"]][which(t(wt.data[["betas"]]) > 0)])

maybe <- c("V11" , "V20")


freq.test <- modelswt_all[["Frequencies"]][["By_Predictor"]]

ge51(freq.test[2 , c(1:7) , with = F])

names(wt.data[["betas"]])

parse_expr(testtest[ , "predictor"])

freq.test <- modelswt_all[["Frequencies"]][["By_Predictor"]]
TRUE %in% (sort(freq.test[1 , c(1:7) , with = F]) > 50)

sum(wt.data[["betas"]] == 0)

ge51 <- function(data) {
        ifelse(TRUE %in% (sort(data) > 50) , return(1) , return(0))
}
ge51(freq.test[1 , c(1:7) , with = F])

testtest <- modelswt_all[["Frequencies"]][["By_Predictor"]] %>%
        .[ , "ge51" := apply(.[ , 
                                !c("all" , "none" , "predictor")] , 
                             1 , ge51)]


freq.test <- list()


freq.test <- modelswt_all[1:7] %>%
        map(coefs_freq) %>%
        setDT %>%
        t %>%
        data.frame %>%
        setDT %>%
        .[ , "all" := apply(. , 1 , all_splits)] %>%
        .[ , "none" := apply(. , 1 , no_splits)] %>%
        .[ , "method" := names(modelswt_all[-8])] %>%
        setkey(. , method) 


freq.test2 <- list()

freq.test2 <- modelswt_all[1:7] %>%
        map(coefs_freq) %>%
        data.frame %>%
        setDT %>%
        .[ , "all" := apply(. , 1 , adapts_100)] %>%
        .[ , "none" := apply(.[ , !c("all")] , 1 , adapts_0)] %>%
        .[ , "predictor" := names(freq.test)[1:(ncol(freq.test) - 3)]] %>%
        setkey(. , predictor) 



test <- modelswt_all[["Frequencies"]]



tester2 <- function(data) { 
        for(i in 1:length(data)) {
                if(is.null(data[[i]][["metrics"]])) {
                        cat("error at i = " , i , "\n")
                } else {
                        cat("n.coefs = " , 
                            data[[i]][["metrics"]][["n.coefs"]] , 
                            "\n")
                }
        }
}
tester2(modelswt_all[["huberlasso"]])
tester2(modelswt_all[["huberelnet"]])
tester2(modelswt_all[["ladlasso"]])
tester2(modelswt_all[["ladelnet"]])
tester2(modelswt_all[["msak3"]])
tester2(modelswt_all[["msak5"]])
tester2(modelswt_all[["msak10"]])

saveRDS(ladlasso.1to20 , "/Users/Matt/Desktop/GitHub_Dissertation_Scripts/Robust_Lasso_ElasticNet/Temp/ladlasso_1to20.RData")

modelswt_nolad <- list(huberlasso = huberlasso.100 %>%
                             map(res_or_err) , 
                     huberelnet = huberelnet5.100 %>%
                             map(res_or_err) , 
                     msak3 = msaelnetk3.100 %>%
                             map(res_or_err) , 
                     msak5 = msaelnetk5.100 %>%
                             map(res_or_err) ,
                     msak10 = msaelnetk10.100 %>%
                             map(res_or_err))

for(i in 1:100) {
        if(!is.null(msaelnetk3.100[[i]][["error"]])) {
                cat("error at i = " , i , "\n")
        }
}


res_or_err2 <- function(data) {
        # if the 'error' list element is NULL, store the 'result' element contents
        if(is.null(data[["error"]])) {
                return(data[["result"]])
        } else { 
                # otherwise, store the 'error' element contents
                cat("error at")
                temp <- data[["error"]]
        }
        
        # save the resulting object
        return(temp)
}

grid.arrange(arrangeGrob(nominalpower.plot , actualpower.plot) , 
             nrow = 1 , arrangeGrob(mylegend) , ncol = 2 , widths = c(100 , 20))



pop.test <- list(normal = list(distribution = "normal" , population = rnorm(10000)) , 
                 heavy = list(distribution = "heavy" , population = c(rnorm(9000) , 
                                                                      rnorm(1000 , 
                                                                            sd = 15)
                                                                      ) ) , 
                 outlier = list(distribution = "outlier" , population = c(rnorm(9000) , 
                                                                          rnorm(1000 , 
                                                                                mean = 10)
                                                                          ) ) , 
                 g0h0 = list(distribution = "g0h0" , population = ghdist(n = 10000 , 
                                                                         g = 0 , 
                                                                         h = 0) ) , 
                 g2h0 = list(distribution = "g2h0" , population = ghdist(n = 10000 , 
                                                                         g = 0.2 , 
                                                                         h = 0) ) , 
                 g0h2 = list(distribution = "g0h2" , population = ghdist(n = 10000 , 
                                                                         g = 0 , 
                                                                         h = 0.2) ) , 
                 g2h2 = list(distribution = "g2h2" , population = ghdist(n = 10000 , 
                                                                       g = 0.2 , 
                                                                       h = 0.2) ) )




test <- hqmsa.sim.fnct(demo.split[[1]] , method = "LAD" , alpha = 1)

newdata <- readRDS("/Users/Matt/Desktop/GitHub_Dissertation_Scripts/Robust_Lasso_ElasticNet/Datasets/wtdata_cvsplit.RData")

newtest <- hqmsa.sim.fnct(newdata[[1]] , method = "msaenet")

wtcvtest <- hqmsa.sim.fnct(wtcv.100[[1]] , method = "LAD")

ladtest <- demo.split %>%
        map(safely(hqmsa.sim.fnct) , method = "LAD" , alpha = 1)
lad1test <- hqmsa.sim.fnct(demo.split[[1]] , method = "LAD" , alpha = 1)


test <- readRDS("/Users/Matt/Desktop/GitHub_Dissertation_Scripts/Robust_Lasso_ElasticNet/Datasets/models100.RData")



hqmsa.sim.fnct2 <- function(data.list , 
                           method = c("msaenet" , 
                                      "quantile" , "LAD" , 
                                      "huber") , 
                           tau = 0.5 , 
                           gamma = 1.345 , 
                           alpha = 0.5 , 
                           nsteps = 5L , 
                           print.time = TRUE) {
        # Store training X and Y to temporary objects
        X_train <- as.matrix(data.list[["X"]])
        Y_train <- as.matrix(data.list[["Y"]])
        
        if(method == "LAD") {
                method <- "quantile"
        }
        
        # If applicable, store holdout/testing X and Y
        if(!is.null(data.list[["X_test"]]) & 
           !is.null(data.list[["Y_test"]])) {
                X_test <- data.list[["X_test"]]
                Y_test <- data.list[["Y_test"]]
        } else {
                X_test <- NULL
                Y_test <- NULL
        }
        
        # lambdas to try for regularization
        lambda.try <- seq(log(1400) , log(0.01) , length.out = 100)
        lambda.try <- exp(lambda.try)
        
        # set a timer start point
        start <- Sys.time()
        
        # cross-validated selection of adaptive lasso
        # # tuning hyperparameter nu/gamma
        
        # # select ridge coefs for weighting
        ridge.model <- cv.glmnet(x = X_train , y = Y_train , 
                                 lambda = lambda.try , alpha = 0)
        lambda.ridge.opt <- ridge.model$lambda.min
        best.ridge.coefs <- predict(ridge.model , 
                                    type = "coefficients" ,
                                    s = lambda.ridge.opt)[-1]
        
        
        # # grid of nu/gamma values to try for cross-validation
        nu.try <- exp(seq(log(0.01) , log(10) , length.out = 100))
        
        # # initialize full list of LAD lasso results from each nu/gamma
        hqmsa.nu.cv.full <- list()
        
        # # initialize matrices of metrics and minimizing results
        hqmsa.nu.cv.lambda <- numeric()
        hqmsa.nu.cv.mse <- numeric()
        hqmsa.nu.cv.msesd <- numeric()
        hqmsa.nu.cv.coefs <- list()
        
        # # Loop over nu/gamma values for CV, 
        # # # storing minimizing lambda within each nu/gamma
        if(method == "msaenet") {
                for(i in 1:length(nu.try)) {
                        #single adaptive lasso run with ridge weighting and nu = 1
                        hqmsa.nu.cv.full[[i]] <- msaenet(x = X_train , 
                                                         y = Y_train , 
                                                         family = "gaussian" , 
                                                         init = "ridge" ,
                                                         alphas = 0.5 , 
                                                         tune = "cv" , 
                                                         nfolds = 5L , 
                                                         rule = "lambda.min" , 
                                                         nsteps = nsteps , 
                                                         tune.nsteps = "max" , 
                                                         scale = nu.try[i])
                        
                        hqmsa.nu.cv.lambda[i] <-
                                hqmsa.nu.cv.full[[i]]$best.lambdas[[nsteps + 1]]
                        
                        hqmsa.nu.cv.coefs[[i]] <- c(NA , coef(hqmsa.nu.cv.full[[i]]))
                        
                        
                        hqmsa.nu.cv.mse[i] <- min(hqmsa.nu.cv.full[[i]]$step.criterion[[nsteps + 1]])
                }
        } else {
                for(i in 1:length(nu.try)) {
                        invisible(capture.output(
                                hqmsa.nu.cv.full[[i]] <- 
                                        cv.hqreg(X = X_train , 
                                                 y = Y_train , 
                                                 method = method , 
                                                 tau = tau , 
                                                 gamma = gamma , 
                                                 lambda = lambda.try ,
                                                 alpha = alpha , 
                                                 preprocess =
                                                         "standardize" , 
                                                 screen = "ASR" , 
                                                 penalty.factor = 
                                                         1 / abs(best.ridge.coefs) ^ nu.try[i] , 
                                                 FUN = "hqreg" , 
                                                 type.measure = "mse"
                                        )
                        )
                        )
                        hqmsa.nu.cv.mse[i] <-
                                min(hqmsa.nu.cv.full[[i]]$cve)
                        hqmsa.nu.cv.msesd[i] <-
                                hqmsa.nu.cv.full[[i]]$cvse[
                                        which.min(hqmsa.nu.cv.full[[i]]$cve)
                                        ]
                        hqmsa.nu.cv.lambda[i] <-
                                hqmsa.nu.cv.full[[i]]$lambda.min
                        hqmsa.nu.cv.coefs[[i]] <- 
                                hqmsa.nu.cv.full[[i]]$fit$beta[ , 
                                                                which.min(hqmsa.nu.cv.full[[i]]$cve)
                                                                ]
                }
        }
        
        
        #specify minimizing nu value and resulting model info
        nu.opt <- nu.try[which.min(hqmsa.nu.cv.mse)]
        lambda.opt <- 
                hqmsa.nu.cv.lambda[
                        which.min(hqmsa.nu.cv.mse)
                        ]
        weights.opt <- 1 / abs(best.ridge.coefs) ^ nu.opt
        hqmsa.coefs <-
                hqmsa.nu.cv.coefs[[
                        which.min(hqmsa.nu.cv.mse)
                        ]]
        hqmsa.mse.min <- min(hqmsa.nu.cv.mse)
        if(!is.null(hqmsa.nu.cv.msesd[1])) {
                hqmsa.mse.min.se <- hqmsa.nu.cv.msesd[
                        which.min(hqmsa.nu.cv.mse)
                        ]               
        }
        
        hqmsa.model.min <- 
                hqmsa.nu.cv.full[
                        which.min(hqmsa.nu.cv.mse)
                        ]
        n.coefs <- sum(hqmsa.coefs[-1] != 0)
        
        # calculate metrics using holdout data, if applicable
        if(!is.null(X_test) & !is.null(Y_test)) {
                # store n
                n <- nrow(data.list[["X_test"]])
                
                # calculate predicted values
                y.pred <- data.list[["X_test"]] %*% hqmsa.coefs[-1]
                if(!is.na(hqmsa.coefs[1])) {
                        y.pred <- y.pred + hqmsa.coefs[1]
                }
                
                # calculate residual
                resid <- y.pred - Y_test
                
                # square the residuals
                resid.sq <- resid ^ 2
                
                # sum the square of residuals
                sum.resid.sq <- sum(resid.sq)
                
                #calculate root mse
                mse <- sum.resid.sq / n
                
                # set endpoint for timer
                end <- Sys.time()
                
                # temporarily store time of current model
                time <- abs(as.numeric(difftime(start , 
                                                end , 
                                                units = "secs"
                )
                )
                )
                
                # print the total runtime of the current model
                if(print.time) {
                        cat("time = " , time , " ;;; ")
                }
                
                
                # put conditions, model info, and metrics into list
                return(list(full.model = hqmsa.model.min ,
                            model.info = list(lambda = lambda.opt , 
                                              coefs = hqmsa.coefs , 
                                              weights = weights.opt
                            ) , 
                            metrics = list(n.coefs = n.coefs , 
                                           runtime = time , 
                                           mse = mse
                            )
                )
                )
        } else {
                # set endpoint for timer
                end <- Sys.time()
                
                # temporarily store time of current model
                time <- abs(as.numeric(difftime(start , 
                                                end , 
                                                units = "secs"
                )
                )
                )
                
                # print the total runtime of the current model
                if(print.time) {
                        cat("time = " , time , " ;;; ")
                }
                
                # put conditions, model info, and metrics into list
                return(list(full.model = hqmsa.model.min ,
                            model.info = list(lambda = lambda.opt , 
                                              coefs = hqmsa.coefs , 
                                              weights = weights.opt
                            ) , 
                            metrics = list(n.coefs = n.coefs , 
                                           runtime = time
                            )
                )
                )
        }
        
        
        
        
        
}




betas.test <- setDT(as.data.frame(matrix(0 , nrow = 1 , ncol = 30)))
betas <- function(x) {
        return(sample(x = c(0.5 , 1 , 1.5 , 2 , 0) , 
                      size = length(x) , replace = TRUE ,
                      prob = c(0.125 , 0.125 , 0.125 , 0.125 , 0.5)
                      )
               )
}


betas.test <- betas.test[ , lapply(.SD , betas)] 



X <- matrix(data <- c(1:300) , ncol = 30)

X %*% t(matrix(betas.test))


coefs_rank(ladlasso.models[["coef.frequencies"]])

test.freq <- models_all[1:7] %>%
       map(coefs_freq) %>%
       setDT %>%
       t %>%
       data.frame %>%
       setDT %>%
       .[ , "all" := apply(. , 1 , all_splits)] %>%
       .[ , "none" := apply(. , 1 , no_splits)] %>%
       .[ , "method" := names(models_all[-8])] %>%
       setkey(. , method) 

test.rank <- test.freq[ , 1:8 , with = F] %>%
       apply(. , 1 , coefs_rank) %>%
       t %>%
       as.data.frame %>%
       setDT %>%
       cbind(. , test.freq[ , "method"])



test.rank <- test.freq[ , 1:8] %>%
       map(coefs_rank) %>%
       setDT #%>%
       t


coefs_rank(as.numeric(test.freq[1 , 1:8]))

coefs_rank(test.freq[1 , 1:8])
           

           
           

       




test.freqrank <- models_all[1:7] %>%
       map(coefs_freqrank) %>%
       
       setDT %>%
       t %>%
       data.frame %>%
       setDT #%>%
       .[ , "all" := apply(. , 1 , all_splits)] %>%
       .[ , "none" := apply(. , 1 , no_splits)] %>%
       .[ , "method" := names(models_all[-8])] %>%
       setkey(. , method) 



test.all <- models_all[1:7] %>%
       map(coefs_freqrank)

test.1 <- models_all[[1]] %>%
       coefs_freq() %>%
       .[ , "frequencies"]
test.2 <- models_all[2] %>%
       coefs_freq()
test.3 <- models_all[3] %>%
       coefs_freq()

frankv(coefs.freq[ , "frequencies"] , order = -1 , ties.method = "min")

coefs.freq <- setDT(as.data.frame(x = matrix(nrow = length(ladlasso.models[[1]][["model.info"]][["coefs"]][-1])) , 
              row.names = names(ladlasso.models[[1]][["model.info"]][["coefs"]][-1]))) %>%
       .[ , ':=' (frequencies = 0 , 
                  rank = NA)] %>%
       .[ , !1 , with = F]


for(i in 1:10) {
       for(j in 1:nrow(coefs.freq)) {
              if(ladlasso.models[[i]][["model.info"]][["coefs"]][(j + 1)] != 0) {
                     coefs.freq[j , "frequencies" := coefs.freq[j , "frequencies"] + 1]
              }
       }
}


coefs.freq[1 , "frequencies"]
ladlasso.models[[1]][["model.info"]][["coefs"]][(2)] != 0

