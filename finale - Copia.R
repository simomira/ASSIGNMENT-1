## ----dataset creation-----------------------------------------------------------------------------------------------------------------
set.seed(29012001)

x1 <- cbind(runif(100, 0, 1))
x2 <- cbind(runif(100, 0, 1))
X <- as.data.frame(cbind(x1,x2))

term1 <- 0.75 * exp(-(9*x1-2)^2/4 - (9*x2-2)^2/4)
term2 <- 0.75 * exp(-(9*x1+1)^2/49 - (9*x2+1)/10)
term3 <- 0.5 * exp(-(9*x1-7)^2/4 - (9*x2-3)^2/4)
term4 <- -0.2 * exp(-(9*x1-4)^2 - (9*x2-7)^2)
  
y <- cbind(term1 + term2 + term3 + term4 + 0.001*rnorm(100))

dataset <- cbind(X,y)
head(dataset)



## ----3d plot, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------
require(scatterplot3d);
graph<-scatterplot3d(x1, x2, y, xlab="x1", ylab="x2", 
                       zlab="y", type="h", highlight.3d=FALSE, 
                       col.axis="black",col.grid="gray",
                       angle=60, pch=16)
graph$points3d(mean(x1), mean(x2), mean(y), 
                 col="red", type="h", pch=16)



## ----results matrix-------------------------------------------------------------------------------------------------------------------
results <- matrix(data=NA, nrow=2,ncol=5)
rownames(results)<-cbind("MSE","R2") 
names<-c()
for (i in 1:5){
  names[i] <- paste("order", as.character(i))
}
colnames(results) <- names


## ----matrix function creation---------------------------------------------------------------------------------------------------------
model_matrix <- function(x,y,n){
  l <- length(x)
  if (n==1){
    X <- cbind(rep(1,l),x,y)
  }
  else if (n==2){
    X <- cbind(rep(1,l),x,y,x**2,y**2,x*y)
  }
  else if (n==3){
    X <- cbind(rep(1,l),x , y , x**2 , y**2 , x*y , x**3 , y**3, (x**2)*y, x*(y**2))
  }
  else if (n==4){
    X <- cbind(rep(1,l),x , y , x**2 , y**2 , x*y , x**3 , y**3, (x**2)*y, x*(y**2), (x**2)*(y**2),x**4, y**4,(x**3)*y,x*(y**3))
  }
  else if (n==5){
    X <- cbind(rep(1,l),x , y , x**2 , y**2 , x*y , x**3 , y**3, (x**2)*y, x*(y**2), (x**2)*(y**2),x**4, y**4,(x**3)*y,x*(y**3),x**5, y**5,(x**3)*(y**2),(x**2)*(y**3))
  }
return(X)
}


## ----beta function creation-----------------------------------------------------------------------------------------------------------
betas <- function(matX,vec_y){
  beta <- solve(t(matX) %*% matX) %*% t(matX) %*% vec_y
  return(beta)
}


## ----prediction function creation-----------------------------------------------------------------------------------------------------
predictions <- function(x,y,n,beta){
  if (n==1){
    prediction <- beta[1] + beta[2]*x + beta[3]*y 
  }
  else if (n==2){
    prediction <- beta[1] + beta[2]* x + beta[3]* y + beta[4]* x**2 + beta[5]* y**2 + beta[6]* x*y 
  }
  else if (n==3){
    prediction <- beta[1] + beta[2]* x + beta[3]* y + beta[4]* x**2 + beta[5]* y**2 + beta[6]* x*y + beta[7]* x**3 + beta[8]* y**3 + beta[9]* (x**2)*y + beta[10]* x*(y**2)
  }
  else if (n==4){
    prediction <- beta[1] + beta[2]* x + beta[3]* y + beta[4]* x**2 + beta[5]* y**2 + beta[6]* x*y + beta[7]* x**3 + beta[8]* y**3 + beta[9]* (x**2)*y + beta[10]* x*(y**2) + beta[11]* (x**2)*(y**2) + beta[12]* x**4 + beta[13]* y**4 + beta[14]* (x**3)*y+ beta[15]* x*(y**3)
  }
  else if (n==5){
    prediction <- beta[1] + beta[2]* x + beta[3]* y + beta[4]* x**2 + beta[5]* y**2 + beta[6]* x*y + beta[7]* x**3 + beta[8]* y**3 + beta[9]* (x**2)*y + beta[10]* x*(y**2) + beta[11]* (x**2)*(y**2) + beta[12]* x**4 + beta[13]* y**4 + beta[14]* (x**3)*y+ beta[15]* x*(y**3) + beta[16]* x**5 + beta[17]* y**5 + beta[18]* (x**3)*(y**2) + beta[19]* (x**2)*(y**3)
  }
  
  return(prediction)
  
}


## ----split function creation----------------------------------------------------------------------------------------------------------
train_test <- function(X,y){
  set.seed(29012001)
  trRowIndex <- sample(1:nrow(X), 0.8*nrow(X))
  
  trdataX <- X[trRowIndex, ]
  trdataY <- y[trRowIndex, ]
  
  tedataX <- X[-trRowIndex, ]
  tedataY <- y[-trRowIndex, ]
  
  Train_Test <- list(trdataX, trdataY, tedataX, tedataY)
  
  return(Train_Test)
}



## -------------------------------------------------------------------------------------------------------------------------------------
translate <- function(vector){
  v <- c()
  for (i in 1:length(vector)){
    v[i] <- toString(vector[i])
  }
  return(v)
}


## ----implementation-------------------------------------------------------------------------------------------------------------------
splitted <- train_test(X,y)

trainingX <- splitted[[1]]
trainingY <- splitted[[2]]
testX <- splitted[[3]]
testY <- splitted[[4]]

matrix_deg1 <- model_matrix(trainingX[,1],trainingX[,2],1)
rownames(matrix_deg1) <- translate(rownames(trainingX))
testX_1 <- as.data.frame(model_matrix(testX$V1,testX$V2,1))
beta_deg1 <- betas(matrix_deg1, trainingY)
pred_deg1 <- predictions(testX[,1],testX[,2],1,beta_deg1)
mse_deg1 <- mean((testY - pred_deg1)^2)
r2_deg1 <- 1 - sum((testY - pred_deg1)^2)/sum((testY - mean(testY))^2)


matrix_deg2 <- model_matrix(trainingX[,1],trainingX[,2],2)
rownames(matrix_deg2) <- translate(rownames(trainingX))
testX_2 <- as.data.frame(model_matrix(testX$V1,testX$V2,2))
beta_deg2 <- betas(matrix_deg2, trainingY)
pred_deg2 <- predictions(testX[,1],testX[,2],2,beta_deg2)
mse_deg2 <- mean((testY - pred_deg2)^2)
r2_deg2 <- 1 - sum((testY - pred_deg2)^2)/sum((testY - mean(testY))^2)


matrix_deg3 <- model_matrix(trainingX[,1],trainingX[,2],3)
rownames(matrix_deg3) <- translate(rownames(trainingX))
testX_3 <- as.data.frame(model_matrix(testX$V1,testX$V2,3))
beta_deg3 <- betas(matrix_deg3, trainingY)
pred_deg3 <- predictions(testX[,1],testX[,2],3,beta_deg3)
mse_deg3 <- mean((testY - pred_deg3)^2)
r2_deg3 <- 1 - sum((testY - pred_deg3)^2)/sum((testY - mean(testY)^2))


matrix_deg4 <- model_matrix(trainingX[,1],trainingX[,2],4)
rownames(matrix_deg4) <- translate(rownames(trainingX))
testX_4 <- as.data.frame(model_matrix(testX$V1,testX$V2,4))
beta_deg4 <- betas(matrix_deg4, trainingY)
pred_deg4 <- predictions(testX[,1],testX[,2],4,beta_deg4)
mse_deg4 <- mean((testY - pred_deg4)^2)
r2_deg4 <- 1 - sum((testY - pred_deg4)^2)/sum((testY - mean(testY))^2)


matrix_deg5 <- model_matrix(trainingX[,1],trainingX[,2],5)
rownames(matrix_deg5) <- translate(rownames(trainingX))
testX_5 <- as.data.frame(model_matrix(testX$V1,testX$V2,5))
beta_deg5 <- betas(matrix_deg5, trainingY)
pred_deg5 <- predictions(testX[,1],testX[,2],5,beta_deg5)
mse_deg5 <- mean((testY - pred_deg5)^2)
r2_deg5 <- 1 - sum((testY - pred_deg5)^2)/sum((testY - mean(testY))^2)

results[1,] <- rbind(mse_deg1, mse_deg2, mse_deg3, mse_deg4, mse_deg5)
results[2,] <- rbind(r2_deg1, r2_deg2, r2_deg3, r2_deg4, r2_deg5)

results


## ----implementation with different noise----------------------------------------------------------------------------------------------
y2 <- cbind(term1 + term2 + term3 + term4 + 0.5*rnorm(100))

results_2 <- matrix(data=NA, nrow=2,ncol=5)
rownames(results_2)<-cbind("MSE","R2")
names<-c()
for (i in 1:5){
  names[i] <- paste("order", as.character(i))
}
colnames(results_2) <- names

splitted_2 <- train_test(X,y2)

matrix_deg1_2 <- model_matrix(splitted_2[[1]][,1],splitted_2[[1]][,2],1); 
beta_deg1_2 <- betas(matrix_deg1_2, splitted_2[[2]])
pred_deg1_2 <- predictions(splitted_2[[3]][,1],splitted_2[[3]][,2],1,beta_deg1_2)
mse_deg1_2 <- mean((splitted_2[[4]] - pred_deg1_2)^2)
r2_deg1_2 <- 1 - sum((splitted_2[[4]] - pred_deg1_2)^2)/sum((splitted_2[[4]] - mean(splitted_2[[4]]))^2)


matrix_deg2_2 <- model_matrix(splitted_2[[1]][,1],splitted_2[[1]][,2],2); 
beta_deg2_2 <- betas(matrix_deg2_2, splitted_2[[2]])
pred_deg2_2 <- predictions(splitted_2[[3]][,1],splitted_2[[3]][,2],2,beta_deg2_2)
mse_deg2_2 <- mean((splitted_2[[4]] - pred_deg2_2)^2)
r2_deg2_2 <- 1 - sum((splitted_2[[4]] - pred_deg2_2)^2)/sum((splitted_2[[4]] - mean(splitted_2[[4]]))^2)


matrix_deg3_2 <- model_matrix(splitted_2[[1]][,1],splitted_2[[1]][,2],3); 
beta_deg3_2 <- betas(matrix_deg3_2, splitted_2[[2]])
pred_deg3_2 <- predictions(splitted_2[[3]][,1],splitted_2[[3]][,2],3,beta_deg3_2)
mse_deg3_2 <- mean((splitted_2[[4]] - pred_deg3_2)^2)
r2_deg3_2 <- 1 - sum((splitted_2[[4]] - pred_deg3_2)^2)/sum((splitted_2[[4]] - mean(splitted_2[[4]]))^2)


matrix_deg4_2 <- model_matrix(splitted_2[[1]][,1],splitted_2[[1]][,2],4); 
beta_deg4_2 <- betas(matrix_deg4_2, splitted_2[[2]])
pred_deg4_2 <- predictions(splitted_2[[3]][,1],splitted_2[[3]][,2],4,beta_deg4_2)
mse_deg4_2 <- mean((splitted_2[[4]] - pred_deg4_2)^2)
r2_deg4_2 <- 1 - sum((splitted_2[[4]] - pred_deg4_2)^2)/sum((splitted_2[[4]] - mean(splitted_2[[4]]))^2)


matrix_deg5_2 <- model_matrix(splitted_2[[1]][,1],splitted_2[[1]][,2],5); 
beta_deg5_2 <- betas(matrix_deg5_2, splitted_2[[2]])
pred_deg5_2 <- predictions(splitted_2[[3]][,1],splitted_2[[3]][,2],5,beta_deg5_2)
mse_deg5_2 <- mean((splitted_2[[4]] - pred_deg5_2)^2)
r2_deg5_2 <- 1 - sum((splitted_2[[4]] - pred_deg5_2)^2)/sum((splitted_2[[4]] - mean(splitted_2[[4]]))^2)

results_2[1,] <- rbind(mse_deg1_2, mse_deg2_2, mse_deg3_2, mse_deg4_2, mse_deg5_2)
results_2[2,] <- rbind(r2_deg1_2, r2_deg2_2, r2_deg3_2, r2_deg4_2, r2_deg5_2)

results_2



## ----SE and CI function creation------------------------------------------------------------------------------------------------------
standard_errors <- function(trainY, matX, beta){
  dSigmaSq <- sum((trainY - matX%*%beta)^2)/(nrow(matX)-ncol(matX))
mVarCovar <- dSigmaSq*chol2inv(chol(t(matX)%*%matX))
vStdErr <- sqrt(diag(mVarCovar))
print(cbind(beta, vStdErr))
return(vStdErr)}

confidence_int <- function(beta, se){
  c <- matrix(data = NA, nrow = length(beta), ncol = 2)
  colnames(c) <- c("Lower","Upper")
  for (i in 1:length(beta)){
    interval = cbind(beta[i]-1.96*se[i], beta[i]+1.96*se[i])
    c[i,] <- interval
  }
return(c)}


## ----confidence intervals-------------------------------------------------------------------------------------------------------------
se_1 <- standard_errors(trainingY, matrix_deg1, beta_deg1)
confidence_int(beta_deg1, se_1)

se_2 <- standard_errors(trainingY, matrix_deg2, beta_deg2)
confidence_int(beta_deg2, se_2)

se_3 <- standard_errors(trainingY, matrix_deg3, beta_deg3)
confidence_int(beta_deg3, se_3)

se_4 <- standard_errors(trainingY, matrix_deg4, beta_deg4)
confidence_int(beta_deg4, se_4)

se_5 <- standard_errors(trainingY, matrix_deg5, beta_deg5)
confidence_int(beta_deg5, se_5)




## ----MSEs training--------------------------------------------------------------------------------------------------------------------
pred_deg_train1 <- predictions(trainingX[,1],trainingX[,2],1,beta_deg1)
mse_deg_train1 <- mean((trainingY - pred_deg_train1)^2)

pred_deg_train2 <- predictions(trainingX[,1],trainingX[,2],2,beta_deg2)
mse_deg_train2 <- mean((trainingY - pred_deg_train2)^2)

pred_deg_train3 <- predictions(trainingX[,1],trainingX[,2],3,beta_deg3)
mse_deg_train3 <- mean((trainingY - pred_deg_train3)^2)

pred_deg_train4 <- predictions(trainingX[,1],trainingX[,2],4,beta_deg4)
mse_deg_train4 <- mean((trainingY - pred_deg_train4)^2)

pred_deg_train5 <- predictions(trainingX[,1],trainingX[,2],5,beta_deg5)
mse_deg_train5 <- mean((trainingY - pred_deg_train5)^2)


## ----MSEs matrix----------------------------------------------------------------------------------------------------------------------
msematrix <- data.frame(1:5, rbind(mse_deg1,mse_deg2,mse_deg3,mse_deg4,mse_deg5),rbind(mse_deg_train1,mse_deg_train2,mse_deg_train3,mse_deg_train4,mse_deg_train5))
colnames(msematrix) <- c("order", "test", "train")
rownames(msematrix) <- c(1:5)
msematrix


## ----train vs test MSE plot, message=FALSE, warning=FALSE-----------------------------------------------------------------------------
require(ggplot2)

ggplot()+
  geom_point(data=msematrix, aes(x=order, y=test), size=2)+
  geom_line(data=msematrix, aes(x=order, y=test, colour="test"), size=1)+
  geom_point(data=msematrix, aes(x=order, y=train), size=2)+
  geom_line(data=msematrix, aes(x=order, y=train, colour="training"), size=1)+
  ylab("Mse")+ggtitle("Training MSEs vs Test MSEs")


## ----MSE with different sample size---------------------------------------------------------------------------------------------------
datapoints <- c(20, 50, 100, 150, 200, 1000, 2000)
comparison_matrix <- as.data.frame(matrix(NA, 7, 3))
comparison_matrix[,1] <- datapoints
colnames(comparison_matrix) <- c("n", "mse_test", "mse_train")

for (i in 1:7){
  n <- datapoints[i]
  x1_bis <- cbind(runif(n, 0, 1))
  x2_bis <- cbind(runif(n, 0, 1))
  X_bis <- as.data.frame(cbind(x1_bis,x2_bis))

  term1_bis <- 0.75 * exp(-(9*x1_bis-2)^2/4 - (9*x2_bis-2)^2/4)
  term2_bis <- 0.75 * exp(-(9*x1_bis+1)^2/49 - (9*x2_bis+1)/10)
  term3_bis <- 0.5 * exp(-(9*x1_bis-7)^2/4 - (9*x2_bis-3)^2/4)
  term4_bis <- -0.2 * exp(-(9*x1_bis-4)^2 - (9*x2_bis-7)^2)
  
  y_bis <- cbind(term1_bis + term2_bis + term3_bis + term4_bis + 0.001*rnorm(n))
  
  splitted_bis <- train_test(X_bis,y_bis)

  trainingX_bis <- splitted_bis[[1]]
  trainingY_bis <- splitted_bis[[2]]
  testX_bis <- splitted_bis[[3]]
  testY_bis <- splitted_bis[[4]]

  matrix_deg3_bis <- model_matrix(trainingX_bis[,1],trainingX_bis[,2],3)
  beta_deg3_bis <- betas(matrix_deg3_bis, trainingY_bis)
  pred_deg3_bis <- predictions(testX_bis[,1],testX_bis[,2],3,beta_deg3_bis)
  mse_deg3_bis <- mean((testY_bis - pred_deg3_bis)^2)

  pred_deg_train3_bis <- predictions(trainingX_bis[,1],trainingX_bis[,2],3,beta_deg3_bis)
  mse_deg_train3_bis <- mean((trainingY_bis - pred_deg_train3_bis)^2)

  comparison_matrix[i,] <- cbind(n, mse_deg3_bis, mse_deg_train3_bis)
}

ggplot()+
  geom_point(data=comparison_matrix, aes(x=n, y=mse_test), size=2)+
  geom_line(data=comparison_matrix, aes(x=n, y=mse_test, colour="mse_test"), size=1)+
  geom_point(data=comparison_matrix, aes(x=n, y=mse_train), size=2)+
  geom_line(data=comparison_matrix, aes(x=n, y=mse_train, colour="mse_train"), size=1)+
  ylab("Mse")+ggtitle("Training MSEs vs Test MSEs")


## ----bootstrap ,message=FALSE, warning=FALSE------------------------------------------------------------------------------------------
set.seed(29012001)

pre = function(data, index, datitest) return(predict(lm(trainingY ~.-1,data = as.data.frame(data),
                                                                      subset = index), newdata=datitest))

numofboot <- 500
results_matrix <- matrix(NA, 5,5)
colnames(results_matrix) <- c("order","MSE","bias2","variance","proof")
for (degree in 1:5){
  
  mat <- get(paste("matrix_deg",toString(degree),sep=""))
  test <- get(paste("testX_", toString(degree), sep=""))
  prediction_matrix <- matrix(0, nrow = nrow(testX), ncol = numofboot) # 20x500 matrix
  prediction_means <- matrix(0,1,nrow = nrow(testX)) # 20x1 vector
  rownames(prediction_matrix) <- rownames(testX)
  
  for (k in 1:numofboot){
    p <- pre(mat, sample(as.numeric(rownames(trainingX)), 80, replace = T),test)
    
    prediction_matrix[,k] = cbind(p) #fill the matrix with predictions for each bootstrap
  }
  
  for (i in 1:nrow(prediction_matrix)){
    
   prediction_means[i,] <- mean(prediction_matrix[i,]) #filling the vector with the mean prediction for each test data
  }
  bias_squared <- mean((testY - prediction_means)^2)
  
  q <- matrix(0,1,nrow = nrow(testX))
  for (k in 1:ncol(prediction_matrix)){
  q <- q + (prediction_matrix[,k]-prediction_means)^2}
  variance <- 1/(numofboot*20)*sum(q); variance
  
  m <- matrix(0,1,nrow = nrow(testX))
  for (k in 1:ncol(prediction_matrix)){
  m <- m + (testY - prediction_matrix[,k])^2}
  mean_squared_error <- 1/(numofboot*20) * sum(m); mean_squared_error
  
  proof <- mean_squared_error- variance -bias_squared
  results_matrix[degree,] <- c(degree,mean_squared_error, bias_squared, variance, round(proof,8))
}
results_matrix




## ----bootstrap results plot-----------------------------------------------------------------------------------------------------------
require(ggplot2)
ggplot()+
  geom_point(data=as.data.frame(results_matrix), aes(x=order, y=variance), size=2)+
  geom_line(data=as.data.frame(results_matrix), aes(x=order, y=variance, colour="variance"), size=1)+
  geom_point(data=as.data.frame(results_matrix), aes(x=order, y=bias2), size=2)+
  geom_line(data=as.data.frame(results_matrix), aes(x=order, y=bias2, colour="bias2"), size=1)+
  geom_point(data=as.data.frame(results_matrix), aes(x=order, y=MSE), size=2)+
  geom_line(data=as.data.frame(results_matrix), aes(x=order, y=MSE, colour="MSE"), size=1)


## ----cross validation, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
require(gridExtra)

numfolds <- c(5,10,nrow(dataset))
matrixlist <- list()
msematrix_cv <- as.data.frame(matrix(NA, nrow = 5,3))
colnames(msematrix_cv) <- c("order", "test_cv", "train_cv")
 

for (k in 1:length(numfolds)){
  folds<-cut(seq(1,nrow(dataset)),breaks=numfolds[k],labels=FALSE)

  for (degree in 1:5){
    
    msestest <- c()
    msestrain <- c()
    
    for(i in 1:numfolds[k]){ 
      
      testIndexes_cv <- which(folds==i,arr.ind=TRUE)
      testData_cv <- dataset[testIndexes_cv, ]
      trainData_cv <- dataset[-testIndexes_cv, ]
      dataset_cv<-model_matrix(trainData_cv[,1],trainData_cv[,2],degree)
      beta_cv <- betas(dataset_cv, trainData_cv[,3])
      predmse_cv <- predictions(testData_cv[,1],testData_cv[,2],degree,beta_cv)
      msetest_cv <- mean((testData_cv[,3] - predmse_cv)^2)
      predtrain_cv<-predictions(trainData_cv[,1],trainData_cv[,2],degree,beta_cv)
      msetrain_cv <- mean((trainData_cv[,3] - predtrain_cv)^2)
      msestest[i] <- msetest_cv
      msestrain[i] <- msetrain_cv
    }
    
    msematrix_cv[degree,] <- c(degree, mean(msestest), mean(msestrain))
  }
  
  matrixlist[[k]] <- msematrix_cv
}
fivefolds<- matrixlist[[1]]
tenfolds <- matrixlist[[2]]
nfolds <- matrixlist[[3]]

p1 <- ggplot()+  geom_point(data=msematrix, aes(x=order, y=test), size=2)+  geom_line(data=msematrix, aes(x=order, y=test, colour="test"), size=1)+  geom_point(data=msematrix, aes(x=order, y=train), size=2)+  geom_line(data=msematrix, aes(x=order, y=train, colour="training"), size=1)+  ylab("Mse")+ggtitle("Tr MSE vs Te MSEs 1 sample")

p2 <- ggplot()+  geom_point(data=fivefolds, aes(x=order, y=test_cv), size=2)+  geom_line(data=fivefolds, aes(x=order, y=test_cv, colour="test_cv"), size=1)+  geom_point(data=fivefolds, aes(x=order, y=train_cv), size=2)+  geom_line(data=fivefolds, aes(x=order, y=train_cv, colour="train_cv"), size=1)+  ylab("Mse")+ggtitle("Tr MSE vs Te MSE 5-FOLDS CV")

p3 <- ggplot()+  geom_point(data=tenfolds, aes(x=order, y=test_cv), size=2)+  geom_line(data=tenfolds, aes(x=order, y=test_cv, colour="test_cv"), size=1)+  geom_point(data=tenfolds, aes(x=order, y=train_cv), size=2)+  geom_line(data=tenfolds, aes(x=order, y=train_cv, colour="train_cv"), size=1)+  ylab("Mse")+ggtitle("Tr MSE vs Te MSE 10-FOLDS CV")

p4 <- ggplot()+  geom_point(data=nfolds, aes(x=order, y=test_cv), size=2)+  geom_line(data=nfolds, aes(x=order, y=test_cv, colour="test_cv"), size=1)+  geom_point(data=nfolds, aes(x=order, y=train_cv), size=2)+  geom_line(data=nfolds, aes(x=order, y=train_cv, colour="train_cv"), size=1)+  ylab("Mse")+ggtitle("Tr MSE vs Te MSE k-FOLDS CV")

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow=2)



## ----final comparisons----------------------------------------------------------------------------------------------------------------
comparisonMSE <- matrix(NA, 5, 4)
dimnames(comparisonMSE) <- list(c(1:5), c("5-folds", "10-folds", "n-folds", "bootstrap"))
comparisonMSE[,1] <- fivefolds[,2]
comparisonMSE[,2] <- tenfolds[,2]
comparisonMSE[,3] <- nfolds[,2]
comparisonMSE[,4] <- results_matrix[,2]
comparisonMSE


## ----scaling function-----------------------------------------------------------------------------------------------------------------
scaling <- function(vector){
  mysd <- function(u) sqrt(sum((u - mean(u))^2)/length(u))
  vnew <- scale(vector, center = TRUE, scale = apply(vector,2,mysd))
  return(vnew)
}


## ----scaled data 1--------------------------------------------------------------------------------------------------------------------
scaled_trainingX <- cbind(scaling(trainingX))
rownames(scaled_trainingX) <- translate(rownames(trainingX))
scaled_trainingY <- scaling(as.matrix(trainingY))
rownames(scaled_trainingY) <- translate(rownames(trainingX))
scaled_testX <- cbind(scaling(testX)) 
rownames(scaled_testX) <- translate(rownames(testX))
scaled_testY<- scaling(as.matrix(testY))
rownames(scaled_testY) <- translate(rownames(testX))


## ----scaled data 2--------------------------------------------------------------------------------------------------------------------
matrix_deg_sc1 <- model_matrix(scaled_trainingX[,1],scaled_trainingX[,2],1)
matrix_deg_sc2 <- model_matrix(scaled_trainingX[,1],scaled_trainingX[,2],2)
matrix_deg_sc3 <- model_matrix(scaled_trainingX[,1],scaled_trainingX[,2],3)
matrix_deg_sc4 <- model_matrix(scaled_trainingX[,1],scaled_trainingX[,2],4)
matrix_deg_sc5 <- model_matrix(scaled_trainingX[,1],scaled_trainingX[,2],5)

testX_sc_1 <- as.data.frame(model_matrix(scaled_testX[,1],scaled_testX[,2],1))
testX_sc_2 <- as.data.frame(model_matrix(scaled_testX[,1],scaled_testX[,2],2))
testX_sc_3 <- as.data.frame(model_matrix(scaled_testX[,1],scaled_testX[,2],3))
testX_sc_4 <- as.data.frame(model_matrix(scaled_testX[,1],scaled_testX[,2],4))
testX_sc_5 <- as.data.frame(model_matrix(scaled_testX[,1],scaled_testX[,2],5))


## ----betaridge function---------------------------------------------------------------------------------------------------------------
betaridge <- function(matX,vec_y,lambda){
  ridge <- solve(t(matX) %*% matX + lambda * diag(1,ncol(matX), ncol(matX))) %*% t(matX) %*% vec_y
  return(ridge)
}


## ----ridge regression bootstrap-------------------------------------------------------------------------------------------------------
numlambda <- 10
mseridge <- list()
lambdas <- 10^(seq(2,-2,length=numlambda))
pred_rid = function(matrice, index, datitest, trainy, lambda){
    index <- translate(index)
    ridge_b<-betaridge(matrice[index,], trainy[index,], lambda)
    return(predictions(datitest[,1],datitest[,2],degree,ridge_b))}

ty <- cbind(trainingY)
rownames(ty) <- translate(rownames(trainingX))

for (l in 1:numlambda){
  lmb <- lambdas[l]
  numofboot_r <- 1000
  results_matrix_r <- matrix(NA, 5,6)
  colnames(results_matrix_r) <- c("order","MSE","bias2","variance","proof", "used_lambda")
  for (degree in 1:5){
    mat_r <- get(paste("matrix_deg",toString(degree),sep=""))
    test_r <- get(paste("testX_", toString(degree), sep=""))
    prediction_matrix_r <- matrix(0, nrow = nrow(testX), ncol = numofboot_r)
    prediction_means_r <- matrix(0,1,nrow = nrow(testX))
    rownames(prediction_matrix_r) <- rownames(testX)
    for (f in 1:numofboot_r){
      p_r <- pred_rid(mat_r,sample(as.numeric(rownames(trainingX)), 80, replace = T),test_r, ty, lmb)
      prediction_matrix_r[,f] = cbind(p_r) 
    }
    
    for (i in 1:nrow(prediction_matrix_r)){
      
    prediction_means_r[i,] <- mean(prediction_matrix_r[i,])}
    bias_squared_r <- mean((testY - prediction_means_r)^2)
    
    q_r <- matrix(0,1,nrow = nrow(testX))
    for (z in 1:ncol(prediction_matrix_r)){
    q_r <- q_r + (prediction_matrix_r[,z]-prediction_means_r)^2}
    variance_r <- 1/(numofboot_r*20)*sum(q_r); variance_r
    
    m_r <- matrix(0,1,nrow = nrow(testX))
    for (k in 1:ncol(prediction_matrix_r)){
    m_r <- m_r + (testY - prediction_matrix_r[,k])^2}
    mean_squared_error_r <- 1/(numofboot_r*20) * sum(m_r); mean_squared_error_r
    
    proof_r <- mean_squared_error_r- variance_r -bias_squared_r
    results_matrix_r[degree,] <- c(degree,mean_squared_error_r, bias_squared_r, variance_r, round(proof_r,8), lmb)
  }
  mseridge[[l]] <- results_matrix_r
}
mseridge[[10]] 



## -------------------------------------------------------------------------------------------------------------------------------------
lambdamses <- as.data.frame(matrix(NA, nrow = 5, ncol = numlambda+1))
lambdamses[,1] <- c(1:5)
colnames(lambdamses) <- c( "order",round(lambdas, 3))
rownames(lambdamses) <- c(1:5)
for (M in 1:length(mseridge)){
  ma <- mseridge[[M]]
  mse_ <- ma[,2]
  lambdamses[,M+1] <- mse_
}
lambdamses


## ----bootstrap plot-------------------------------------------------------------------------------------------------------------------
require(ggplot2)
ggplot()+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,2]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,2], colour="100"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,3]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,3], colour="35.938"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,4]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,4], colour="12.915"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,5]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,5], colour="4.642"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,6]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,6], colour="1.668"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,7]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,7], colour="0.599"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,8]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,8], colour="0.215"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,9]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,9], colour="0.077"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,10]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,10], colour="0.028"), size=1)+  geom_point(data=lambdamses, aes(x=order, y=lambdamses[,11]), size=2)+  geom_line(data=lambdamses, aes(x=order, y=lambdamses[,11], colour="0.01"), size=1)+  ylab("Mse test")+ggtitle("MSE test for different lambdas and degrees")


## -------------------------------------------------------------------------------------------------------------------------------------
numlambda <- 10
lambdas <- 10^(seq(-2,2,length=numlambda))

matrixlist_rid5 <- list()
 
for (l in 1:numlambda){
  lmb_rid5 <- lambdas[l]
  msematrix_cv_rid5 <- as.data.frame(matrix(NA, nrow = 5,4))
  colnames(msematrix_cv_rid5) <- c("used lambda","order", "test_cv", "train_cv")
  folds_rid5<-cut(seq(1,nrow(dataset)),breaks=5,labels=FALSE)
    
    for (degree in 1:5){
      
      msestest_rid5 <- c()
      msestrain_rid5 <- c()
      
      for(i in 1:5){ 
        
        testIndexes_cv_rid5 <- which(folds_rid5==i,arr.ind=TRUE)
        testData_cv_rid5 <- dataset[testIndexes_cv_rid5, ]
        trainData_cv_rid5 <- dataset[-testIndexes_cv_rid5, ]
        dataset_cv_rid5<-model_matrix(trainData_cv_rid5[,1],trainData_cv_rid5[,2],degree)
        beta_cv_rid5 <- betaridge(dataset_cv_rid5, trainData_cv_rid5[,3], lmb_rid5)
        predmse_cv_rid5 <- predictions(testData_cv_rid5[,1],testData_cv_rid5[,2],degree,beta_cv_rid5)
        msetest_cv_rid5 <- mean((testData_cv_rid5[,3] - predmse_cv_rid5)^2)
        predtrain_cv_rid5<-predictions(trainData_cv_rid5[,1],trainData_cv_rid5[,2],degree,beta_cv_rid5)
        msetrain_cv_rid5 <- mean((trainData_cv_rid5[,3] - predtrain_cv_rid5)^2)
        msestest_rid5[i] <- msetest_cv_rid5
        msestrain_rid5[i] <- msetrain_cv_rid5
      }
      
      msematrix_cv_rid5[degree,] <- c(lmb_rid5, degree, mean(msestest_rid5), mean(msestrain_rid5))
    }
    
    matrixlist_rid5[[l]] <- msematrix_cv_rid5
}
matrixlist_rid5[[1]]


## -------------------------------------------------------------------------------------------------------------------------------------
numlambda <- 10
lambdas <- 10^(seq(-2,2,length=numlambda))

matrixlist_rid10 <- list()
 
for (l in 1:numlambda){
  lmb_rid10 <- lambdas[l]
  msematrix_cv_rid10 <- as.data.frame(matrix(NA, nrow = 5,4))
  colnames(msematrix_cv_rid10) <- c("used lambda","order", "test_cv", "train_cv")
  folds_rid10<-cut(seq(1,nrow(dataset)),breaks=10,labels=FALSE)
  
    
    for (degree in 1:5){
      
      msestest_rid10 <- c()
      msestrain_rid10 <- c()
      
      for(i in 1:10){ 
        
        testIndexes_cv_rid10 <- which(folds_rid10==i,arr.ind=TRUE)
        testData_cv_rid10 <- dataset[testIndexes_cv_rid10, ]
        trainData_cv_rid10 <- dataset[-testIndexes_cv_rid10, ]
        dataset_cv_rid10<-model_matrix(trainData_cv_rid10[,1],trainData_cv_rid10[,2],degree)
        beta_cv_rid10 <- betaridge(dataset_cv_rid10, trainData_cv_rid10[,3], lmb_rid10)
        predmse_cv_rid10 <- predictions(testData_cv_rid10[,1],testData_cv_rid10[,2],degree,beta_cv_rid10)
        msetest_cv_rid10 <- mean((testData_cv_rid10[,3] - predmse_cv_rid10)^2)
        predtrain_cv_rid10<-predictions(trainData_cv_rid10[,1],trainData_cv_rid10[,2],degree,beta_cv_rid10)
        msetrain_cv_rid10 <- mean((trainData_cv_rid10[,3] - predtrain_cv_rid10)^2)
        msestest_rid10[i] <- msetest_cv_rid10
        msestrain_rid10[i] <- msetrain_cv_rid10
      }
      
      msematrix_cv_rid10[degree,] <- c(lmb_rid10, degree, mean(msestest_rid10), mean(msestrain_rid10))
    }
    
    matrixlist_rid10[[l]] <- msematrix_cv_rid10
}


## -------------------------------------------------------------------------------------------------------------------------------------
#CROSS VALIDATION WITH THE SAME TEST SET 

numlambda <- 10
lambdas <- 10^(seq(-2,2,length=numlambda))
Zmatrixlist_rid10 <- list()
ZtestData_cv_rid10 <- testX
trainingY <- cbind(trainingY)
rownames(trainingY) <- rownames(trainingX)

for (l in 1:numlambda){
  Zlmb_rid10 <- lambdas[l]
  Zmsematrix_cv_rid10 <- as.data.frame(matrix(NA, nrow = 5,4))
  colnames(Zmsematrix_cv_rid10) <- c("used lambda","order", "test_cv", "train_cv")
  Zfolds_rid10<-cut(seq(1,nrow(trainingX)),breaks=10,labels=FALSE)
  
    for (degree in 1:5){
      
      Zmsestest_rid10 <- c()
      Zmsestrain_rid10 <- c()
      
      for(i in 1:10){ 
        
        ZtestIndexes_cv_rid10 <- which(Zfolds_rid10==i,arr.ind=TRUE)
        datatrainX_ridge10 <- trainingX[-ZtestIndexes_cv_rid10, ]
        datatrainY_ridge10 <- trainingY[-ZtestIndexes_cv_rid10, ]
        Zdataset_cv_rid10<-model_matrix(datatrainX_ridge10[,1],datatrainX_ridge10[,2],degree)
        Zbeta_cv_rid10 <- betaridge(Zdataset_cv_rid10, datatrainY_ridge10, l)
        Zpredmse_cv_rid10 <- predictions(testX[,1],testX[,2],degree,Zbeta_cv_rid10)
        Zmsetest_cv_rid10 <- mean((testY - Zpredmse_cv_rid10)^2)
        Zpredtrain_cv_rid10<-predictions(datatrainX_ridge10[,1],datatrainX_ridge10[,2],degree,Zbeta_cv_rid10)
        Zmsetrain_cv_rid10 <- mean((datatrainY_ridge10 - Zpredtrain_cv_rid10)^2)
        Zmsestest_rid10[i] <- Zmsetest_cv_rid10
        Zmsestrain_rid10[i] <- Zmsetrain_cv_rid10
      }
      
      Zmsematrix_cv_rid10[degree,] <- c(Zlmb_rid10, degree, mean(Zmsestest_rid10), mean(Zmsestrain_rid10))
    }
    
    Zmatrixlist_rid10[[l]] <- Zmsematrix_cv_rid10
}
Zmatrixlist_rid10[[1]]
matrixlist_rid10[[1]]


## -------------------------------------------------------------------------------------------------------------------------------------
s5 <- as.data.frame(matrix(NA, nrow = 5, ncol = numlambda+1))
s5[,1] <- c(1:5)
colnames(s5) <- c( "order",round(lambdas, 3))
rownames(s5) <- c(1:5)
for (M in 1:length(matrixlist_rid5)){
  mt <- matrixlist_rid5[[M]]
  msete5 <- mt[,3]
  s5[,M+1] <- msete5}

bestlam_cv5 <- matrix(NA,5,1)
dimnames(bestlam_cv5) <- list(c("order 1","order 2","order 3","order 4","order 5"),"lambdas")
for (row in 1:5){
  lb_cv5 <- which.min(s5[row,])
  bestlam_cv5[row,] <- colnames(s5)[lb_cv5]
}
bestlam_cv5


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
require(glmnet)
lambdas <- 10^(seq(-2,2,length=numlambda))
p5 <- matrix(NA,5,2)
dimnames(p5) <- list(1:5, c("order", "best lambda"))
testfeatures <- list(testX_sc_1, testX_sc_2,testX_sc_3,testX_sc_4,testX_sc_5)
for (order in 1:5){
  mt <- get(paste("matrix_deg_sc",toString(order),sep=""))
  cv_ridge <- cv.glmnet(mt, scaled_trainingY, alpha = 0, lambda = lambdas, nfolds = 5)
  optimal_lambda <- cv_ridge$lambda.min
  p5[order,] <- c(order, optimal_lambda)
}
p5 


## -------------------------------------------------------------------------------------------------------------------------------------
s10 <- as.data.frame(matrix(NA, nrow = 5, ncol = numlambda+1))
s10[,1] <- c(1:5)
colnames(s10) <- c( "order",round(lambdas, 3))
rownames(s10) <- c(1:5)
for (M in 1:length(matrixlist_rid10)){
  mt <- matrixlist_rid10[[M]]
  msete10 <- mt[,3]
  s10[,M+1] <- msete10}

bestlam_cv10 <- matrix(NA,5,1)
dimnames(bestlam_cv10) <- list(c("order 1","order 2","order 3","order 4","order 5"),"lambdas")
for (row in 1:5){
  lb_cv10 <- which.min(s10[row,])
  bestlam_cv10[row,] <- colnames(s10)[lb_cv10]
}
bestlam_cv10


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
require(glmnet)
lambdas <- 10^(seq(-2,2,length=numlambda))
p10 <- matrix(NA,5,2)
dimnames(p10) <- list(1:5, c("order", "best lambda"))
for (order in 1:5){
  mt <- get(paste("matrix_deg_sc",toString(order),sep=""))
  cv_ridge <- cv.glmnet(mt, scaled_trainingY, alpha = 0, lambda = lambdas, nfolds = 10, type.measure="mse")
  optimal_lambda <- cv_ridge$lambda.min
  p10[order,] <- c(order, optimal_lambda)
  
    #cv_ridge$cvm[cv_ridge$lambda==cv_ridge$lambda.min] to show which is the minimum MSE
  #Note that with cv_ridge$lambda that cv.glmnet gives the lambdas in decreasing order, so from cv_ridge$cvm the last value is the one we have to look at, and it corresponds to the best value (0.01 in 4 out of 5 cases).

 }
p10


## -------------------------------------------------------------------------------------------------------------------------------------
require(glmnet)
pr <- matrix(NA,5,2)
dimnames(pr) <- list(1:5, c("order", "best lambda"))
for (order in 1:5){
  mt <- get(paste("matrix_deg",toString(order),sep=""))
  cv_ridger <- cv.glmnet(mt, trainingY, alpha = 0, nfolds = 10, type.measure="mse")
  par(mfrow=c(1,2))
  plot(cv_ridger)
  plot(cv_ridger$glmnet.fit, xvar="lambda", label=5)
  optimal_lambdar <- cv_ridger$lambda.min
  pr[order,] <- c(order, optimal_lambdar)}
pr


## -------------------------------------------------------------------------------------------------------------------------------------
library(glmnet)

pl <- matrix(NA,5,2)
dimnames(pl) <- list(1:5, c("order", "best lambda"))
for (order in 1:5){
  mt <- get(paste("matrix_deg",toString(order),sep=""))

  cv_model <- cv.glmnet(mt, trainingY, alpha = 1)
  best_lambda <- cv_model$lambda.min
  pl[order,] <- c(order, best_lambda)}
pl


## -------------------------------------------------------------------------------------------------------------------------------------
matt <- matrix_deg3
colnames(matt) <- c("int", "x1", "x2", "x1^2", "x2^2", "x1*x2", "x1^3", "x2^3", "(x1^2)*x2", "x1*(x2^2)")
cv_model3 <- cv.glmnet(matt, trainingY, alpha = 1)
plot(cv_model3)


## -------------------------------------------------------------------------------------------------------------------------------------
plot(cv_model3$glmnet.fit, xvar="lambda", label=5)


## -------------------------------------------------------------------------------------------------------------------------------------
d <- read.table("C:/Users/cmira/Desktop/ny.csv", 
                 header = TRUE,
                 row.names = 1,
                 sep = ",")
df <- d[,c(1,2,3)]


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
require(skimr)


## -------------------------------------------------------------------------------------------------------------------------------------
myskim2 <- skim_with(base = sfl(),
                     numeric = sfl(hist = NULL))
myskim2(df)


## -------------------------------------------------------------------------------------------------------------------------------------
df[which.max(df$price),]


## -------------------------------------------------------------------------------------------------------------------------------------
require(GGally)
ggpairs(df_complete, title="correlogram with ggpairs()")


## -------------------------------------------------------------------------------------------------------------------------------------
df2 <- as.data.frame(round(cbind(scaling(cbind(df$latitude)), scaling(cbind(df$longitude)), scaling(cbind(df$price))),6))
dimnames(df2) <- list(rownames(df), colnames(df))


## -------------------------------------------------------------------------------------------------------------------------------------
mysplitted <- train_test(df2[,c(1,2)],cbind(df2$price))

mytrainingX <- mysplitted[[1]]
mytrainingY <- mysplitted[[2]]
mytestX <- mysplitted[[3]]
mytestY <- mysplitted[[4]]

mymatrix1 <- model_matrix(mytrainingX[,1],mytrainingX[,2],1)
rownames(mymatrix1) <- translate(rownames(mytrainingX))
mytestX_1 <- as.data.frame(model_matrix(mytestX$V1,mytestX$V2,1))
mybeta1 <- betas(mymatrix1, mytrainingY)
mypred1 <- predictions(mytestX[,1],mytestX[,2],1,mybeta1)
mymse1 <- mean((mytestY - mypred1)^2)
myr21 <- 1 - sum((mytestY - mypred1)^2)/sum((mytestY - mean(mytestY))^2)

mymatrix2 <- model_matrix(mytrainingX[,1],mytrainingX[,2],2)
rownames(mymatrix2) <- translate(rownames(mytrainingX))
mytestX_2 <- as.data.frame(model_matrix(mytestX$V1,mytestX$V2,2))
mybeta2 <- betas(mymatrix2, mytrainingY)
mypred2 <- predictions(mytestX[,1],mytestX[,2],2,mybeta2)
mymse2 <- mean((mytestY - mypred1)^2)
myr22 <- 1 - sum((mytestY - mypred2)^2)/sum((mytestY - mean(mytestY))^2)

mymatrix3 <- model_matrix(mytrainingX[,1],mytrainingX[,2],3)
rownames(mymatrix3) <- translate(rownames(mytrainingX))
mytestX_3 <- as.data.frame(model_matrix(mytestX$V1,mytestX$V2,3))
mybeta3 <- betas(mymatrix3, mytrainingY)
mypred3 <- predictions(mytestX[,1],mytestX[,2],3,mybeta3)
mymse3 <- mean((mytestY - mypred3)^2)
myr23 <- 1 - sum((mytestY - mypred3)^2)/sum((mytestY - mean(mytestY))^2)

mymatrix4 <- model_matrix(mytrainingX[,1],mytrainingX[,2],4)
rownames(mymatrix4) <- translate(rownames(mytrainingX))
mytestX_4 <- as.data.frame(model_matrix(mytestX$V1,mytestX$V2,4))
mybeta4 <- betas(mymatrix4, mytrainingY)
mypred4 <- predictions(mytestX[,1],mytestX[,2],4,mybeta4)
mymse4 <- mean((mytestY - mypred4)^2)
myr24 <- 1 - sum((mytestY - mypred4)^2)/sum((mytestY - mean(mytestY))^2)

mymatrix5 <- model_matrix(mytrainingX[,1],mytrainingX[,2],5)
rownames(mymatrix5) <- translate(rownames(mytrainingX))
mytestX_5 <- as.data.frame(model_matrix(mytestX$V1,mytestX$V2,5))
mybeta5 <- betas(mymatrix5, mytrainingY)
mypred5 <- predictions(mytestX[,1],mytestX[,2],5,mybeta5)
mymse5 <- mean((mytestY - mypred5)^2)
myr25 <- 1 - sum((mytestY - mypred5)^2)/sum((mytestY - mean(mytestY))^2)


## -------------------------------------------------------------------------------------------------------------------------------------
myresults <- matrix(data=NA, nrow=2,ncol=5)
rownames(myresults)<-cbind("MSE","R2")
mynames<-c()
for (i in 1:5){
  mynames[i] <- paste("order", as.character(i))
}
colnames(myresults) <- mynames

myresults[1,] <- rbind(mymse1, mymse2, mymse3, mymse4, mymse5)
myresults[2,] <- rbind(myr21, myr22, myr23, myr24, myr25)

myresults


## -------------------------------------------------------------------------------------------------------------------------------------
mypredtrain1 <- predictions(mytrainingX[,1],mytrainingX[,2],1,mybeta1)
mymsetrain1 <- mean((mytrainingY - mypredtrain1)^2)
mypredtrain2 <- predictions(mytrainingX[,1],mytrainingX[,2],2,mybeta2)
mymsetrain2 <- mean((mytrainingY - mypredtrain2)^2)
mypredtrain3 <- predictions(mytrainingX[,1],mytrainingX[,2],3,mybeta3)
mymsetrain3 <- mean((mytrainingY - mypredtrain3)^2)
mypredtrain4 <- predictions(mytrainingX[,1],mytrainingX[,2],4,mybeta4)
mymsetrain4 <- mean((mytrainingY - mypredtrain4)^2)
mypredtrain5 <- predictions(mytrainingX[,1],mytrainingX[,2],5,mybeta5)
mymsetrain5 <- mean((mytrainingY - mypredtrain5)^2)

mymsematrix <- data.frame(1:5, rbind(mymse1,mymse2,mymse3,mymse4,mymse5),rbind(mymsetrain1,mymsetrain2,mymsetrain3,mymsetrain4,mymsetrain5))
colnames(mymsematrix) <- c("order", "test", "train")
rownames(mymsematrix) <- c(1:5)


## -------------------------------------------------------------------------------------------------------------------------------------
ggplot()+
  geom_point(data=mymsematrix, aes(x=order, y=test), size=2)+
  geom_line(data=mymsematrix, aes(x=order, y=test, colour="test"), size=1)+
  geom_point(data=mymsematrix, aes(x=order, y=train), size=2)+
  geom_line(data=mymsematrix, aes(x=order, y=train, colour="training"), size=1)+
  ylab("Mse")+ggtitle("Training MSEs vs Test MSEs")

## -------------------------------------------------------------------------------------------------------------------------------------
mymsematrix


## -------------------------------------------------------------------------------------------------------------------------------------
mynumfolds <- c(5,10)
mymatrixlist <- list()
mymsematrix_cv <- as.data.frame(matrix(NA, nrow = 5,3))
colnames(mymsematrix_cv) <- c("order", "test_cv", "train_cv")
 

for (k in 1:length(mynumfolds)){  
  myfolds<-cut(seq(1,nrow(df2)),breaks=mynumfolds[k],labels=FALSE)

  for (degree in 1:5){
    
    mymsestest <- c()
    mymsestrain <- c()
    
    for(i in 1:mynumfolds[k]){ 
      
      mytestIndexes_cv <- which(myfolds==i,arr.ind=TRUE)
      mytestData_cv <- df2[mytestIndexes_cv, ]
      mytrainData_cv <- df2[-mytestIndexes_cv, ]
      mydataset_cv<-model_matrix(mytrainData_cv[,1],mytrainData_cv[,2],degree)
      mybeta_cv <- betas(mydataset_cv, mytrainData_cv[,3])
      mypredmse_cv <- predictions(mytestData_cv[,1],mytestData_cv[,2],degree,mybeta_cv)
      mymsetest_cv <- mean((mytestData_cv[,3] - mypredmse_cv)^2)
      mypredtrain_cv<-predictions(mytrainData_cv[,1],mytrainData_cv[,2],degree,mybeta_cv)
      mymsetrain_cv <- mean((mytrainData_cv[,3] - mypredtrain_cv)^2)
      mymsestest[i] <- mymsetest_cv
      mymsestrain[i] <- mymsetrain_cv
    }
    
    mymsematrix_cv[degree,] <- c(degree, mean(mymsestest), mean(mymsestrain))
  }
  
  mymatrixlist[[k]] <- mymsematrix_cv
}
myfivefolds<- mymatrixlist[[1]]
mytenfolds <- mymatrixlist[[2]]

myp1 <- ggplot()+  geom_point(data=mymsematrix, aes(x=order, y=test), size=2)+  geom_line(data=mymsematrix, aes(x=order, y=test, colour="test"), size=1)+  geom_point(data=mymsematrix, aes(x=order, y=train), size=2)+  geom_line(data=mymsematrix, aes(x=order, y=train, colour="training"), size=1)+  ylab("Mse")+ggtitle("Tr MSE vs Te MSEs 1 sample")

myp2 <- ggplot()+  geom_point(data=myfivefolds, aes(x=order, y=test_cv), size=2)+  geom_line(data=myfivefolds, aes(x=order, y=test_cv, colour="test_cv"), size=1)+  geom_point(data=myfivefolds, aes(x=order, y=train_cv), size=2)+  geom_line(data=myfivefolds, aes(x=order, y=train_cv, colour="train_cv"), size=1)+  ylab("Mse")+ggtitle("Tr MSE vs Te MSE 5-FOLDS CV")

myp3 <- ggplot()+  geom_point(data=mytenfolds, aes(x=order, y=test_cv), size=2)+  geom_line(data=mytenfolds, aes(x=order, y=test_cv, colour="test_cv"), size=1)+  geom_point(data=mytenfolds, aes(x=order, y=train_cv), size=2)+  geom_line(data=mytenfolds, aes(x=order, y=train_cv, colour="train_cv"), size=1)+  ylab("Mse")+ggtitle("Tr MSE vs Te MSE 5-FOLDS CV")

grid.arrange(myp1, myp2, myp3, ncol = 2, nrow = 2)



## -------------------------------------------------------------------------------------------------------------------------------------
ridgematrix <- matrix(NA,5,3)
dimnames(ridgematrix) <- list(1:5, c("order", "best lambda", "mse"))
for (order in 1:5){
  mymt <- get(paste("mymatrix",toString(order),sep=""))
  mycv_ridge <- cv.glmnet(mymt, mytrainingY, alpha = 0, nfolds = 10)
  myridgemse <- min(mycv_ridge$cvm)
  myoptimal_lambda <- mycv_ridge$lambda.min
  ridgematrix[order,] <- c(order, myoptimal_lambda, myridgemse)
}
ridgematrix 


## -------------------------------------------------------------------------------------------------------------------------------------
lassomatrix <- matrix(NA,5,3)
dimnames(lassomatrix) <- list(1:5, c("order", "best lambda", "mse"))
for (order in 1:5){
  mymt <- get(paste("mymatrix",toString(order),sep=""))

  mycv_lasso <- cv.glmnet(mymt, mytrainingY, alpha = 1,  nfolds = 10)
  mylassomse <- min(mycv_lasso$cvm)
  mybest_lambda <- mycv_lasso$lambda.min
  lassomatrix[order,] <- c(order, mybest_lambda, mylassomse)
  }
lassomatrix


## -------------------------------------------------------------------------------------------------------------------------------------
final <- cbind(mytenfolds$test_cv, ridgematrix[,"mse"], lassomatrix[,"mse"])
colnames(final) <- c("OLS Mse", "Ridge Mse", "Lasso Mse")
final


## -------------------------------------------------------------------------------------------------------------------------------------
df_complete <- read.table("C:/Users/cmira/Desktop/AB_NYC_2019.csv", 
                 header = TRUE,
                 row.names = 1,
                 sep = ",")
df_complete <- df_complete[complete.cases(df_complete), ]
head(df_complete) 


## -------------------------------------------------------------------------------------------------------------------------------------
df_complete2 <- as.data.frame(round(cbind(scaling(cbind(df_complete$latitude)), scaling(cbind(df_complete$longitude)), scaling(cbind(df_complete$price)), scaling(cbind(df_complete$minimum_nights)), scaling(cbind(df_complete$number_of_reviews)), scaling(cbind(df_complete$reviews_per_month)), scaling(cbind(df_complete$calculated_host_listings_count)), scaling(cbind(df_complete$availability_365))),6))
dimnames(df_complete2) <- list(rownames(df_complete), colnames(df_complete))


## -------------------------------------------------------------------------------------------------------------------------------------
splitols <- train_test(df_complete2[,c(1,2,4,5,6,7,8)],cbind(df_complete2$price))
model <- lm(formula = splitols[[2]] ~ .-1, data = splitols[[1]])
mseols <- mean((splitols[[4]] - predict(model, newdata = splitols[[3]]))^2)


## -------------------------------------------------------------------------------------------------------------------------------------
Amynumfolds <- c(5,10)
Amylist <- list()
Amymsematrix_cv <- as.data.frame(matrix(NA, nrow = 5,3))
colnames(Amymsematrix_cv) <- c("order", "test_cv", "train_cv")
 

for (k in 1:length(Amynumfolds)){  
  Amyfolds<-cut(seq(1,nrow(df_complete2)),breaks=Amynumfolds[k],labels=FALSE)
    
    Amymsestest <- c()
    Amymsestrain <- c()
    
    for(i in 1:Amynumfolds[k]){ 
      
      AmytestIndexes_cv <- which(Amyfolds==i,arr.ind=TRUE)
      AmytestData_cv <- df_complete2[AmytestIndexes_cv, ]
      testY_cv <- cbind(AmytestData_cv[,3])
      testX_cv <- AmytestData_cv[,-3]
      AmytrainData_cv <- df_complete2[-AmytestIndexes_cv, ]
      trainY_cv <- cbind(AmytrainData_cv[,3])
      trainX_cv <- AmytrainData_cv[,-3]
      mod <- lm(trainY_cv ~.-1, data = trainX_cv)
      Apredtrain_ <- predict(mod)
      Apredtest_ <- predict(mod, newdata = testX_cv)
      Amymsetest_cv <- mean((testY_cv - Apredtest_)^2)
      Amymsetrain_cv <- mean((trainY_cv - Apredtrain_)^2)
      Amymsestest[i] <- Amymsetest_cv
      Amymsestrain[i] <- Amymsetrain_cv
    }
    
      Amylist[[k]] <- c(mean(Amymsestest), mean(Amymsestrain))
}
fivefolds_complete<- Amylist[1]
tenfolds_complete <- Amylist[2]

fivefolds_complete; tenfolds_complete



## -------------------------------------------------------------------------------------------------------------------------------------
X = model.matrix(price ~ .-1, data = df_complete2)
ridge_complete <- cv.glmnet(X, df_complete2$price, alpha = 0, nfolds = 10)
ridgecomp_mse <- min(ridge_complete$cvm)
ridgecom_optimal_lambda <- ridge_complete$lambda.min
ridgeresults <- c(ridgecom_optimal_lambda, ridgecomp_mse)

ridgeresults 


## -------------------------------------------------------------------------------------------------------------------------------------
X = model.matrix(price ~ .-1, data = df_complete2)
lasso_complete <- cv.glmnet(X, df_complete2$price, alpha = 1, nfolds = 10)
lassocomp_mse <- min(lasso_complete$cvm)
lassocomp_optimal_lambda <- lasso_complete$lambda.min
lassoresults <- c(lassocomp_optimal_lambda, lassocomp_mse)
lassoresults 


## -------------------------------------------------------------------------------------------------------------------------------------
finalcomparisons <- cbind(tenfolds_complete[[1]][1], ridgeresults[2], lassoresults[2])
colnames(finalcomparisons) <- c("cv 10folds", "ridge", "lasso");
finalcomparisons


## -------------------------------------------------------------------------------------------------------------------------------------
plot(ridge_complete $glmnet.fit, "lambda")

## -------------------------------------------------------------------------------------------------------------------------------------
plot(lasso_complete $glmnet.fit, "lambda")


## -------------------------------------------------------------------------------------------------------------------------------------
nomi <- c(colnames(df_complete)[-3])
df_noprice <- df_complete[,-3]


## -------------------------------------------------------------------------------------------------------------------------------------
n <- 1
newdf1 <- cbind.data.frame(df_complete$price, poly(as.matrix(df_noprice), degree = n, raw = TRUE))
store1 <- colnames(newdf1)
names(newdf1)[-1]<-paste0("x",c(1:7))
names(newdf1)[1]<-"prezzo"

newdf21<- round(as.data.frame(scale(newdf1)),3)

split1 <- train_test(newdf21[,c(2,3,4,5,6,7,8)],cbind(newdf21$prezzo))
trainx1 <- split1[[1]]
trainy1 <- split1[[2]]
testx1 <- split1[[3]]
testy1 <- split1[[4]]

polylm1 <- lm(trainy1 ~ ., trainx1)
prd1 <- predict(polylm1, newdata = testx1)
msee1 <- mean((testy1 - prd1)^2)
msee1


## -------------------------------------------------------------------------------------------------------------------------------------
n <- 2
newdf2 <- cbind.data.frame(df_complete$price, poly(as.matrix(df_noprice), degree = n, raw = TRUE))
store2 <- colnames(newdf2)
names(newdf2)[-1]<-paste0("x",c(1:35))
names(newdf2)[1]<-"prezzo"

newdf22<- as.data.frame(scale(newdf2))
split2 <- train_test(newdf22[,c(2:35)],cbind(newdf22$prezzo))
trainx2 <- split2[[1]]
trainy2 <- split2[[2]]
testx2 <- split2[[3]]
testy2 <- split2[[4]]

polylm2 <- lm(trainy2 ~ ., trainx2)
prd2 <- predict(polylm2, newdata = testx2)
msee2 <- mean((testy2 - prd2)^2)
msee2

## -------------------------------------------------------------------------------------------------------------------------------------
n <- 3
newdf3 <- cbind.data.frame(df_complete$price, poly(as.matrix(df_noprice), degree = n, raw = TRUE))
store3 <- colnames(newdf3)
names(newdf3)[-1]<-paste0("x",c(1:119))
names(newdf3)[1]<-"prezzo"

newdf23<- as.data.frame(scale(newdf3))
split3 <- train_test(newdf23[,c(2:119)],cbind(newdf23$prezzo))
trainx3 <- split3[[1]]
trainy3 <- split3[[2]]
testx3 <- split3[[3]]
testy3 <- split3[[4]]

polylm3 <- lm(trainy3 ~ ., trainx3)
prd3 <- predict(polylm3, newdata = testx3)
msee3 <- mean((testy3 - prd3)^2)
msee3


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
n <- 4
newdf4 <- cbind.data.frame(df_complete$price, poly(as.matrix(df_noprice), degree = n, raw = TRUE))
store4 <- colnames(newdf4)
names(newdf4)[-1]<-paste0("x",c(1:329))
names(newdf4)[1]<-"prezzo"

newdf24<- as.data.frame(scale(newdf4))
split4 <- train_test(newdf24[,c(2:329)],cbind(newdf24$prezzo))
trainx4 <- split4[[1]]
trainy4 <- split4[[2]]
testx4 <- split4[[3]]
testy4 <- split4[[4]]

polylm4 <- lm(trainy4 ~ ., trainx4)
prd4 <- predict(polylm4, newdata = testx4)
msee4 <- mean((testy4 - prd4)^2)
msee4


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
n <- 5
newdf5 <- cbind.data.frame(df_complete$price, poly(as.matrix(df_noprice), degree = n, raw = TRUE))
store5 <- colnames(newdf5)
names(newdf5)[-1]<-paste0("x",c(1:791))
names(newdf5)[1]<-"prezzo"

newdf25<- as.data.frame(scale(newdf5))
split5 <- train_test(newdf25[,c(2:791)],cbind(newdf25$prezzo))
trainx5 <- split5[[1]]
trainy5 <- split5[[2]]
testx5 <- split5[[3]]
testy5 <- split5[[4]]

polylm5 <- lm(trainy5 ~ ., trainx5)
prd5 <- predict(polylm5, newdata = testx5)
msee5 <- mean((testy5 - prd5)^2)
msee5

## -------------------------------------------------------------------------------------------------------------------------------------
cbind(msee1,msee2,msee3,msee4,msee5)


## -------------------------------------------------------------------------------------------------------------------------------------
#step(polylm3, direction = “backward”) THIS IS COMMENTED OUT BECAUSE IT TAKES TOO LONG (25MIN)


## -------------------------------------------------------------------------------------------------------------------------------------
finalpoly <- lm(trainy3 ~ x1 + x2 + x3 + x4 + x6 + x8 + x9 + x11 + 
    x12 + x13 + x14 + x15 + x16 + x18 + x19 + x20 + x22 + x23 + 
    x24 + x25 + x27 + x28 + x30 + x32 + x34 + x41 + x42 + x44 + 
    x45 + x46 + x47 + x49 + x50 + x52 + x53 + x54 + x56 + x58 + 
    x59 + x60 + x61 + x62 + x65 + x66 + x67 + x71 + x72 + x73 + 
    x74 + x75 + x77 + x78 + x79 + x80 + x82 + x83 + x84 + x85 + 
    x86 + x87 + x88 + x89 + x90 + x92 + x93 + x96 + x98 + x100 + 
    x101 + x102 + x104 + x105 + x107 + x108 + x111 + x112 + x114 + 
    x116 + x117, data = trainx3)
prdfin <- predict(finalpoly, newdata = testx3)
mseefin <- mean((testy3 - prdfin)^2)
mseefin


## -------------------------------------------------------------------------------------------------------------------------------------
bestvariables3 <- c(1,  2,  3,  4,  6,  8,  9,  11,  12,  13, 14,15,16,18 ,19,20 ,22 ,23,24 ,25,27,28,30,32,34 ,41,42,44,45,46,47,49,50 ,52,53,54 ,56 ,58 ,59 ,60,61,62 ,65 ,66 ,67 ,71 ,72 ,73 ,74 ,75,77,78 ,79 ,80 ,82 ,83 ,84 ,85 ,86 ,87 ,88 ,89 ,90 ,92 ,93,96,98,100,101 ,102 ,104,105 ,107 ,108,111,112 ,114,116 ,117)
store3[bestvariables3+1]


## -------------------------------------------------------------------------------------------------------------------------------------
var <- c("x1", "x2" ,"x3" ,"x4" ,"x6" ,"x8" ,"x9", "x11" ,"x12" ,"x13", "x14", "x15" ,"x16", "x18" ,"x19" ,"x20" ,"x22" ,"x23","x24", "x25" ,"x27", "x28","x30", "x32" ,"x34" ,"x41" ,"x42" ,"x44","x45", "x46" ,"x47", "x49" ,"x50", "x52" ,"x53" ,"x54" ,"x56" ,"x58",     "x59", "x60" ,"x61", "x62" ,"x65", "x66" ,"x67" ,"x71" ,"x72" ,"x73",     "x74", "x75", "x77", "x78" ,"x79", "x80" ,"x82" ,"x83" ,"x84" ,"x85",     "x86", "x87", "x88", "x89" ,"x90", "x92", "x93" ,"x96" ,"x98" ,"x100" ,  "x101" ,"x102" ,"x104" ,"x105", "x107" ,"x108" ,"x111" ,"x112", "x114",    "x116" ,"x117")

finalridge <- cv.glmnet(as.matrix(trainx3[,var]), trainy3, alpha = 0, nfolds = 10)
final_ridge_opt_lambda <- finalridge$lambda.min
ridge_pred <- predict(finalridge, s = final_ridge_opt_lambda, newx = as.matrix(testx3[,var]))
msefinridge <- mean((ridge_pred - testy3)^2)


## -------------------------------------------------------------------------------------------------------------------------------------
finallasso <- cv.glmnet(as.matrix(trainx3[,var]), trainy3, alpha = 1, nfolds = 10)
final_lasso_opt_lambda <- finallasso$lambda.min
lasso_pred <- predict(finallasso, s = final_lasso_opt_lambda, newx = as.matrix(testx3[,var]))
msefinlasso <- mean((lasso_pred - testy3)^2)


## -------------------------------------------------------------------------------------------------------------------------------------
mseefin; msefinridge; msefinlasso

