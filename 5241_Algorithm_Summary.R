# 5241 Machine Learning Algorithm

# index subset
test=test[test$y%in%c(3,5,8),] # selected specified values

# Ch3 PCA-----------------------------------------------------------------------------------------------------------------------------------------------
# generate multi normal
z<-matrix(rnorm(n*p),nc=2) # generate N(0,I)
mu<-c(5,8) # set mean
Sig<-matrix(c(2,0.9,0.9,1),nr=2) # set covariance matrix
V<-svd(Sig)$v # eigenvector
rD<-diag(svd(Sig)$d) # square root of D
m<-matrix(mu,nr=n,nc=p,byrow=T) # create nxp matrix m, each row is mu
x<-z%*%rD%*%t(V)+m # transform z into x~N(mu,Sig)

# visualize the zipcode data
image(sym_digit, col= gray((8:0)/8), axes=FALSE)

# PCA
# project the images onto the subspace spanned by the first two principle components
scaled.data <- scale(data, center=TRUE, scale=FALSE) # scale the data
pca <- svd(scaled.data)
par(mfrow=c(1,1), mai=c(0.6, 0.6, 0.6, 0.6))
plot(pca$d[1]* pca$u[,1], pca$d[2]* pca$u[, 2],pch=16, xlab="First PC", ylab="Second PC" ) # first two columns of ud (scores)
# screeplot
plot(seq(from=1,to=256, by=1), (pca$d)^2/sum((pca$d)^2), xlab="Priciple componnets", ylab="Proportion of variance explained", pch=16)

# used train data pca to predict test data
prin_comp=prcomp(train_data, scale. = T)
predict(prin_comp, newdata =test_data )

# Ch4 Classification------------------------------------------------------------------------------------------------------------------------------------
# K means clustering
km.out <- kmeans(data, 2, nstart=50)

# Ch5 Linear Methods------------------------------------------------------------------------------------------------------------------------------------
# Multi-regression
lm.fit <- lm(Sales ~ .+Income:Advertising+Price:Age,data=dat) # A:B interaction term of A and B
lm.fit <- lm(Y ~ A*B,data=) # shorthand for lm(Y ~ A+B+A:B)
lm.fit2 <- lm(Y ~ X+I(X^2)) # predictor transformation

main=paste("Random sample", format(i)) # put iterator i in the text

# sample confidence band/confidence interval
plot.fit <- predict(model, x, level=0.95, interval="confidence", se.fit=T)
lines(plot.x$inc, plot.fit$fit[,1]+ww*plot.fit$se.fit, col=2, lty=2)
lines(plot.x$inc, plot.fit$fit[,1]-ww*plot.fit$se.fit, col=2, lty=2)

contrasts(x) # returns the coding R uses for the dummy variables

# Ch6 Classification------------------------------------------------------------------------------------------------------------------------------------
# Logistic Regression
train <- sample.int(nrow(data), 100)
glm.fit <- glm(y~x1+x2,family=binomial,data=dat,subset=train)
abline(a=-glm.fit$coefficients[1]/glm.fit$coefficients[3], b=-glm.fit$coefficients[2]/glm.fit$coefficients[3]) # boundary line for two variables

# predictions
glm.probs <- predict(logistic.model, iris[-train,], type="response") # predict the probability directly by type="response"
glm.pred <- glm.probs>0.5
table(Y[-train], glm.pred)

# Linear Discriminant Analysis
library(MASS)
lda.model <- lda(Y~x1+x2, data=dat, subset=train)
lda.pred <- predict(lda.model, iris[-train,])
table(Y[-train], lda.pred$class)


# Ch9 Cross Validation----------------------------------------------------------------------------------------------------------------------------------
# set test and train index
train <- sample(1:n, n*0.7)
test <- c(1:n)[-train] # 30% as test

# function to calculate loss
cv_error <- function(fitted_model,newy,newx){
  pred <- predict(fitted_model, newx = newx)
  error <- colMeans((pred-newy)^2) # L2 loss
  return(error)
}

# K-fold cross validation index
# set K=N, leave one out(LOO) CV
for( j in 1:k){
  validation_index <- c((j-1)*(length(train)%/%k)+1:(length(train)%/%k)) # ':' has priority over '+' operator
  train_index <- c(1:length(train))[-validation_index]
}

# Bootstrap
library(boot)
results <- boot(data=data.frame(as.matrix(nuclear)), statistic=p_v_estimate, R=1000) # statistic can be a defined function
results$t0 # result statistic of data
results$t # matrix of R rows statistics results of bootstraps

# Ch12 Shrinkage----------------------------------------------------------------------------------------------------------------------------------------
# glmnet model
# lambda here can be a vector(output predict will be a vector too)!
library(glmnet)
lasso.mod <- glmnet(xtrain, ytrain, alpha = 1, lambda=lambda) # alpha=0 ridge, alpha=1 lasso

# L1 penalty example
lasso.fit<-glmnet(as.matrix(votes[train, -1]), party_N[train], family = "binomial") # alpha=1 by default
lasso.cv<-cv.glmnet(as.matrix(votes[train, -1]), party_N[train],family = "binomial", type.measure = "class")
lasso.coef <- predict(lasso.fit, type = "coefficients", s = lasso.cv$lambda.1se )

# L2 penalty example
ridge.fit<-glmnet(as.matrix(votes[train, -1]), party_N[train], alpha = 0, family = "binomial")
ridge.cv<-cv.glmnet(as.matrix(votes[train, -1]), party_N[train],alpha=0, family = "binomial", type.measure = "deviance")
ridge.coef <- predict(ridge.fit, type = "coefficients", s = ridge.cv$lambda.1se )

