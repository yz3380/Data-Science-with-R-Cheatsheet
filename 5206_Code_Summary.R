# 5206 Final Review

# fix decode problem
Sys.setlocale("LC_ALL","English")

# Session-> Set Workin Directory-> Choose Directory getwd();setwd()

# Week1----------------------------------------------------------------------------------------------------------------------
  
# create a null numeric vector with length 3 
x<-vector("numeric",length=3)
# variable types, mode(x) as.numeric(x);as.integer(x);as.character(x);as.logical(x);as.complex(x);typeof(x)
  
# data types:
# vectors(same mode), scalars(one element vectors), matrices(two dimension, same mode), arrays(more than two dimensions),
# lists(like a vector, but different modes), dataframes(like matrix, but different modes)

# build a matrix
mat<-matrix(1:9, nrow=3, ncol=3, byrow=FALSE)
mat[1,2]<-round(sqrt(27),3)
colnames(mat)<-c("col1","col2") # name columns of a matrix, also rownames()
identity<-diag(3) # diagonal matrix
locator(n=1) # find a place on the plot

# numeric matrices
A %*% B # matrix multiplication
solve(A,b) # solve Ax=b
solve(A) # inverse of A

# logical operators
which(c(0,1,4)<3) # return indices when logical is TRUE
x[x>3] # extract element of x greater than 3

# NA and NULL values
mean(x,na.rm=TRUE) # calculate mean for values other than NA
x<-NULL # define an empty vector

# list 
lst<-list(name1=object1,name2=object2)
lst$c
lst[[i]] # ith component lst[["c"]]
names(lst)

# Week2----------------------------------------------------------------------------------------------------------------------

split(vector/dataframe,list(factor,vector2)) # return is a list
table(vector,factor) # produce contingency tables, can use matrix operations
data<-data.frame(c1=x,c2=y,c3=z, row.names=xx, stringsAsFactors = FALSE) head(data,2)
data<-rbind(data,new_row) # add rows to a dataframe
data$new<-new_row # add new column

# importing data
read.table("data.txt",header=F,as.is=T,row.names=1,col.names=c(),sep='') # set first column to row names
read.csv("data.csv", header=F, as.is=T)
HC<-scan("HonorCode.txt", sep="\n")
HC<-factor(HC,levels=unique(HC))

findwords<-function(text_fac){ # return a list of location of each levels
  words<-split(1:length(text_fac),text_fac) # text_fac is a factor here
  return(words)
}

alphabetized_list<-function(wordlist){ # sort word in Alphabetical order
  nms<-names(wordlist)
  sorted<-sort(nms)
  return(worldlist[sorted])
}

ifelse(test_logical, value_TRUE, value_FALSE)
iris$Versicolor<-ifelse(iris$Species=="versicolor",1,0)

# apply family commands
apply(matrix,2,sum) # 1:row sum, 2:column sum
lapply(list,mean) # returns a list
unlist(lapply(list,mean)) # unlist into a vector
order(x) # returns a vector of indices will permute its argument into sorted order
tapply(vector,factor,fcn)
sapply(vector,fcn, ...) 

# Week3----------------------------------------------------------------------------------------------------------------------
 
# training data: train our method how to estimate the model
# test set: test how well the model is at prediction
  
# least square estimation
x<-cbind(rep(1,nrow(x)),x1,x2,x3)
beta_hat<-solve((t(x)%*%x))%*%t(x)%*%y # beta=(X'X)^-1X'Y
round(t(beta_hat),2)
fit<-lm(y~x1+x2+x3,data=data) # will get the same result

# bootstrap
B<-1000
estimates<-vector(length=B)
for(b in 1:B){
  new_sample<-sample(vec,size=n,replace=TRUE)
  estimates[b]<-mean(new_sample)
}
var(estimates) # estimate the variance by bootstrap
L<-2*mean(vec)-quantile(estimates,.975) # regular 95% bootstrap lower interval
U<-2*mean(vec)-quantile(estimates,.025)
Lstar<-quantile(estimates,.025) # percentile based 95% bootstrap lower interval

# Week4a---------------------------------------------------------------------------------------------------------------------
  
# exploratory data analysis
par(mfrow=c(1,2), pty="s") # multiple graphs, set pty="s" to create square plots
default<-par() # clear par in console
par(default)

barplot(height,names.arg) # for categorical(discrete) variavle
diamonds$cut<-factor(diamonds$cut,levels=c('a','b','c'))
barplot(height=table(diamonds$cut),names.arg=names(table(diamonds$cut)))
hist(diamonds$carat,breaks=100) # for continuous variable, break is the total number of bins
boxplot(values~group) # visualize continuous variable over categorical variable, use '~' here!
plot(y~x) # continuous v.s. continuous, same as plot(x,y)

small_diam<-diamonds[sample(1:rows,1000),] # randomly selecting 1000 rows 
plot(log(x),log(y),col=z) # z is a factor here, ploted in different colors
legend("bottomright", legend=levels(z), fill=1:length(levels(z)), cex=.5) # plot with legend
abline(intercept, slope) # add a straight line
abline(fit)
points(xloc,yloc,label=,pch=,col=) # add points
text(xloc,yloc,label=,cex=)

# Week4b--------------------------------------------------------------------------------------------------------------------

# character & string operation nchar() # number of characters
cat() # printing strings with arguments coerced
substr(string, start=1, stop=3) substr(c("Columbia","slumberparty","sugarplum"),c(3,2,7),c(5,4,9)) # can apply to vectors
strsplit(string, split='') #return is a list
paste(vector1, vector2, sep="", collapse="") # condense multiple strings

# search strings
HC<-readLines("HonorCode2.txt") HC<-paste(HC,collapse=' ')
HC.words<-strsplit(HC,split=' ')[[1]] # make readLines result a vector of each word
word_count<-table(HC.words) # use table to count words, can be sorted using sort()
grep(pattern, x, value=F, invert=F) # return vector indices(line numbers)
grepl(pattern, x) # return vector of logical values

# regular expressions
# [a-z], [[:punct:]] # all punctuations
# [[:space:]] # all space characters including tab, space, newline
# [^aeiou] # negates the range in brackets
# . stands for any character, "c..s"
# +[1,inf), *[0,inf), ?[0,1], {n}, {n,}, {n,m}
# quantifiers only apply to the last character before, (abc){3}
# \\., \\n, \\s
# [a-z,]$ # ending with a-z and comma
# ^[^A-Z] # not beginning with capital letters

regexpr(pattern, x) # return only the first location
gregexpr(pattern, x) # return a list of all the locations
regmatches(x, gregexpr(pattern, x)) # return a list of all the match contents
gsub(pattern, replacement, x) # replace string content

coords_split<-sapply(coords, strplit, split="\\s+") # use sapply to split a list
coords_mat<-matrix(unlist(coords_split), ncol=2, byrow=T) # unpack list into a matrix 

# Week5---------------------------------------------------------------------------------------------------------------------

# interfaces are the places where the function interacts with the global environment: at the inputs and the outputs
stopifnot(length(x)==1, y>0) # raise error if any series of arguments is FALSE
curve(func, from=-1,to=1, add=TRUE) # plot a curve, added to the original plot
curve(2*x^3,add=,col=)

rm(list=ls()) # clear R global environments
all(a==b) # compare two vectors

# gradient descent
est.scaling.exponent<-function(beta, beta_0=6611, response=y, predictor=x,
                               max.iter=100,stop.deriv=.01,deriv.step=.001,
                               step.scale=1e-15){
  iter<-0
  deriv<-Inf
  sse<-function(b){sum((response-beta_0*predictor^b)^2)}
  for (i in 1:max.iter){
    iter<-iter+1
    deriv<-(sse(beta+deriv.step)-sse(beta))/deriv.step
    beta<-beta-deriv*step.scale
    if(abs(deriv)<stop.deriv){break}
  }
  fit<-list(beta=beta, iteration=iter, converged=(iter<max.iter))
  return(fit)
}

# classification
# a good classifier is one for which the test error is the smallest

# KNN
plot(x1,x2,col=color)
legend("bottomright", legend=levels(color), col=1:length(levels(color)), pch=1) # legend example

KNN.decision<-function(lag1.new,lag2.new,k=5,lag1=data$lag1,lag2=data$lag2,dir=data$direction){
  n<-length(lag1)
  # stopifnot(length(lag2)==n, length(lag1.new)==1, length(lag2.new)==1, k<=n)
  dists<-sqrt((lag1.new-lag1)^2+(lag2.new-lag2)^2)
  neighbors<-order(dists)[1:k] 
  neighb.dir<-dir[neighbors]
  choice<-names(which.max(table(neighb.dir))) # find the choice of KNN
  return(choice)
}

# multi knn with lagnew a vector
lagnew<-unlist(lagnew) # when use a vector directly from a dataframe, always unlist before iterate

# Week6---------------------------------------------------------------------------------------------------------------------
  
italy.strikes<-subset(strikes, country=='Italy', select) # get a sub dataset

# split and combine a dataset
my.strike.lm<-function(country.df){
return(lm(strike.volume~left.parliament, data=country.df)$coeff)
}
strikes.split<-split(strikes, strikes$country)
strike.coeff<-sapply(strikes.split, my.strike.lm) # return a matrix

par(mfrow=c(1,1),mar=c(8,2,1,1)) # mar stands for margins on each side of the plot, default is c(5.1, 4.1, 4.1, 2.1)
plot(1:ncol(strike.coeff), strike.coeff[2,], xaxt="n", xlab, ylab, main) # customize axis
axis(side=1, at=1:ncol(strike.coef), labels=colnames(strike.coef),las=2) # draw your own x-axis
abline(h=0, col='grey')
plot(x,y,xlab=,ylab=,main=,cex=  ,cex.axis=  ,las=  ) # use cex= and cex.axis= to zoom the shapes

# compute average in different years and plot the moving average
three.mean<-function(df){
return(apply(df[,c("unemployment","inflation","strike.volume")],2,mean))
}
years.split<-split(strikes,strikes$year)
years.mat<-sapply(years.split,three.mean)

plot(colnames(years.mat), years.mat[1,], xlab, ylab, type='l', ylim=range(years.mat[1,]))
points(colnames(years.mat), years.mat[2,], type='l', col='red')
legend("topleft", legend=c("unemployment","inflation"), fill=c("black","red"), cex=.5)


# plyr package
# **ply with input "adl", output "adl_" 
library(plyr)
# a*ply(.data, .margin, .func) 
aaply(my.array, 2:3, sum) # cut along col&3rd plane, get sum of vertical values.
# l*ply(.data, .func)
llply(my.list, summary)

par(mfrow=c(3,3), mar=c(4,4,1,1))
a_ply(my.array, 2:3, plot) # no return value

# d*ply(.data, .variable, .func, .parallel=FALSE) # use .variable to split the dataframe
strike.coeff<-sapply(strikes.split, my.strike.lm)
strike.coef.a<-daply(strikes, .(country), my.strike.lm) # different dimensions compared to result of sapply()
strike.coef.75<-ddply(strikes, .(country, yearPre1975), my.strike.lm) # split on two or more variables
strike.coef.75<-ddply(strikes, .(country, I(year<=1975)), my.strike.lm)
df<-ddply(df, .(factor), transform, newcol=func(col1, col2)) # add a column to dataframe by ddply

Rating.test<-sapply(ryear,kNN.regression,k=7) # apply function to vecter/list/dataframe using sapply

# Midterm---------------------------------------------------------------------------------------------------------------------

# multiple plot and parameters
myplot<-function(apps.subset) {
plot(apps.subset$Price, apps.subset$Rating, xlab="Price", ylab="Rating", pch=16,lwd=0.8,
     main=unique(apps.subset$Category),cex=0.8,cex.axis=0.5,cex.main=0.6,cex.lab=0.5, tcl=-0.2,mgp=c(0.6,0.05,0.0))
}
par(mfrow=c(6,6), mai=c(0.2, 0.15, 0.15, 0.15))
d_ply(apps,.(Category),myplot) # _ply means no return value

# Week8------------------------------------------------------------------------------------------------------------------------

# tibble, no row names, never changes type of inputs, e.g. convert factor
library(tibble)
as_tibble(head(iris,3))
df<-tibble(x=1:3,y=sample(seq(1,5,by=2),3),z=rnorm(3))

# map
# return list: map()
# return vector: map_int(), map_lgl(), map_dbl(), map_chr()
# return data frames(tibble): map_dfr(), map_dfc() 
map_dbl(1:3,log)
library(purrr) # purrr pipeline
1:3%>%map_dbl(~log(.)+sin(.))

library(dplyr)
strike.coef<-strikes%>%split(.$country)%>%map(~lm(strike.volume~left.parliament,data=.))%>%map_dfc(coef)
mtcars%>%split(.$cyl)%>%map(~lm(mpg~wt,data=.))%>%map(summary)%>%map_dbl("r.squared")

# reshaping using melt(),dcast(),acast()
require(reshape2)
snoq.melt<-melt(snoq, id.vars="year", variable.name="day", value.name= "precip")
# snoq.cast<-dcast(snoq.melt,year~variable)
snoq.melt<-na.omit(snoq.melt) # delete rows contains a missing data

# ggplot
library(ggplot2)
ggplot(data=mpg) # color inside aes:class, outside aes: whole
+geom_point(mapping=aes(x=displ,y=hwy,color/alpha/size=class)) # aplha=1/10, set transparency for heavy data
+facet_wrap(~class, nrow=2) # split the ggplot
+facet_grid(drv~class) # facet on two variables
+labs(title="", x="", y="")
# geom_bar/text/boxplot/abline
geom_path/geom_line(mapping=aes(x=x,y=y,col="z")) # path:order in data, line: order in x
geom_smooth(mapping=aes(x=Sepal.Length, y=Petal.Length, color=Species),method="lm",formula=y~x)
# ggplot(data=df1, aes(x, y)) + geom_point() + geom_path()

# ggplot histogram
ggplot(data.frame(x=x))+geom_histogram(mapping=aes(x=x,y=..density..),col="blue",bins=50)
+xlim(45,90)+stat_function(fun=mixture,col="green")

# Week9-----------------------------------------------------------------------------------------------------------------------
  
# LCG generator seed<-10
new.random<-function(a=5,c=12,m=16){
  out<-(a*seed+c)%%m
  seed<<-out # assign a global variable
  return(out) # return(out/m) as uniform
}

# plot t distributions with different df
t<-seq(-10,10,by=.01)
df<-c(1,2,5,30,100)
plot(t,dnorm(t),lty=1,col="red",ylab="f(t)",main="student's t") 
for (i in 1:5){
  lines(t,dt(t,df=df[i]),lty=i) # lines can only be added to a original plot
}
legend<-c(paste("df=",df,sep=""),"N(0,1)")
legend("topright",legend=legend,lty=c(1:5,1),col=c(rep(1,5),2))

# sample with probability
n<-1000
p<-c(0.1,0.2,0.7)
x<-sample(1:3,size=n,prob=p,replace=T)

rolls<-ceiling(runif(100,0,6)) # simulate a dice, use floor() for lower integer table(rolls)

# inverse transform for exponential
finverse<-function(u,lambda){
  return(ifelse((u<0|u>1),0,-(1/lambda)*log(1-u)))
}
x<-finverse(runif(1000),2)
hist(x,prob=T,breaks=15)
y<-seq(0,10,.01)
lines(y,2*exp(-2*y),col="blue",lty=2) # add another line

# acceptance-rejection method
# 1. Sample Y~g, e=g/alpha>f where alpha<=min(g/f)
# 2. Sample U~Unif(0,1)
# 3. If U<f(Y)/e(Y), accept Y
n.samps<-1000
n<-0
samps<-numeric(n.samps)
while(n<n.samps){
  y<-runif(1) # use uniform as envelope g
  u<-runif(1)
  if(u<f(y)/e(y)){
    n<-n+1
    samps[n]<-y
  }
}

# monte carlo integration, (importance sampling!)
# to estimate integration of g(x), draw x from p and take sample mean of f(x)=g(x)/p(x)
g.over.p<-function(x){
  return(sqrt(2*pi)*x^2*exp(-0.5*x^2)) # return g(x)/p(x), p is density function of standard normal 
}
mean(g.over.p(rnorm(10000)))

# simulate Pr(X<3) in exponential
mean(rexp(10000,rate=1/3)<3)

# generate binomial(n=10,p=0.3)
R<-1000;n<-10
binom<-NULL
for (i in 1:R){
  u<-runif(n)
  binom[i]<-sum(u<0.3)
}

# common distributions from U~unif(0,1)
# Ber(p): X=I(U<p), Binomial(n,p) is sum of n Bernoulli(p)
# Unif(a,b): X=a+(b-a)U
# Cauchy(a,b): X=a+b*tan(pi*(U-1/2))
# N(0,1): X1=sqrt(-2*log(U1))*cos(2*pi*U2), X2=sqrt(-2*log(U1))*sin(2*pi*U2)


# plot cauchy histogram
hist(cauchy.draws,prob=T,main="Cauchy Draws",xlab="X",breaks=10000,xlim=c(-10,10),ylim=c(0,.4))

# Week10--------------------------------------------------------------------------------------------------------------------
  
# permutation test if mean of two groups are equal
girlcats<-cats$Sex=="F"
Dhat<-mean(cats$Hwt[girlcats])-mean(cats$Hwt[!girlcats]) # original/exist difference
nf<-sum(girlcats)
nm<-sum(!girlcats)
P<-10000
sample_diffs<-rep(NA,P)
for(i in 1:P){
  permu<-cats$Hwt[sample(1:(nf+nm))]
  meanf<-mean(permu[1:nf])
  meanm<-mean(permu[-(1:nf)])
  sample_diffs[i] <- meanf-meanm
}
pval<-mean(abs(sample_diffs)>=abs(Dhat)) # already a two side p-value
format.pval(pval) # get precise of p-value(small values)

# Week11---------------------------------------------------------------------- ----------------------------------------------
  
plot(ecdf(cats$Hwt)) # emprical cdf
plot(density(cats$Hwt), lty="dashed") # estimated pdf
quantile(cats$Hwt,c(0.25,0.5,0.75)) # check quantiles

# mom estimator for gamma
gamma.MMest<-function(data){
m<-mean(data)
v<-var(data)
return(c(a=m^2/v,s=v/m)) # return a named vector
}
cat.MM<-gamma.MMest(cats$Hwt)
curve(dgamma(x,shape=cat.MM["a"],scale=cat.MM["s"]),from=0,to=20,col="blue")

# numerical mom
gamma.mean<-function(a,s){return(a*s)}
gamma.var<-function(a,s){return(a*s^2)}
gamma.diff<-function(params,data){
  a<-params[1]
  s<-params[2]
  return((mean(data)-gamma.mean(a,s))^2+(var(data)-gamma.var(a,s))^2)
}

# mle estimator, advantages: 1. consistent, 2. efficient
neg.gamma.ll<-function(params,data){
a<-params[1]
s<-params[2]
return(-sum(dgamma(data,shape=a,scale=s,log=T))) # log=T makes log density, return negative to minimize
}

nlm(neg.gamma.ll,c(19,1),data=cats$Hwt) # non-linear minimization

# qq plot, quantile vs quantile
qqplot(cats$Hwt,qgamma((1:99)/100,shape=a,scale=s),ylab="Theoretical Quantiles")
abline(0,1,col="red")

# calibration plot, empirical cdf vs theoretical cdf
plot(ecdf(pgamma(cats$Hwt,shape=a,scale=s)),main="Calibration of gamma") # x the, y emp
abline(0,1,col="red")

# ks test
ks.test(cats$Hwt,pgamma,shape=a,scale=s) 
ks.test(cats$Hwt[cats$Sex=="F"],cats$Hwt[cats$Sex=="M"])

# t test for means, assume two groups of independent random normal
t.test(group1, group2)

# hack using 90% as train
n<-length(cats$Hwt)
train<-sample(1:n,size=round(0.9*n))
cat.MM<-gamma.MMest(cats$Hwt[train])
a<-cat.MM["a"];s<-cat.MM["s"]
ks.test(cats$Hwt[-train],pgamma,shape=a,scale=s)

# Week12---------------------------------------------------------------------------------------------------------------------

# gradient descent
# number of iterations: O(1/e), e is error scale
# cost of each iteration: take d derivatives, O(d)
# total O(d/e), hessian is d^2, inverting is d^3
# when calculating test error, using mean square error(MSE) by default, e.g. mean((y-yhat)^2)
library(numDeriv)
grad.descent<-function(f,x0,max.iter=200,step.size=1e-3,stopping.deriv=0.01, ...){ # ... stands for further arguments!
  n<-length(x0)
  xmat<-matrix(0, nrow=n, ncol=max.iter)
  xmat[,1]<-x0
  for (k in 2:max.iter) {
    grad.cur<-grad(f,xmat[,k-1], ...) # grad() function in library(numDeriv)
    if (all(abs(grad.cur)<stopping.deriv)) { # stop if all value of gradient less than stopping deriv
      k <- k-1
      break
    }
    xmat[,k]<-xmat[,k-1]-step.size*grad.cur # converge to O(1/k), k is iterations
    # newton's method converge to O(1/k^2)
    # xmat[,k]<-xmat[,k-1]-solve(hessian(f,xmat[,k-1], ...))%*%grad.cur
  }
  xmat<-xmat[,1:k] # Trim
  return(list(x=xmat[,k], xmat=xmat, k=k, minimum=f(xmat[,k],...)))
}

# optim function
mse<-function(theta){
  mean((gmp$pcgmp-theta[1]*gmp$pop^theta[2])^2)
} 
grad.mse<-function(theta){grad(func=mse,x=theta)}
theta0<-c(5000,0.15)
fit1<-optim(theta0,mse,grad.mse,method="BFGS",hessian=T) # default is gradient free, BFGS is newton, need grad

# nls method
fit2<-nls(pcgmp~theta0*pop^theta1,data=gmp,start=list(theta0=5000,theta1=0.10))
summary(fit2)

plot(pcgmp~pop, data = gmp, log = 'x')
pop.order <- order(gmp$pop)
lines(gmp$pop[pop.order], fitted(fit2)[pop.order]) # ordered lines
curve(fit1$par[1]*x^fit1$par[2], add = TRUE,lty = "dashed", col = "red")

# Week13-----------------------------------------------------------------------------------------------------------------------
  
# logistic regression
# E(y)=p=exp(b0+b1*x)/(1+exp(b0+b1*x))
# by solving odd=p/(1-p)=exp(b0+b1*x)
neg.logistic.NLL<-function(beta,data=prostate){
  n<-nrow(data)
  y<-data$Y
  x<-cbind(1,as.matrix(data[,1:7]))
  linear.component<-x%*%beta
  p.i<-exp(linear.component)/(1+exp(linear.component))
  return(-sum(dbinom(y,size=1,p=p.i,log=T)))
}

nlm(neg.logistic.NLL,p=,data=) # use ```{r,warning=FALSE} for warnings

# odds ratio
# =odds2/odds1=exp(b0+b1*(x+1))/exp(b0+b1*x)=exp(b1)
# odds of event{Y=1} occuring are multipled by exp(b1) for every unit increase in x

# glm function
model<-glm(y~x,data=dat,family=binomial(link="logit"))
x.test<-data.frame(x=7) # you have to specify variable names in the new dataframe, exactly same as in the model
linear.pred<-predict(model, newdata=x.test) # predicts the linear component by default
p.pred<-exp(linear.pred)/(1+exp(linear.pred))
p.pred<-predict(model, newdata=x.test, type="response") # predict the probability directly

glm(Y~X1+X2+X3+X4,data=glm.data,family="poisson")

model.quad<-lm(Y~x+I(x^2),data=data.train) # fit high order linear regression

plot(x,y,type='o') # line+dot type
lines(x,y,type='p',col='red') # two dot plots in one figure


