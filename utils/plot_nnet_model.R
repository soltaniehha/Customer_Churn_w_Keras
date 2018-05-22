# https://beckmw.wordpress.com/2013/11/14/visualizing-neural-networks-in-r-update/
library(RSNNS)
library(clusterGeneration)
library(nnet)

#import the function from Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

seed.val<-2
set.seed(seed.val)

num.vars<-35
num.obs<-1000
#input variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
#output variables
parms<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)
parms2<-runif(num.vars,-10,10)
y2<-rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=20)

#final datasets
rand.vars<-data.frame(rand.vars)
resp<-data.frame(y1)
names(resp)<-c('Y1')
dat.in<-data.frame(resp,rand.vars)
#neural net with three hidden layers, 9, 11, and 8 nodes in each
mod<-mlp(rand.vars, resp, size=c(16,12),linOut=T)
par(mar=numeric(4),family='serif')
plot.nnet(mod)
