library(sm)

set.seed(42)

rolling.intercept<-numeric()
rolling.obs<-numeric()

# Generate the observations
pos<-rnorm(100,12,1)
neg<-rnorm(99900,15,1)

# Add in the responses
pos.obs<-cbind(pos,'response'=1)
neg.obs<-cbind(neg,'response'=0)

# Create the experiment
experiment<-data.frame('obs'=pos.obs[,1],'resp'=pos.obs[,2])
experiment[101:100000,]<-c(neg.obs[,1], neg.obs[,2])

# Create the models with varying amounts of data
model1<-glm(resp~obs,data=experiment[1:200,],family='binomial')
count<-1
current.model<-model1
summary(current.model)
rolling.intercept[count]<-coef(current.model)['(Intercept)']
rolling.obs[count]<-coef(current.model)['obs']

model2<-glm(resp~obs,data=experiment[1:500,],family='binomial')
count<-2
current.model<-model2
summary(current.model)
rolling.intercept[count]<-coef(current.model)['(Intercept)']
rolling.obs[count]<-coef(current.model)['obs']

model3<-glm(resp~obs,data=experiment[1:1000,],family='binomial')
count<-3
current.model<-model3
summary(current.model)
rolling.intercept[count]<-coef(current.model)['(Intercept)']
rolling.obs[count]<-coef(current.model)['obs']

model4<-glm(resp~obs,data=experiment[1:5000,],family='binomial')
count<-4
current.model<-model4
summary(current.model)
rolling.intercept[count]<-coef(current.model)['(Intercept)']
rolling.obs[count]<-coef(current.model)['obs']

model5<-glm(resp~obs,data=experiment[1:100000,],family='binomial')
count<-5
current.model<-model5
summary(current.model)
rolling.intercept[count]<-coef(current.model)['(Intercept)']
rolling.obs[count]<-coef(current.model)['obs']
    
imageH<-150/25.4
imageW<-150/25.4
# Change as required
setwd('/Users/rob/PhDStuff/ThesisSoftware/LRModels')
plot.filename<-'PosNegSplit.pdf'
# Uncomment line below for saving to PDF
#pdf(plot.filename,height=imageH,width=imageW)

par(mfrow=c(2,2))
plot(x=experiment[1:1000,]$obs,y=experiment[1:1000,]$resp,main='Obs and Response')

sm.density(experiment$obs,col='black',lty=2,rugplot=FALSE)
sm.density(experiment$obs[experiment$resp == 1],col='red',add=T)
sm.density(experiment$obs[experiment$resp == 0],col='blue',add=T)
title('Obs Mix')

rolling.intercept
summary(rolling.intercept)
mean(rolling.intercept)
sd(rolling.intercept)
plot(x=seq(1,5),y=rolling.intercept,xlab='Model Number',ylab='Intercept'
    ,ylim=c(30,60),main='Intercept',pch=20,type='b')
abline(h=mean(rolling.intercept))

rolling.obs
summary(rolling.obs)
mean(rolling.obs)
sd(rolling.obs)
plot(x=seq(1,5),y=rolling.obs,xlab='Model Number',ylab='Obs Coef'
    ,ylim=c(-5,0),main='Obs Coef',pch=20,type='b')
abline(h=mean(rolling.obs))

exp(rolling.obs)
round(exp(rolling.obs),2)

# Uncomment line below for saving to PDF
#dev.off()


