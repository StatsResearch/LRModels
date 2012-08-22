library(sm)

set.seed(42)

rolling.intercept<-numeric()
rolling.obs<-numeric()

run.five.models<-function(seed.number=42)
{
    set.seed(seed.number)

    pos<-rnorm(100,12,1)
    sm.density(pos)
    
    neg<-rnorm(99900,15,1)
    sm.density(neg)

obs<-pos
obs[101:100000]<-neg
# nrow(obs)
# 
# sm.density(obs)
# length(obs)

pos.obs<-cbind(pos,'response'=1)
neg.obs<-cbind(neg,'response'=0)


experiment<-data.frame('obs'=pos.obs[,1],'resp'=pos.obs[,2])
experiment[101:100000,]<-c(neg.obs[,1], neg.obs[,2])

plot(x=experiment$obs,y=experiment$resp)

dev.new()

sm.density(obs)
sm.density(pos,col='red',add=T)
sm.density(neg,col='blue',add=T)


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

model5<-glm(resp~obs,data=experiment[1:10000,],family='binomial')
count<-5
current.model<-model5
summary(current.model)
rolling.intercept[count]<-coef(current.model)['(Intercept)']
rolling.obs[count]<-coef(current.model)['obs']

rolling.intercept
summary(rolling.intercept)
mean(rolling.intercept)
sd(rolling.intercept)


rolling.obs
summary(rolling.obs)
mean(rolling.obs)
sd(rolling.obs)





