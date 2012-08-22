# Full model with quadratic mean and std dev
# 
# This meets the published format
# 
# SVN $Id: Full-Elasticnet-pt5-Model-V1-20120509.R 623 2012-05-13 22:26:48Z rob $

library(glmnet)


model.name<-'Full-Elasticnet-pt5'

model.code<-function()
{
                        
    model<-cv.glmnet(as.matrix(all.vars.clean[,2:23]),all.vars.clean$response.var,alpha=0.5
                                                                    ,family='binomial',standardize=FALSE)
    return(model)                       
}

