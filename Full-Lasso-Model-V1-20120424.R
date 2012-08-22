# Full model using Lasso regression
# 
# This meets the published format
# 
# SVN $Id: Full-Lasso-Model-V1-20120424.R 662 2012-06-24 22:00:51Z rob $

library(glmnet)


model.name<-'Full-Lasso'

model.code<-function()
{
                        
    model<-cv.glmnet(as.matrix(all.vars.clean[,2:23]),all.vars.clean$response.var,alpha=1
                                                                    ,family='binomial',standardize=FALSE)
    return(model)                       
}

