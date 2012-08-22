# Minimum model with quasibinomial residuals
# 
# This meets the published format
# 
# SVN $Id: MinModel-Inter-V1-20120501.R 623 2012-05-13 22:26:48Z rob $

model.name<-'MinimumInter'

model.code<-function()
{
    model<-glm(case_label~ Age * Sex
                        * BPm_mean 
                        * HRT_sd * BPm_sd 
                        ,family='binomial', data=model.input)
    return(model)                       
}

