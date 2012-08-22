# Minimum model with quasibinomial residuals
# 
# This meets the published format
# 
# SVN $Id: MinModel-Quasi-V1-20120501.R 623 2012-05-13 22:26:48Z rob $

model.name<-'MinimumQuasi'

model.code<-function()
{
    model<-glm(case_label~ Age + Sex
                        + BPm_mean 
                        + HRT_sd + BPm_sd 
                        ,family='quasibinomial', data=model.input)
    return(model)                       
}

