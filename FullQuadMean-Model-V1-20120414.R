# Full model with quadratic mean and std dev
# 
# This meets the published format
# 
# SVN $Id: FullQuadMean-Model-V1-20120414.R 623 2012-05-13 22:26:48Z rob $

model.name<-'FullQuadMean'

model.code<-function()
{
                        
    model<-glm(case_label~ Age + Sex 
                        + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
                        #+ BPp_spot
                        + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
                        + I(HRT_mean^2) + I(BPs_mean^2) + I(BPd_mean^2) + I(BPm_mean^2) + I(BPp_mean^2)
                        + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
                        + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
                        ,family='binomial', data=model.input)
    return(model)                       
}

