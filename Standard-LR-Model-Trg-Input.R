# The standard model input


HRT_spot<-get(paste(sep='','HRT_spot_',event.horizon,'_',window.size))
BPs_spot<-get(paste(sep='','BPs_spot_',event.horizon,'_',window.size)) 
BPd_spot<-get(paste(sep='','BPd_spot_',event.horizon,'_',window.size))
BPm_spot<-get(paste(sep='','BPm_spot_',event.horizon,'_',window.size))
BPp_spot<-get(paste(sep='','BPp_spot_',event.horizon,'_',window.size))

HRT_mean<-get(paste(sep='','HRT_mean_',event.horizon,'_',window.size))
BPs_mean<-get(paste(sep='','BPs_mean_',event.horizon,'_',window.size)) 
BPd_mean<-get(paste(sep='','BPd_mean_',event.horizon,'_',window.size))
BPm_mean<-get(paste(sep='','BPm_mean_',event.horizon,'_',window.size))
BPp_mean<-get(paste(sep='','BPp_mean_',event.horizon,'_',window.size))

HRT_sd<-get(paste(sep='','HRT_sd_',event.horizon,'_',window.size))
BPs_sd<-get(paste(sep='','BPs_sd_',event.horizon,'_',window.size)) 
BPd_sd<-get(paste(sep='','BPd_sd_',event.horizon,'_',window.size))
BPm_sd<-get(paste(sep='','BPm_sd_',event.horizon,'_',window.size))
BPp_sd<-get(paste(sep='','BPp_sd_',event.horizon,'_',window.size))

HRT_slope<-get(paste(sep='','HRT_slope_',event.horizon,'_',window.size))
BPs_slope<-get(paste(sep='','BPs_slope_',event.horizon,'_',window.size)) 
BPd_slope<-get(paste(sep='','BPd_slope_',event.horizon,'_',window.size))
BPm_slope<-get(paste(sep='','BPm_slope_',event.horizon,'_',window.size))
BPp_slope<-get(paste(sep='','BPp_slope_',event.horizon,'_',window.size))


model.input<-data.frame(Age,Sex,HRT_spot,BPs_spot,BPd_spot,BPm_spot,BPp_spot,
                                HRT_mean,BPs_mean,BPd_mean,BPm_mean,BPp_mean,
                                HRT_sd,BPs_sd,BPd_sd,BPm_sd,BPp_sd,
                                HRT_slope,BPs_slope,BPd_slope,BPm_slope,BPp_slope
                        )

