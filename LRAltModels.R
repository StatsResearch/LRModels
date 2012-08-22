# ROC Curve support 
library(ROCR)

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtils.R')
start<-now()

#options(error=utils::recover)
#Rprof()



# Set up logging
source('UtilScripts/Logging.R')
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/BSG-TLC-output-',event.horizon,'-',window.size,'.log')
log.level<-3

# Read in the command line arguments
LogWrite('LoopingLRModelsEDA.R Starting ...',3)
LogWrite('-- reading arguments',3)

cmd.args<-commandArgs(TRUE);
all.args<-'Command Line Args:'
for (arg in cmd.args)
{
    all.args<-paste(sep='', all.args, '  ', arg);
}

LogWrite(all.args,0)

TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output')

imageWidth<-95
imageHeight<-110
dpiRes<-600

event.horizon<-10
window.size<-5
sequence<-3



        trg.filename<-paste(sep='',TTG_DIR,'/TTG_Training_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
    
        attach(all.trg)


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
            
            model.terms<-data.frame(Age,Sex,HRT_mean,BPs_mean,BPd_mean,BPm_mean,BPp_mean)
            
model.mean<-glm(case_label~ Age * Sex 
                                    + HRT_mean * BPs_mean * BPd_mean * BPm_mean * BPp_mean
                                    ,family='binomial',data=model.terms)           
            

model.full<-glm(case_label~ Age + Sex 
                                    + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
                                    #+ BPp_spot
                                    + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
                                    + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
                                    + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
                                    ,family='binomial')
    
    
model.mean.interaction<-glm(case_label~ Age * Sex 
                                    + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
                                    + HRT_mean * BPs_mean * BPd_mean * BPm_mean * BPp_mean
                                    + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
                                    + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
                                    ,family='binomial')
    
        detach(all.trg)
        
        
        
        
        
        predict.single.minute<-predict(model1,newdata=all.test[1,],type='response')
        #Notice the way to change colnames within a frame, [] outside the ()
         colnames(single.row)[8:10]<-c("HRT_spot",'BPs_spot','BPd_spot')
         

# Predicting from a single minute's data       

single.row<-data.frame(55,0
                ,59,123,53,73,70
                ,59.8,122.6,53.2,73.2,69.4
                ,0.83666,0.5477226,1.095445,0.4472136,1.516575
                ,0,0.2571429,0,0,0
            )
colnames(single.row)<-c('Age','Sex'
                            ,'HRT_spot','BPs_spot','BPd_spot','BPm_spot','BPp_spot'
                            ,'HRT_mean','BPs_mean','BPd_mean','BPm_mean','BPp_mean'
                            ,'HRT_sd','BPs_sd','BPd_sd','BPm_sd','BPp_sd'
                            ,'HRT_slope','BPs_slope','BPd_slope','BPm_slope','BPp_slope'
                        )
                        
predict.single.minute<-predict(model1,newdata=single.row,type='response')