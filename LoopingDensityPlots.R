# ROC Curve support 
library(ROCR)
library(sm)

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
LogWrite('LoopingDdensityPlots.R Starting ...',3)
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

plotFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/ROC_EH_',event.horizon,'_DemogSpot.png')
bitmap(plotFileName,width=imageWidth,height=imageHeight,res= dpiRes,units='mm')
par(mfrow=c(3,2), mar=c(5.1,4.1,2.5,4.1),font.main=1,cex.main=1.2,cex.lab=1.5)

densityCalc<-sm.density(EventDuration,positive=T)
modeCalc<-densityCalc$eval.points[order(densityCalc$estimate)]
modeIndex<-length(modeCalc)
EUSIG.data.90.5.modeLine<-modeCalc[modeIndex]

for(window.size in seq(5,5,by=5))
{
    
    for(sequence in seq(1,3))
    {
        trg.filename<-paste(sep='',TTG_DIR,'/TTG_Training_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
    
        attach(all.trg)
        model1<-glm(case_label~ Age + Sex 
                                + get(paste(sep='','HRT_spot_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPs_spot_',event.horizon,'_',window.size)) 
                                + get(paste(sep='','BPd_spot_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPm_spot_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPp_spot_',event.horizon,'_',window.size))

                                + get(paste(sep='','HRT_mean_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPs_mean_',event.horizon,'_',window.size)) 
                                + get(paste(sep='','BPd_mean_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPm_mean_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPp_mean_',event.horizon,'_',window.size))
                                
                                + get(paste(sep='','HRT_sd_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPs_sd_',event.horizon,'_',window.size)) 
                                + get(paste(sep='','BPd_sd_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPm_sd_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPp_sd_',event.horizon,'_',window.size))
                                
                                + get(paste(sep='','HRT_slope_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPs_slope_',event.horizon,'_',window.size)) 
                                + get(paste(sep='','BPd_slope_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPm_slope_',event.horizon,'_',window.size))
                                + get(paste(sep='','BPp_slope_',event.horizon,'_',window.size))
                                
                ,family='binomial')
    
        detach(all.trg)
    
    
        test.filename<-paste(sep='',TTG_DIR,'/TTG_Test_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.test<-read.table(test.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
    
        predict.data<-predict(model1,all.test,type='response')
    
    
        # Make a data frame of the predictions from the model and the class_labels of what 
        # actually happened
        pred.data<-data.frame(predict.data,all.test$case_label)
    
        colnames(pred.data)<-c('prediction','hypo')
    
    
        pred<-prediction(pred.data$prediction,pred.data$hypo)
        perf<-performance(pred,'tpr','fpr')
    
        title<-paste(sep='','ROC for ',event.horizon,'_',window.size,' BDS')
        if(sequence == 1)
        {
            plot(perf,avg='threshold',spread.estimate='boxplot',main=title)
        }
        else
        {
            plot(perf,avg='threshold',spread.estimate='boxplot',add=TRUE)
        }
    }
    
    AddNNROC()

}



dev.off()

end<-now()
timeInfo<-ElapsedTime(start,end)
LogWrite(unlist(timeInfo$printStr),0)