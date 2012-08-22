# ROC Curve support 
library(ROCR)
library('RJSONIO')

# This is all you can alter

event.horizon<-15
num.repeats<-10

select.cutoff<-0.03

# -------------


INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

# Set up logging
source('UtilScripts/Logging.R')
hostname<-system('hostname',intern=T)
smallname<-unlist(strsplit(hostname,'\\.'))[1]
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/LoopingLR-output-',smallname,'EH-',event.horizon,'.log')
log.level<-3

# Start things off 
LogWrite('LoopingLRModelsEDA.R Starting ...',3)

AddNNROC<-function()
{
    # Now add in the Bayesian NN for 30_15
    # Point to the script and data area
    setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/LogisticRegression/BayesianNNComparison'))
    
    # Read in the training and test sets
    
    predict.data<-read.table("30_15_PredListWithHdrAndData.txt",header=T, sep="", na.strings="NULL", dec=".", strip.white=TRUE)
    
    numRows<-length(predict.data$Case)
    
    # Correct the Prob column, which has the probability reversed if the 
    # Pred variable = 0, I think the idea here was "we are predicting a zero
    # with probability x" where x is high i.e. 1 - the classifier probability
    # if you think it is going to be a low value. 
    #
    # We want the probability from the classifier
    
    Prob.adjusted<-numeric(numRows)
    
    for(count in 1:numRows)
    {
        if (predict.data$Pred[count] == 0)
        {
            Prob.adjusted[count]<-(1-predict.data$Prob[count])
        
        }
        else
        {
            Prob.adjusted[count]<-(predict.data$Prob[count])
        }
    }
    
    
    pred.data<-data.frame(Prob.adjusted,predict.data$Actual)
    
    colnames(pred.data)<-c('prediction','hypo')
    
    pred.positive<-pred.data[pred.data$hypo==1,]
    totalPos<-length(pred.positive$prediction)
    
    
    true.pos.0.3<-pred.positive[pred.positive$prediction>=0.3,]
    truePos<-length(true.pos.0.3$prediction)
    
    #------------------------------------------------------
    
    pred.negative<-pred.data[pred.data$hypo==0,]
    totalNeg<-length(pred.negative$prediction)
    
    true.neg.0.3<-pred.negative[pred.negative$prediction<0.3,]
    trueNeg<-length(true.neg.0.3$prediction)
    
    false.pos.0.3<-pred.negative[pred.negative$prediction>=0.3,]
    falsePos<-length(false.pos.0.3$prediction)	
    
    pred<-prediction(pred.data$prediction,pred.data$hypo)
    perf<-performance(pred,'tpr','fpr')
    
    perf.auc<-performance(pred,measure="auc")
    perf.auc
    
    current.auc<-as.numeric(perf.auc@y.values)
    cat('NN AUC: ', current.auc,'\n')
    
    #plot(perf,avg='threshold',spread.estimate='boxplot',lwd=0.5,col='red',add=TRUE)
    plot(perf,lwd=0.5,col='red',add=TRUE)
    
    
    abline(0,1,lty=2,lwd=0.5)
}

AssessModel<-function(model.name,model.input.test,case.labels)
{
    predict.data<-predict(model.name,model.input.test,type='response')
    # Make a data frame of the predictions from the model and the class_labels of what actually happened
    pred.data<-data.frame(predict.data,case.labels)
    colnames(pred.data)<-c('prediction','hypo')

    pred<-prediction(pred.data$prediction,pred.data$hypo)
    perf<-performance(pred,'tpr','fpr')
    perf.auc<-performance(pred,measure="auc")
    perf.auc
    
    current.auc<-as.numeric(perf.auc@y.values)
    cat(current.auc,'\n')
    
    return(list('auc'=current.auc,'perf'=perf))
}

# SelectionThreshold<-function(signif,threshold)
# {
#     if(signif<=threshold)
#     {
#         return(1)
#     }
#     else
#     {
#         return(0)
#     }    
# }

# Main code starts below -------

TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_',event.horizon)

imageWidth<-95
imageHeight<-110
dpiRes<-600

# The object that collects the running results
res.info<-list()
res.info$start<-time.Now.str()
res.info$num.repeats<-num.repeats
res.info$single.runs<-list()

for(window.size in seq(5,5,by=5))
{
    running.auc.model.full<-vector()
    running.auc.model.mean.quadratic<-vector()
    running.auc.model.mean.slope.quadratic<-vector()
    

    plotFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/ROC_EH_',event.horizon,'_', window.size,'_withAUC.png')
    bitmap(plotFileName,width=imageWidth,height=imageHeight,res= dpiRes,units='mm')
    #par(mfrow=c(3,2), mar=c(5.1,4.1,2.5,4.1),font.main=1,cex.main=1.2,cex.lab=1.5)
    
    for(sequence in seq(1,num.repeats))
    {
        trg.filename<-paste(sep='',TTG_DIR,'/TTG_Training_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
    
        # Space to store the modelling results for each sequence file
        model.res<-list()
        
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
            
            
            model.input<-data.frame(Age,Sex,HRT_spot,BPs_spot,BPd_spot,BPm_spot,BPp_spot,
                                            HRT_mean,BPs_mean,BPd_mean,BPm_mean,BPp_mean,
                                            HRT_sd,BPs_sd,BPd_sd,BPm_sd,BPp_sd,
                                            HRT_slope,BPs_slope,BPd_slope,BPm_slope,BPp_slope
                                    )


            model.full<-glm(case_label~ Age + Sex 
                                    + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
                                    #+ BPp_spot
                                    + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
                                    + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
                                    + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
                                    ,family='binomial', data=model.input)
            model.res$model.full<-list()
            model.res$model.full$name<-'model.full'
            model.res$model.full$sequence<-sequence
            model.res$model.full$aic<-summary(model.full)$aic
            model.res$model.full$null.deviance<-summary(model.full)$null.deviance
            model.res$model.full$deviance<-summary(model.full)$deviance
            model.res$model.full$coef<-summary(model.full)$coefficients[,1]
            model.res$model.full$se<-summary(model.full)$coefficients[,2]
            model.res$model.full$signif<-summary(model.full)$coefficients[,4]
            #fred<-summary(model.full)$coefficients[,4]
            #model.res$model.full$selection<-lapply(fred,SelectionThreshold,select.cutoff)
            
                                    
            model.mean.quadratic<-glm(case_label~ Age + Sex 
                                    + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
                                    #+ BPp_spot
                                    + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
                                    + I(HRT_mean^2) + I(BPs_mean^2) + I(BPd_mean^2) + I(BPm_mean^2) + I(BPp_mean^2)
                                    + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
                                    + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
                                    ,family='binomial', data=model.input)
            model.res$model.mean.quadratic<-list()
            model.res$model.mean.quadratic$name<-'model.mean.quadratic'
            model.res$model.mean.quadratic$sequence<-sequence
            model.res$model.mean.quadratic$coef<-summary(model.mean.quadratic)$coefficients[,1]
            model.res$model.mean.quadratic$se<-summary(model.mean.quadratic)$coefficients[,2]
            model.res$model.mean.quadratic$signif<-summary(model.mean.quadratic)$coefficients[,4]
                                    
            model.mean.slope.quadratic<-glm(case_label~ Age + Sex 
                                    + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
                                    #+ BPp_spot
                                    + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
                                    + I(HRT_mean^2) + I(BPs_mean^2) + I(BPd_mean^2) + I(BPm_mean^2) + I(BPp_mean^2)
                                    + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
                                    + I(HRT_sd^2) + I(BPs_sd^2) + I(BPd_sd^2) + I(BPm_sd^2) + I(BPp_sd^2)
                                    + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
                                    ,family='binomial', data=model.input)
            model.res$model.mean.slope.quadratic<-list()
            model.res$model.mean.slope.quadratic$name<-'model.mean.slope.quadratic'
            model.res$model.mean.slope.quadratic$sequence<-sequence
            model.res$model.mean.slope.quadratic$coef<-summary(model.mean.slope.quadratic)$coefficients[,1]
            model.res$model.mean.slope.quadratic$se<-summary(model.mean.slope.quadratic)$coefficients[,2]
            model.res$model.mean.slope.quadratic$signif<-summary(model.mean.slope.quadratic)$coefficients[,4]
    
        detach(all.trg)
       
        test.filename<-paste(sep='',TTG_DIR,'/TTG_Test_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.test<-read.table(test.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

        attach(all.test)
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
            
            model.input.test<-data.frame(Age,Sex,HRT_spot,BPs_spot,BPd_spot,BPm_spot,BPp_spot,
                                            HRT_mean,BPs_mean,BPd_mean,BPm_mean,BPp_mean,
                                            HRT_sd,BPs_sd,BPd_sd,BPm_sd,BPp_sd,
                                            HRT_slope,BPs_slope,BPd_slope,BPm_slope,BPp_slope)

        detach(all.test)
        
        # ------------ Prediction phase ---------- #
        pred.results<-AssessModel(model.full,model.input.test,all.test$case_label)
        
        model.res$model.full$auc<-pred.results$current.auc
        running.auc.model.full[sequence]<-pred.results$current.auc
        
        pred.results<-AssessModel(model.mean.quadratic,model.input.test,all.test$case_label)
        model.res$model.mean.quadratic$auc<-pred.results$current.auc
        running.auc.model.mean.quadratic[sequence]<-pred.results$current.auc
        
        pred.results<-AssessModel(model.mean.slope.quadratic,model.input.test,all.test$case_label)
        model.res$model.mean.slope.quadratic$auc<-pred.results$current.auc
        
        
        running.auc.model.mean.slope.quadratic[sequence]<-pred.results$current.auc
        
        # Hook this run's results into the info object
        res.info$single.runs[[sequence]]<-model.res
    
        # --- Build up the ROC plot --- #
        
        title<-paste(sep='','ROC for ',event.horizon,'_',window.size,' BDS')
        if(sequence == 1)
        {
            plot(pred.results$perf,avg='threshold',spread.estimate='boxplot',main=title)
        }
        else
        {
            plot(pred.results$perf,avg='threshold',spread.estimate='boxplot',add=TRUE)
        }
    }
    
    AddNNROC()
    
    # Log some top level stats
    avg.auc.model.full<-mean(running.auc.model.full)
    sd.auc.model.full<-sd(running.auc.model.full)
    res.info$avg.auc.model.full<-avg.auc.model.full
    res.info$sd.auc.model.full<-sd.auc.model.full
    
    cat('avg.auc.model.full:',avg.auc.model.full,'\n')
    cat('sd.auc.model.full:',sd.auc.model.full,'\n')
    #-----
    avg.auc.model.mean.quadratic<-mean(running.auc.model.mean.quadratic)
    sd.auc.model.mean.quadratic<-sd(running.auc.model.mean.quadratic)
    res.info$avg.auc.model.mean.quadratic<-avg.auc.model.mean.quadratic
    res.info$sd.auc.model.mean.quadratic<-sd.auc.model.mean.quadratic
    
    cat('avg.auc.model.mean.quadratic:',avg.auc.model.mean.quadratic,'\n')
    cat('sd.auc.model.mean.quadratic:',sd.auc.model.mean.quadratic,'\n')
    #----- 
    avg.auc.model.mean.slope.quadratic<-mean(running.auc.model.mean.slope.quadratic)
    sd.auc.model.mean.slope.quadratic<-sd(running.auc.model.mean.slope.quadratic)
    res.info$avg.auc.model.mean.slope.quadratic<-avg.auc.model.mean.slope.quadratic
    res.info$sd.auc.model.mean.slope.quadratic<-sd.auc.model.mean.slope.quadratic
    
    cat('avg.auc.model.mean.slope.quadratic:',avg.auc.model.mean.slope.quadratic,'\n')
    cat('sd.auc.model.mean.slope.quadratic:',sd.auc.model.mean.slope.quadratic,'\n')

}

dev.off()

res.info$stop<-time.Now.str()

JSON.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/ROC-',smallname,'-EH-',event.horizon,'-Info.json')
cat(file=JSON.filename,toJSON(res.info))

end<-Sys.time()
timeInfo<-ElapsedTime(start,end)
LogWrite(unlist(timeInfo$printStr),0)