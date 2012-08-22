# Script for LR Modelling tests
# SVN: $Id: LR_All_EH_5_ModelsEDA.R 545 2012-03-13 22:25:40Z rob $

# ROC Curve support 
library(ROCR)
library('RJSONIO')



# This is all you can alter

#event.horizon<-10
num.repeats<-10

selection.cutoff<-0.03

# -------------


INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

# cmd.args<-commandArgs(TRUE);
# all.args<-'Command Line Args:'
# index<-0
# for (arg in cmd.args)
# {
#     index<-index+1
#     all.args<-paste(sep=',', all.args,':index = ', index, '  ', arg)
# }
# 
# for (arg in cmd.args) cat("  ", arg, "\n", sep="")
# 
# # cmd.args[1] style, EH=10
# event.horizon<-as.numeric(strsplit(cmd.args[1],'=')[[1]][2])
# cat("---> event.horizon: ", event.horizon, "\n", sep='')
# 
# # cmd.args[2] style, mode=3X2 or mode=single
# mode<-strsplit(cmd.args[2],'=')[[1]][2]
# cat("---> mode: ", mode, "\n", sep='')
# 
# num.repeats<-10
# if(is.na(cmd.args[3]) == FALSE)
# {
#     num.repeats<-as.numeric(strsplit(cmd.args[3],'=')[[1]][2])
#     cat("---> num.repeats: ", mode, "\n", sep='')
# }

# Set up logging
source('UtilScripts/Logging.R')
hostname<-system('hostname',intern=T)
smallname<-unlist(strsplit(hostname,'\\.'))[1]
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/LR-15-EH5-output-',smallname,'-EH5.log')
log.level<-3

# Start things off 
LogWrite('LR_15_EH5_ModelsEDA.R Starting ...',3)

# Log the command line arguments
#LogWrite(paste('all.args',all.args),3)

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

SelectionThreshold<-function(signif,threshold)
{
    if(signif<=threshold)
    {
        return(1)
    }
    else
    {
        return(0)
    }    
}
    

# Main code starts below -------

#TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_',event.horizon)
TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_15_EH5')
TTG_BASE_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_All_EH5')

# imageWidth<-95
# imageHeight<-110
# dpiRes<-600

# placeholder
imageWidth<-3
imageHeight<-3

# The object that collects the running results
res.info<-list()
res.info$start<-time.Now.str()
res.info$num.repeats<-num.repeats
res.info$single.runs<-list()

plotFileName<-''

running.auc.model.15.EH5<-vector()
running.auc.model.mean.quadratic<-vector()
running.auc.model.mean.sd.quadratic<-vector()
running.selection<-data.frame()
running.selection.col.names<-vector()



imageWidth<-95/25.4
imageHeight<-110/25.4

plotFileName<-paste(sep='',INSTALL_DIR,
                     '/PhDStuff/ThesisSoftware/LRModels/','output_15_EH5/ROC-',smallname,'-15-EH-5-Info.json')
pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10)
par(font.main=1)



first.run<-TRUE

print('Processing: 15 EH_5')
for(sequence in seq(1,num.repeats))
{
    trg.filename<-paste(sep='',TTG_BASE_DIR,'/All_Training_EH_5_seq_',sequence, '.csv')
    all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

    # Space to store the modelling results for each sequence file
    model.res<-list()
    model.res$trg.filename<-trg.filename
    print(paste(sep='','Sequence: ',sequence))
    
    attach(all.trg)
        
        model.input<-data.frame(Age,Sex
                                # ,HRT_spot_10_5 ,BPs_spot_10_5 ,BPd_spot_10_5 ,BPm_spot_10_5 ,BPp_spot_10_5
#                                 ,HRT_mean_10_5 ,BPs_mean_10_5 ,BPd_mean_10_5 ,BPm_mean_10_5 ,BPp_mean_10_5
#                                 ,HRT_sd_10_5 ,BPs_sd_10_5 ,BPd_sd_10_5 ,BPm_sd_10_5 ,BPp_sd_10_5
#                                 ,HRT_slope_10_5 ,BPs_slope_10_5 ,BPd_slope_10_5 ,BPm_slope_10_5 ,BPp_slope_10_5
                                
                                ,HRT_spot_15_5 ,BPs_spot_15_5 ,BPd_spot_15_5 ,BPm_spot_15_5 ,BPp_spot_15_5
                                ,HRT_mean_15_5 ,BPs_mean_15_5 ,BPd_mean_15_5 ,BPm_mean_15_5 ,BPp_mean_15_5
                                ,HRT_sd_15_5 ,BPs_sd_15_5 ,BPd_sd_15_5 ,BPm_sd_15_5 ,BPp_sd_15_5
                                ,HRT_slope_15_5 ,BPs_slope_15_5 ,BPd_slope_15_5 ,BPm_slope_15_5 ,BPp_slope_15_5

                                ,HRT_spot_20_5 ,BPs_spot_20_5 ,BPd_spot_20_5 ,BPm_spot_20_5 ,BPp_spot_20_5
                                ,HRT_mean_20_5 ,BPs_mean_20_5 ,BPd_mean_20_5 ,BPm_mean_20_5 ,BPp_mean_20_5
                                ,HRT_sd_20_5 ,BPs_sd_20_5 ,BPd_sd_20_5 ,BPm_sd_20_5 ,BPp_sd_20_5
                                ,HRT_slope_20_5 ,BPs_slope_20_5 ,BPd_slope_20_5 ,BPm_slope_20_5 ,BPp_slope_20_5
                                
                                ,HRT_spot_25_5 ,BPs_spot_25_5 ,BPd_spot_25_5 ,BPm_spot_25_5 ,BPp_spot_25_5
                                ,HRT_mean_25_5 ,BPs_mean_25_5 ,BPd_mean_25_5 ,BPm_mean_25_5 ,BPp_mean_25_5
                                ,HRT_sd_25_5 ,BPs_sd_25_5 ,BPd_sd_25_5 ,BPm_sd_25_5 ,BPp_sd_25_5
                                ,HRT_slope_25_5 ,BPs_slope_25_5 ,BPd_slope_25_5 ,BPm_slope_25_5 ,BPp_slope_25_5
                                
                                ,HRT_spot_30_5 ,BPs_spot_30_5 ,BPd_spot_30_5 ,BPm_spot_30_5 ,BPp_spot_30_5
                                ,HRT_mean_30_5 ,BPs_mean_30_5 ,BPd_mean_30_5 ,BPm_mean_30_5 ,BPp_mean_30_5
                                ,HRT_sd_30_5 ,BPs_sd_30_5 ,BPd_sd_30_5 ,BPm_sd_30_5 ,BPp_sd_30_5
                                ,HRT_slope_30_5 ,BPs_slope_30_5 ,BPd_slope_30_5 ,BPm_slope_30_5 ,BPp_slope_30_5
                                )


        model.15.EH5<-glm(case_label~ Age + Sex 
        
                               #  + HRT_spot_10_5 + BPs_spot_10_5 + BPd_spot_10_5 + BPm_spot_10_5 
#                                 + HRT_mean_10_5 + BPs_mean_10_5 + BPd_mean_10_5 + BPm_mean_10_5 + BPp_mean_10_5
#                                 + HRT_sd_10_5 + BPs_sd_10_5 + BPd_sd_10_5 + BPm_sd_10_5 + BPp_sd_10_5
#                                 + HRT_slope_10_5 + BPs_slope_10_5 + BPd_slope_10_5 + BPm_slope_10_5 + BPp_slope_10_5
                                
                                + HRT_spot_15_5 + BPs_spot_15_5 + BPd_spot_15_5 + BPm_spot_15_5 
                                + HRT_mean_15_5 + BPs_mean_15_5 + BPd_mean_15_5 + BPm_mean_15_5 + BPp_mean_15_5
                                + HRT_sd_15_5 + BPs_sd_15_5 + BPd_sd_15_5 + BPm_sd_15_5 + BPp_sd_15_5
                                + HRT_slope_15_5 + BPs_slope_15_5 + BPd_slope_15_5 + BPm_slope_15_5 + BPp_slope_15_5

                                + HRT_spot_20_5 + BPs_spot_20_5 + BPd_spot_20_5 + BPm_spot_20_5 
                                + HRT_mean_20_5 + BPs_mean_20_5 + BPd_mean_20_5 + BPm_mean_20_5 + BPp_mean_20_5
                                + HRT_sd_20_5 + BPs_sd_20_5 + BPd_sd_20_5 + BPm_sd_20_5 + BPp_sd_20_5
                                + HRT_slope_20_5 + BPs_slope_20_5 + BPd_slope_20_5 + BPm_slope_20_5 + BPp_slope_20_5
                                
                                + HRT_spot_25_5 + BPs_spot_25_5 + BPd_spot_25_5 + BPm_spot_25_5 
                                + HRT_mean_25_5 + BPs_mean_25_5 + BPd_mean_25_5 + BPm_mean_25_5 + BPp_mean_25_5
                                + HRT_sd_25_5 + BPs_sd_25_5 + BPd_sd_25_5 + BPm_sd_25_5 + BPp_sd_25_5
                                + HRT_slope_25_5 + BPs_slope_25_5 + BPd_slope_25_5 + BPm_slope_25_5 + BPp_slope_25_5
                                
                                + HRT_spot_30_5 + BPs_spot_30_5 + BPd_spot_30_5 + BPm_spot_30_5 
                                + HRT_mean_30_5 + BPs_mean_30_5 + BPd_mean_30_5 + BPm_mean_30_5 + BPp_mean_30_5
                                + HRT_sd_30_5 + BPs_sd_30_5 + BPd_sd_30_5 + BPm_sd_30_5 + BPp_sd_30_5
                                + HRT_slope_30_5 + BPs_slope_30_5 + BPd_slope_30_5 + BPm_slope_30_5 + BPp_slope_30_5
                                                 
                                ,family='binomial', data=model.input)
    
        # --- Store off the results of the model
        signals<-unlist(attr(model.15.EH5$R,'dimnames')[1])
        if(first.run == TRUE)
        {
            running.selection<-cbind(signals)
            running.selection.col.names[1]<-'Signal'
            first.run<-FALSE
        }
    
        model.res$model.15.EH5<-list()
        model.res$model.15.EH5$name<-'model.15.EH5'
        model.res$model.15.EH5$sequence<-sequence
    
        model.res$model.15.EH5$aic<-summary(model.15.EH5)$aic
        model.res$model.15.EH5$null.deviance<-summary(model.15.EH5)$null.deviance
        model.res$model.15.EH5$deviance<-summary(model.15.EH5)$deviance
    
        model.res$model.15.EH5$coef<-summary(model.15.EH5)$coefficients[,1]
        model.res$model.15.EH5$se<-summary(model.15.EH5)$coefficients[,2]
    
        signif<-summary(model.15.EH5)$coefficients[,4]
        model.res$model.15.EH5$signif<-signif
        model.res$model.15.EH5$selection<-lapply(signif,SelectionThreshold,selection.cutoff)
        running.selection.col.names[sequence+1]<-paste(sep='','run_',sequence)
        running.selection<-cbind(running.selection,model.res$model.15.EH5$selection)
        
        # ------------------
        
                                
       #  model.mean.quadratic<-glm(case_label~ Age + Sex 
#                                 + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
#                                 #+ BPp_spot
#                                 + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
#                                 + I(HRT_mean^2) + I(BPs_mean^2) + I(BPd_mean^2) + I(BPm_mean^2) + I(BPp_mean^2)
#                                 + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
#                                 + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
#                                 ,family='binomial', data=model.input)
#                                 
#         model.res$model.mean.quadratic<-list()
#         model.res$model.mean.quadratic$name<-'model.mean.quadratic'
#         model.res$model.mean.quadratic$sequence<-sequence
#         model.res$model.mean.quadratic$coef<-summary(model.mean.quadratic)$coefficients[,1]
#         model.res$model.mean.quadratic$se<-summary(model.mean.quadratic)$coefficients[,2]
#         model.res$model.mean.quadratic$signif<-summary(model.mean.quadratic)$coefficients[,4]
#         model.res$model.mean.quadratic$selection<-lapply(summary(model.mean.quadratic)$coefficients[,4],SelectionThreshold,selection.cutoff)
#                  
#         model.mean.sd.quadratic<-glm(case_label~ Age + Sex 
#                                 + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
#                                 #+ BPp_spot
#                                 + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
#                                 + I(HRT_mean^2) + I(BPs_mean^2) + I(BPd_mean^2) + I(BPm_mean^2) + I(BPp_mean^2)
#                                 + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
#                                 + I(HRT_sd^2) + I(BPs_sd^2) + I(BPd_sd^2) + I(BPm_sd^2) + I(BPp_sd^2)
#                                 + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
#                                 ,family='binomial', data=model.input)
#     
#  
#         model.res$model.mean.sd.quadratic<-list()
#         model.res$model.mean.sd.quadratic$name<-'model.mean.sd.quadratic'
#         model.res$model.mean.sd.quadratic$sequence<-sequence
#         model.res$model.mean.sd.quadratic$coef<-summary(model.mean.sd.quadratic)$coefficients[,1]
#         model.res$model.mean.sd.quadratic$se<-summary(model.mean.sd.quadratic)$coefficients[,2]
#         model.res$model.mean.sd.quadratic$signif<-summary(model.mean.sd.quadratic)$coefficients[,4]
#         model.res$model.mean.sd.quadratic$selection<-lapply(summary(model.mean.sd.quadratic)$coefficients[,4],SelectionThreshold,selection.cutoff)

    detach(all.trg)
   
    test.filename<-paste(sep='',TTG_BASE_DIR,'/All_Test_EH_5_seq_',sequence,'.csv')
    all.test<-read.table(test.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

    attach(all.test)
    
        model.input.test<-data.frame(Age,Sex
#                                 ,HRT_spot_10_5 ,BPs_spot_10_5 ,BPd_spot_10_5 ,BPm_spot_10_5 ,BPp_spot_10_5
#                                 ,HRT_mean_10_5 ,BPs_mean_10_5 ,BPd_mean_10_5 ,BPm_mean_10_5 ,BPp_mean_10_5
#                                 ,HRT_sd_10_5 ,BPs_sd_10_5 ,BPd_sd_10_5 ,BPm_sd_10_5 ,BPp_sd_10_5
#                                 ,HRT_slope_10_5 ,BPs_slope_10_5 ,BPd_slope_10_5 ,BPm_slope_10_5 ,BPp_slope_10_5
                                
                                ,HRT_spot_15_5 ,BPs_spot_15_5 ,BPd_spot_15_5 ,BPm_spot_15_5 ,BPp_spot_15_5
                                ,HRT_mean_15_5 ,BPs_mean_15_5 ,BPd_mean_15_5 ,BPm_mean_15_5 ,BPp_mean_15_5
                                ,HRT_sd_15_5 ,BPs_sd_15_5 ,BPd_sd_15_5 ,BPm_sd_15_5 ,BPp_sd_15_5
                                ,HRT_slope_15_5 ,BPs_slope_15_5 ,BPd_slope_15_5 ,BPm_slope_15_5 ,BPp_slope_15_5

                                ,HRT_spot_20_5 ,BPs_spot_20_5 ,BPd_spot_20_5 ,BPm_spot_20_5 ,BPp_spot_20_5
                                ,HRT_mean_20_5 ,BPs_mean_20_5 ,BPd_mean_20_5 ,BPm_mean_20_5 ,BPp_mean_20_5
                                ,HRT_sd_20_5 ,BPs_sd_20_5 ,BPd_sd_20_5 ,BPm_sd_20_5 ,BPp_sd_20_5
                                ,HRT_slope_20_5 ,BPs_slope_20_5 ,BPd_slope_20_5 ,BPm_slope_20_5 ,BPp_slope_20_5
                                
                                ,HRT_spot_25_5 ,BPs_spot_25_5 ,BPd_spot_25_5 ,BPm_spot_25_5 ,BPp_spot_25_5
                                ,HRT_mean_25_5 ,BPs_mean_25_5 ,BPd_mean_25_5 ,BPm_mean_25_5 ,BPp_mean_25_5
                                ,HRT_sd_25_5 ,BPs_sd_25_5 ,BPd_sd_25_5 ,BPm_sd_25_5 ,BPp_sd_25_5
                                ,HRT_slope_25_5 ,BPs_slope_25_5 ,BPd_slope_25_5 ,BPm_slope_25_5 ,BPp_slope_25_5
                                
                                ,HRT_spot_30_5 ,BPs_spot_30_5 ,BPd_spot_30_5 ,BPm_spot_30_5 ,BPp_spot_30_5
                                ,HRT_mean_30_5 ,BPs_mean_30_5 ,BPd_mean_30_5 ,BPm_mean_30_5 ,BPp_mean_30_5
                                ,HRT_sd_30_5 ,BPs_sd_30_5 ,BPd_sd_30_5 ,BPm_sd_30_5 ,BPp_sd_30_5
                                ,HRT_slope_30_5 ,BPs_slope_30_5 ,BPd_slope_30_5 ,BPm_slope_30_5 ,BPp_slope_30_5
                                )


    detach(all.test)
    
    # ------------ Prediction phase ---------- #
    pred.results<-AssessModel(model.15.EH5,model.input.test,all.test$case_label)
    
    model.res$model.15.EH5$auc<-pred.results$auc
    running.auc.model.15.EH5[sequence]<-pred.results$auc
    
#     pred.results<-AssessModel(model.mean.quadratic,model.input.test,all.test$case_label)
#     model.res$model.mean.quadratic$auc<-pred.results$auc
#     running.auc.model.mean.quadratic[sequence]<-pred.results$auc
#     
#     pred.results.msq<-AssessModel(model.mean.sd.quadratic,model.input.test,all.test$case_label)
#     model.res$model.mean.sd.quadratic$auc<-pred.results.msq$auc
#     running.auc.model.mean.sd.quadratic[sequence]<-pred.results.msq$auc
    
    # Hook this run's results into the info object
    res.info$single.runs[[sequence]]<-model.res

    # --- Build up the ROC plot --- #
    
    title<-paste(sep='','ROC for All EH_5 BDS')
    if(sequence == 1)
    {
        plot(pred.results$perf,avg='threshold',spread.estimate='boxplot',main=title,lwd=0.5)
    }
    else
    {
        plot(pred.results$perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=0.5)
    }
}

AddNNROC()

# Log some top level stats
avg.auc.model.15.EH5<-mean(running.auc.model.15.EH5)
sd.auc.model.15.EH5<-sd(running.auc.model.15.EH5)
selection.totals<-vector()
for(count in 1:dim(running.selection)[1])
{
    selection.totals[count]<-sum(unlist(running.selection[count,-1]))  
}

res.info$auc.array.model.15.EH5<-running.auc.model.15.EH5
res.info$avg.auc.model.15.EH5<-avg.auc.model.15.EH5
res.info$sd.auc.model.15.EH5<-sd.auc.model.15.EH5
colnames(running.selection)<-running.selection.col.names
running.selection<-cbind(running.selection,selection.totals)
res.info$selection.matrix.model.15.EH5<-running.selection
#res.info$selection.totals.model.15.EH5<-selection.totals
res.info$selection.totals.model.15.EH5<-cbind(signals,selection.totals)


cat('avg.auc.model.15.EH5:',avg.auc.model.15.EH5,'\n')
cat('sd.auc.model.15.EH5:',sd.auc.model.15.EH5,'\n')
#-----
# avg.auc.model.mean.quadratic<-mean(running.auc.model.mean.quadratic)
# sd.auc.model.mean.quadratic<-sd(running.auc.model.mean.quadratic)
# res.info$auc.array.model.mean.quadratic<-running.auc.model.mean.quadratic
# res.info$avg.auc.model.mean.quadratic<-avg.auc.model.mean.quadratic
# res.info$sd.auc.model.mean.quadratic<-sd.auc.model.mean.quadratic
# 
# cat('avg.auc.model.mean.quadratic:',avg.auc.model.mean.quadratic,'\n')
# cat('sd.auc.model.mean.quadratic:',sd.auc.model.mean.quadratic,'\n')
# #----- 
# avg.auc.model.mean.sd.quadratic<-mean(running.auc.model.mean.sd.quadratic)
# sd.auc.model.mean.sd.quadratic<-sd(running.auc.model.mean.sd.quadratic)
# res.info$auc.array.model.mean.sd.quadratic<-running.auc.model.mean.sd.quadratic
# res.info$avg.auc.model.mean.sd.quadratic<-avg.auc.model.mean.sd.quadratic
# res.info$sd.auc.model.mean.sd.quadratic<-sd.auc.model.mean.sd.quadratic
# 
# cat('avg.auc.model.mean.sd.quadratic:',avg.auc.model.mean.sd.quadratic,'\n')
# cat('sd.auc.model.mean.sd.quadratic:',sd.auc.model.mean.sd.quadratic,'\n')

library(MASS)
matrixFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/',
                                'output_All_EH5/SelectionMatrix_All_EH5.txt')
#colnames(running.selection)<-running.selection.col.names
write.matrix(file=matrixFileName,sep=',',running.selection)

dev.off()

res.info$stop<-time.Now.str()
end<-Sys.time()
timeInfo<-ElapsedTime(start,end)
elapsed.str<-paste('Elapsed Time:',unlist(timeInfo$printStr))
res.info$elapsed<-elapsed.str

JSON.filename<-paste(sep='',INSTALL_DIR,
                     '/PhDStuff/ThesisSoftware/LRModels/','output_15_EH5/ROC-',smallname,'-15-EH-5-Info.json')
cat(file=JSON.filename,toJSON(res.info))


end<-Sys.time()
timeInfo<-ElapsedTime(start,end)
elapsed.str<-paste('Elapsed Time:',unlist(timeInfo$printStr))

LogWrite(elapsed.str,0)
LogWrite('... Processing Complete',0)
print(paste('... Processing Complete,',elapsed.str))
                   
