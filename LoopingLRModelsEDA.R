# Script for LR Modelling tests
# SVN: $Id: LoopingLRModelsEDA.R 595 2012-04-08 21:39:22Z rob $

# ROC Curve support 
library(ROCR)
library('RJSONIO')



# This is all you can alter

# event.horizon<-10
# num.repeats<-10

selection.cutoff<-0.03

# -------------


INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

cmd.args<-commandArgs(TRUE);
all.args<-'Command Line Args:'
index<-0
for (arg in cmd.args)
{
    index<-index+1
    all.args<-paste(sep=',', all.args,':index = ', index, '  ', arg)
}

for (arg in cmd.args) cat("  ", arg, "\n", sep="")

# cmd.args[1] style, EH=10
event.horizon<-as.numeric(strsplit(cmd.args[1],'=')[[1]][2])
cat("---> event.horizon: ", event.horizon, "\n", sep='')

# cmd.args[2] style, mode=3X2 or mode=single
mode<-strsplit(cmd.args[2],'=')[[1]][2]
cat("---> mode: ", mode, "\n", sep='')


use.min.model<-FALSE
if(is.na(cmd.args[3]) == FALSE)
{
    use.min.model<-strsplit(cmd.args[3],'=')[[1]][2]
    cat("---> use.min.model: ", use.min.model, "\n", sep='')
}

num.repeats<-10
if(is.na(cmd.args[4]) == FALSE)
{
    num.repeats<-as.numeric(strsplit(cmd.args[4],'=')[[1]][2])
    cat("---> num.repeats: ", mode, "\n", sep='')
}

# Set up logging
source('UtilScripts/Logging.R')
hostname<-system('hostname',intern=T)
smallname<-unlist(strsplit(hostname,'\\.'))[1]
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/LoopingLR-output-',smallname,'EH-',event.horizon,'.log')
log.level<-3

# Start things off 
LogWrite('LoopingLRModelsEDA.R Starting ...',3)

# Log the command line arguments
LogWrite(paste('all.args',all.args),3)

# Get the support functions
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels'))
source('H_StatsSupport.R')


AddModelStats<-function(auc.mean=0.755,auc.median=0.5,auc.sd=0.1
                            ,h.mean=0.05,h.median=0.045,h.sd=0.01
                            ,index.equal=FALSE)
{
    x<-0.55
    text.cex<-0.8
    text(x,0.40,pos=4,cex=text.cex, paste(sep='','AUC Mean: ', sprintf('%.3f',auc.mean)))
    text(x,0.35,pos=4,cex=text.cex, paste(sep='','AUC Median: ', sprintf('%.3f',auc.median)))
    text(x,0.30,pos=4,cex=text.cex, paste(sep='','AUC SD: ',sprintf('%.3f',auc.sd)))
    
    text(x,0.20,pos=4,cex=text.cex, paste(sep='','H Mean: ', sprintf('%.3f',h.mean)))
    text(x,0.15,pos=4,cex=text.cex, paste(sep='','H Median: ', sprintf('%.3f',h.median)))
    text(x,0.10,pos=4,cex=text.cex, paste(sep='','H SD: ',sprintf('%.3f',h.sd)))
    
    if(index.equal == TRUE)
    {
        text(x - 0.1,0.05,pos=4,cex=text.cex, paste(sep='','Median model, AUC = H'))
    }
       
}

GetBestModelIndex<-function(median,all.scores)
{
    ordered.all.scores<-order(all.scores)
    
    sorted.all.scores<-sort(all.scores)
    len<-length(all.scores)
    lower.diff<- abs(median - all.scores[len/2])
    upper.diff<- abs(median - all.scores[(len/2) + 1])
    
    best.model.index <- (len/2) + 1
    if(lower.diff < upper.diff)
    {
        best.model.index <- (len/2)
    }


    return(ordered.all.scores[best.model.index])
}

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
    plot(perf,lwd=1.0,col='red',add=TRUE)
    
    
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
    
    H_Results<-Calc_H_Score(case.labels,pred.data$prediction)
    
    h_score<-H_Results$H
    
    return(list('auc'=current.auc,'h_score'=h_score,'perf'=perf))
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


TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_',event.horizon)

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

min.model.str<-''
if(use.min.model == TRUE)
{
    min.model.str<-'min_'
}

plotFileName<-''
if(mode == '3X2')
{
    imageWidth<-150/25.4
    imageHeight<-225/25.4
    plotFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/'
                                ,min.model.str,'output_',event.horizon,'/ROC_EH_',event.horizon,'_3X2.pdf')
                
    pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10)
    par(mfrow=c(3,2), mar=c(5.1,4.1,2.5,4.1),font.main=1)
}



for(window.size in seq(5,30,by=5))
{

    running.auc.model.full.objs<-list()
    running.auc.model.full.preds<-list()
    
    running.auc.model.full<-vector()
    running.h_score.model.full<-vector()

    running.auc.model.mean.quadratic<-vector()
    running.auc.model.mean.sd.quadratic<-vector()
    
    running.selection<-data.frame()
    running.selection.col.names<-vector()
    
    
    if(mode == 'single')
    {
        imageWidth<-95/25.4
        imageHeight<-110/25.4
        plotFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/'
                                ,min.model.str,'output_',event.horizon,'/ROC_EH_'
                                ,event.horizon,'_', window.size,'_withAUC-FM1.pdf')
        pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10)
        par(font.main=1)
    }
    
    
    first.run<-TRUE
    
    print(paste(sep='','Processing: ',event.horizon,'_',window.size))
    for(sequence in seq(1,num.repeats))
    {
        trg.filename<-paste(sep='',TTG_DIR,'/TTG_Training_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
    
        # Space to store the modelling results for each sequence file
        model.res<-list()
        model.res$trg.filename<-trg.filename
        print(paste(sep='','Sequence: ',sequence))
        
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
        
            # --- Store off the results of the model
            signals<-unlist(attr(model.full$R,'dimnames')[1])
            if(first.run == TRUE)
            {
                running.selection<-cbind(signals)
                running.selection.col.names[1]<-'Signal'
                first.run<-FALSE
            }
        
            model.res$model.full<-list()
            model.res$model.full$name<-'model.full'
            model.res$model.full$sequence<-sequence
        
            model.res$model.full$aic<-summary(model.full)$aic
            model.res$model.full$null.deviance<-summary(model.full)$null.deviance
            model.res$model.full$deviance<-summary(model.full)$deviance
        
            model.res$model.full$coef<-summary(model.full)$coefficients[,1]
            model.res$model.full$se<-summary(model.full)$coefficients[,2]
        
            signif<-summary(model.full)$coefficients[,4]
            model.res$model.full$signif<-signif
            model.res$model.full$selection<-lapply(signif,SelectionThreshold,selection.cutoff)
            running.selection.col.names[sequence+1]<-paste(sep='','run_',sequence)
            running.selection<-cbind(running.selection,model.res$model.full$selection)
            
            # ------------------
            
                                    
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
            model.res$model.mean.quadratic$selection<-lapply(summary(model.mean.quadratic)$coefficients[,4],SelectionThreshold,selection.cutoff)
                     
            model.mean.sd.quadratic<-glm(case_label~ Age + Sex 
                                    + HRT_spot + BPs_spot + BPd_spot + BPm_spot 
                                    #+ BPp_spot
                                    + HRT_mean + BPs_mean + BPd_mean + BPm_mean + BPp_mean
                                    + I(HRT_mean^2) + I(BPs_mean^2) + I(BPd_mean^2) + I(BPm_mean^2) + I(BPp_mean^2)
                                    + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd
                                    + I(HRT_sd^2) + I(BPs_sd^2) + I(BPd_sd^2) + I(BPm_sd^2) + I(BPp_sd^2)
                                    + HRT_slope + BPs_slope + BPd_slope + BPm_slope + BPp_slope
                                    ,family='binomial', data=model.input)
        
     
            model.res$model.mean.sd.quadratic<-list()
            model.res$model.mean.sd.quadratic$name<-'model.mean.sd.quadratic'
            model.res$model.mean.sd.quadratic$sequence<-sequence
            model.res$model.mean.sd.quadratic$coef<-summary(model.mean.sd.quadratic)$coefficients[,1]
            model.res$model.mean.sd.quadratic$se<-summary(model.mean.sd.quadratic)$coefficients[,2]
            model.res$model.mean.sd.quadratic$signif<-summary(model.mean.sd.quadratic)$coefficients[,4]
            model.res$model.mean.sd.quadratic$selection<-lapply(summary(model.mean.sd.quadratic)$coefficients[,4],SelectionThreshold,selection.cutoff)

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
        
        model.res$model.full$auc<-pred.results$auc
        running.auc.model.full[sequence]<-pred.results$auc
        
        model.res$model.full$h_score<-pred.results$h_score
        running.h_score.model.full[sequence]<-pred.results$h_score
        
        # Store off the model and prediction objects
        running.auc.model.full.objs[[sequence]]<-model.full
        running.auc.model.full.preds[[sequence]]<-pred.results
        
        
        pred.results<-AssessModel(model.mean.quadratic,model.input.test,all.test$case_label)
        model.res$model.mean.quadratic$auc<-pred.results$auc
        running.auc.model.mean.quadratic[sequence]<-pred.results$auc
        
        pred.results.msq<-AssessModel(model.mean.sd.quadratic,model.input.test,all.test$case_label)
        model.res$model.mean.sd.quadratic$auc<-pred.results.msq$auc
        running.auc.model.mean.sd.quadratic[sequence]<-pred.results.msq$auc
        
        # Hook this run's results into the info object
        res.info$single.runs[[sequence]]<-model.res
    
        # --- Build up the ROC plot --- #
        
        title<-paste(sep='','ROC for ',event.horizon,'_',window.size,' BDS')
        if(use.min.model == TRUE)
        {
            title<-paste(sep='','ROC for min model ',event.horizon,'_',window.size,' BDS')
        }
        
        if(sequence == 1)
        {
            #plot(pred.results$perf,avg='threshold',spread.estimate='boxplot',main=title,lwd=0.5)
            plot(pred.results$perf,main=title,lwd=0.5,col='gray70')
        }
        else
        {
            #plot(pred.results$perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=0.5,col='gray80')
            plot(pred.results$perf,add=TRUE,lwd=0.5,col='gray80')
        }
    }
    
    AddNNROC()
    
    # Pick out the median model by AUC
    auc.mean<-mean(running.auc.model.full)
    auc.median<-median(running.auc.model.full)
    auc.sd<-sd(running.auc.model.full)
    auc.bm.index<-GetBestModelIndex(auc.median,running.auc.model.full)
    
    # Add the median model ROC from AUC as a blue thicker line
    best.perf<-running.auc.model.full.preds[[auc.bm.index]]$perf
    plot(best.perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=1.5,col='blue')
    
    # Pick out the median model by H_score
    h_score.mean<-mean(running.h_score.model.full)
    h_score.median<-median(running.h_score.model.full)
    h_score.sd<-sd(running.h_score.model.full)
    h_score.bm.index<-GetBestModelIndex(h_score.median,running.h_score.model.full)
    
    best.model.index.equal = TRUE
    # Plot if the index is *not* the same 
    if(h_score.bm.index != auc.bm.index)
    {
        # Add the median model ROC from H_score as a green thicker line
        best.perf<-running.auc.model.full.preds[[h_score.bm.index]]$perf
        plot(best.perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=1.5,col='green')
        best.model.index.equal = FALSE
    }
    
    AddModelStats(auc.mean,auc.median,auc.sd
                    ,h_score.mean,h_score.median,h_score.sd
                    ,best.model.index.equal)
    
    # Log some top level stats
    avg.auc.model.full<-mean(running.auc.model.full)
    sd.auc.model.full<-sd(running.auc.model.full)
    selection.totals<-vector()
    for(count in 1:dim(running.selection)[1])
    {
        selection.totals[count]<-sum(unlist(running.selection[count,-1]))  
    }
    
    res.info$auc.array.model.full<-running.auc.model.full
    res.info$avg.auc.model.full<-avg.auc.model.full
    res.info$sd.auc.model.full<-sd.auc.model.full
    colnames(running.selection)<-running.selection.col.names
    running.selection<-cbind(running.selection,selection.totals)
    res.info$selection.matrix.model.full<-running.selection
    #res.info$selection.totals.model.full<-selection.totals
    res.info$selection.totals.model.full<-cbind(signals,selection.totals)
    
    
    cat('avg.auc.model.full:',avg.auc.model.full,'\n')
    cat('sd.auc.model.full:',sd.auc.model.full,'\n')
    #-----
    avg.auc.model.mean.quadratic<-mean(running.auc.model.mean.quadratic)
    sd.auc.model.mean.quadratic<-sd(running.auc.model.mean.quadratic)
    res.info$auc.array.model.mean.quadratic<-running.auc.model.mean.quadratic
    res.info$avg.auc.model.mean.quadratic<-avg.auc.model.mean.quadratic
    res.info$sd.auc.model.mean.quadratic<-sd.auc.model.mean.quadratic
    
    cat('avg.auc.model.mean.quadratic:',avg.auc.model.mean.quadratic,'\n')
    cat('sd.auc.model.mean.quadratic:',sd.auc.model.mean.quadratic,'\n')
    #----- 
    avg.auc.model.mean.sd.quadratic<-mean(running.auc.model.mean.sd.quadratic)
    sd.auc.model.mean.sd.quadratic<-sd(running.auc.model.mean.sd.quadratic)
    res.info$auc.array.model.mean.sd.quadratic<-running.auc.model.mean.sd.quadratic
    res.info$avg.auc.model.mean.sd.quadratic<-avg.auc.model.mean.sd.quadratic
    res.info$sd.auc.model.mean.sd.quadratic<-sd.auc.model.mean.sd.quadratic
    
    cat('avg.auc.model.mean.sd.quadratic:',avg.auc.model.mean.sd.quadratic,'\n')
    cat('sd.auc.model.mean.sd.quadratic:',sd.auc.model.mean.sd.quadratic,'\n')
    
    library(MASS)
    matrixFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/'
                                    ,min.model.str,'output_',event.horizon,'/SelectionMatrix_EH_'
                                    ,event.horizon,'_', window.size,'.txt')
    #colnames(running.selection)<-running.selection.col.names
    write.matrix(file=matrixFileName,sep=',',running.selection)
    
    
    
    if(mode == 'single')
    {
        dev.off()
    }
    
    # Save off the best model
    best.model.index<-auc.bm.index
    best.model<-running.auc.model.full.objs[[best.model.index]]
    best.model.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/'
                                    ,min.model.str,'output_',event.horizon,'/Best_Model_',smallname,'_',
                                        event.horizon,'_', window.size,'_Index_',best.model.index,'.rda')
    
    cat(sep='', 'Best Model Index = ',best.model.index,'\n')
    save(best.model,file=best.model.filename)
    
    res.info$stop<-time.Now.str()
    end<-Sys.time()
    timeInfo<-ElapsedTime(start,end)
    elapsed.str<-paste('Elapsed Time:',unlist(timeInfo$printStr))
    res.info$elapsed<-elapsed.str
    
    JSON.filename<-paste(sep='',INSTALL_DIR
                            ,'/PhDStuff/ThesisSoftware/LRModels/',min.model.str,'output_'
                            ,event.horizon,'/ROC-',smallname,'-EH-',event.horizon,'-',window.size,'-Info.json')
    cat(file=JSON.filename,toJSON(res.info))

}

if(mode == '3X2')
{
    dev.off()
}
end<-Sys.time()
timeInfo<-ElapsedTime(start,end)
elapsed.str<-paste('Elapsed Time:',unlist(timeInfo$printStr))

LogWrite(elapsed.str,0)
LogWrite('... Processing Complete',0)
print(paste('... Processing Complete,',elapsed.str))

warnings()

                   
