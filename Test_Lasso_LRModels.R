# Script for Lasso LR Modelling tests
# SVN: $Id: Test_Lasso_LRModels.R 662 2012-06-24 22:00:51Z rob $

# ROC Curve support 
library(ROCR)
library(RJSONIO)

DEBUG<-FALSE

# This is all you can alter

if(DEBUG == TRUE)
{
    event.horizon<-10
    model.file<-'Full-Lasso-Model-V1-20120424.R'
    mode<-'3X2'
    num.repeats<-10
}

selection.cutoff<-0.003

# -------------

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

if(DEBUG == FALSE)
{
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
    
    # cmd.args[2] style, model.file=MinModel-V1-20120412.R
    model.file<-strsplit(cmd.args[2],'=')[[1]][2]
    cat("---> model.file: ", model.file, "\n", sep='')
    
    # cmd.args[3] style, mode=3X2 or mode=single
    mode<-'3X2'
    if(is.na(cmd.args[3]) == FALSE)
    {
        mode<-strsplit(cmd.args[3],'=')[[1]][2]
        cat("---> mode: ", mode, "\n", sep='')
    }
      
   num.repeats<-10
    #num.repeats<-2
    if(is.na(cmd.args[4]) == FALSE)
    {
        num.repeats<-as.numeric(strsplit(cmd.args[4],'=')[[1]][2])
        cat("---> num.repeats: ", num.repeats, "\n", sep='')
    }
}

# Set up logging
source('UtilScripts/Logging.R')
hostname<-system('hostname',intern=T)
smallname<-unlist(strsplit(hostname,'\\.'))[1]
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/Build_LRModels-output-',smallname,'EH-',event.horizon,'.log')
log.level<-3

# Start things off 
LogWrite('MinLRModelsEDA.R Starting ...',3)

if(DEBUG == FALSE)
{
    # Log the command line arguments
    LogWrite(paste('all.args',all.args),3)
}

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

GetMedianModelIndex<-function(median,all.scores)
{
    print(median)
    print(all.scores)
    ordered.all.scores<-order(all.scores)
    print(ordered.all.scores)
    sorted.all.scores<-sort(all.scores)
    print(sorted.all.scores)
    len<-length(all.scores)
    lower.diff<- abs(median - all.scores[len/2])
    upper.diff<- abs(median - all.scores[(len/2) + 1])
    
    best.model.index <- (len/2) + 1
    if(lower.diff < upper.diff)
    {
        best.model.index <- (len/2)
    }

    print(ordered.all.scores[best.model.index])
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

CheckLassoSelection<-function(coefficient,threshold)
{
    if(abs(coefficient) >= threshold)
    {
        return(1)
    }
    else
    {
        return(0)
    }    
}
  
BuildCoefficents<-function(model)
{
    results<-list()
    signals<-unlist(coef(model)@Dimnames[[1]])
    vals<-paste(coef(model))
    results$coef<-data.frame(vals)
    results$signals<-signals
    rownames(results$coef)<-signals

    return( results )
}  

CatchDecode<-function(event.horizon,window.size)
{
    msg<-paste('Error Trap: EH: ',event.horizon,', WS: ', window.size)
    cat(sep='',msg,'\n')
    LogWrite(msg,0)
    LogWrite(geterrmessage(),0)
    #LogWrite(traceback(),0)
}

# Main code starts below -------

base.LR.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels')

######### The loading of the model ##############
setwd(base.LR.dir)
source(model.file)
#################################################

LR.results.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/output_',event.horizon,'/MN_',model.name)
dir.create(LR.results.dir)

TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_',event.horizon)

# placeholder
imageWidth<-3
imageHeight<-3

# The object that collects the running results
res.info<-list()
res.info$start<-time.Now.str()
res.info$num.repeats<-num.repeats
res.info$single.runs<-list()

plotFileName<-''
if(mode == '3X2')
{
    imageWidth<-150/25.4
    imageHeight<-225/25.4
    plotFileName<-paste(sep='',LR.results.dir,'/ROC_EH_',event.horizon,'_3X2.pdf')
                
    pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10)
    par(mfrow=c(3,2), mar=c(5.1,4.1,2.5,4.1),font.main=1)
}

model.name.logged<-FALSE

for(window.size in seq(5,30,by=5))
{
    running.model.objs<-list()
    running.model.preds<-list()
    
    running.auc<-vector()
    running.h_score<-vector()
    
    running.selection<-data.frame()
    running.selection.col.names<-vector()
    
    if(mode == 'single')
    {
        imageWidth<-95/25.4
        imageHeight<-110/25.4
        plotFileName<-paste(sep='',LR.results.dir,'/ROC_EH_',event.horizon,'_', window.size,'_withAUC_andH.pdf')
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
            
            setwd(base.LR.dir)
            source('Standard-LR-Model-Trg-Input.R')
            
            response.var<-case_label 
    
            all.vars<-data.frame(response.var,model.input) 
            all.vars.clean<-na.omit(all.vars) 
        
            # Assumes model file conforms to published format
            # model.name is in the 'sourced' file
            # model.code() function is in the 'sourced' file
        
            running.model<-model.code()
            
            if(model.name.logged == FALSE)
            {
                LogWrite(model.name,0)
                cat(sep='',' Processing model: ', model.name, '\n')
                model.name.logged = TRUE
            }
        
        detach(all.trg)
        
        
        # --- Store off the results of the model
        signals<-unlist(coef(running.model)@Dimnames[[1]])
        if(first.run == TRUE)
        {   
            running.selection<-cbind(signals)
            running.selection.col.names[1]<-'Signal'
            first.run<-FALSE
        }
    
        model.res$model<-list()
        model.res$model$name<-model.name
        model.res$model$sequence<-sequence
    
#         model.res$model$aic<-summary(running.model)$aic
#         model.res$model$null.deviance<-summary(running.model)$null.deviance
#         model.res$model$deviance<-summary(running.model)$deviance
#     
         coefficients<-BuildCoefficents(running.model)
         model.res$model$coef<-coefficients
#         model.res$model$se<-summary(running.model)$coefficients[,2]
#     
#         signif<-summary(running.model)$coefficients[,4]
#         model.res$model$signif<-signif

        model.res$model$selection<-lapply(as.numeric(paste(coef(running.model))),CheckLassoSelection,selection.cutoff)
        
        running.selection.col.names[sequence+1]<-paste(sep='','run_',sequence)
        running.selection<-cbind(running.selection,model.res$model$selection)
        
        # ------------------
            
        
       
        test.filename<-paste(sep='',TTG_DIR,'/TTG_Test_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.test<-read.table(test.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

        setwd(base.LR.dir)
        source('Standard-LR-Model-Test-Input.R')
        
        all.test.data<-data.frame(all.test$case_label,model.input.test) 
        all.test.data.clean<-na.omit(all.test.data) 

        
        # ------------ Prediction phase ---------- #
        pred.results<-AssessModel(running.model,as.matrix(all.test.data.clean[,2:23]),as.matrix(all.test.data.clean[,1]))
        
        model.res$model$auc<-pred.results$auc
        running.auc[sequence]<-pred.results$auc
        
        model.res$model$h_score<-pred.results$h_score
        running.h_score[sequence]<-pred.results$h_score
        
        # Store off the model and prediction objects
        running.model.objs[[sequence]]<-running.model
        running.model.preds[[sequence]]<-pred.results
        
        # Hook this run's results into the info object
        res.info$single.runs[[sequence]]<-model.res
    
        # --- Build up the ROC plot --- #
        
        title<-paste(sep='', model.name,' ROC for ',event.horizon,'_',window.size,' BDS')
        first.plot<-FALSE
        if(sequence == 1)
        {
            tryCatch(plot(pred.results$perf,main=title,lwd=0.5,col='gray70')
                                                            , error=function(e) CatchDecode(event.horizon,window.size))
            first.plot<-FALSE
        }
        else
        {
            if(first.plot == FALSE)
            {
                tryCatch(plot(pred.results$perf,add=TRUE,lwd=0.5,col='gray80')
                                                            , error=function(e) CatchDecode(event.horizon,window.size))
            }
            else
            {
                tryCatch(plot(pred.results$perf,main=title,lwd=0.5,col='gray70')
                                                            , error=function(e) CatchDecode(event.horizon,window.size))
                first.plot<-FALSE
            }
            #plot(pred.results$perf,add=TRUE,lwd=0.5,col='gray80')
        }
    }
    
    #AddNNROC()
    

    # Pick out the median model by AUC
    auc.mean<-mean(running.auc)
    auc.median<-median(running.auc)
    auc.sd<-sd(running.auc)
    auc.mm.index<-GetMedianModelIndex(auc.median,running.auc)
    cat('auc.mm.index:',auc.mm.index,'\n')

    # Add the median model ROC from AUC as a blue thicker line
    median.perf<-running.model.preds[[auc.mm.index]]$perf
    tryCatch(plot(median.perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=1.5,col='blue')
                                                    , error=function(e) CatchDecode(event.horizon,window.size))
    #plot(median.perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=1.5,col='blue')

    # Pick out the median model by H_score
    h_score.mean<-mean(running.h_score)
    h_score.median<-median(running.h_score)
    h_score.sd<-sd(running.h_score)
    h_score.mm.index<-GetMedianModelIndex(h_score.median,running.h_score)
    cat('h_score.mm.index:',h_score.mm.index,'\n')

    median.model.index.equal = TRUE
    # Plot if the index is *not* the same 
    if(h_score.mm.index != auc.mm.index)
    {
        # Add the median model ROC from H_score as a green thicker line
        median.perf<-running.model.preds[[h_score.mm.index]]$perf
        tryCatch(plot(median.perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=1.5,col='green')
                                                    , error=function(e) CatchDecode(event.horizon,window.size))
        #plot(median.perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=1.5,col='green')
        median.model.index.equal = FALSE
    }

    AddModelStats(auc.mean,auc.median,auc.sd
                    ,h_score.mean,h_score.median,h_score.sd
                    ,median.model.index.equal)

    
    # Log some top level stats
    mean.auc<-mean(running.auc)
    sd.auc<-sd(running.auc)
    
    mean.h_score<-mean(running.h_score)
    sd.h_score<-sd(running.h_score)
    
    selection.totals<-vector()
    cat(sep='',unlist(running.selection),'\n')
    for(count in 1:dim(running.selection)[1])
    {
        selection.totals[count]<-sum(unlist(running.selection[count,-1]))  
    }

    res.info$auc.array<-running.auc
    res.info$mean.auc<-mean.auc
    res.info$sd.auc<-sd.auc
    
    res.info$h_score.array<-running.h_score
    res.info$mean.h_score<-mean.h_score
    res.info$sd.h_score<-sd.h_score
    
    colnames(running.selection)<-running.selection.col.names
    running.selection<-cbind(running.selection,selection.totals)
    res.info$selection.matrix.model<-running.selection
    res.info$selection.totals.model<-cbind(signals,selection.totals)
    
    cat('mean.auc:',mean.auc,'\n')
    cat('sd.auc:',sd.auc,'\n')
    cat('mean.h_score:',mean.h_score,'\n')
    cat('sd.h_score:',sd.h_score,'\n')
    #-----
    
    
    library(MASS)
    matrixFileName<-paste(sep='',LR.results.dir,'/SelectionMatrix_EH_',event.horizon,'_', window.size,'.txt')
    write.matrix(file=matrixFileName,sep=',',running.selection)
    
    if(mode == 'single')
    {
        dev.off()
    }
    
    # Save off the median auc model
    median.auc.model.index<-auc.mm.index
    median.auc.model<-running.model.objs[[median.auc.model.index]]
    median.auc.model.filename<-paste(sep='',LR.results.dir,'/Median_AUC_Model_',smallname,'_',
                                        event.horizon,'_', window.size,'_Index_',median.auc.model.index,'.rda')
    
    cat(sep='', 'Median AUC Model Index = ',median.auc.model.index,'\n')
    save(median.auc.model,file=median.auc.model.filename)

    res.info$median.auc.model.index<-median.auc.model.index
    res.info$median.auc.model.filename<-median.auc.model.filename
    
    # Save off the median h_score model
    median.h_score.model.index<-h_score.mm.index
    median.h_score.model<-running.model.objs[[median.h_score.model.index]]
    median.h_score.model.filename<-paste(sep='',LR.results.dir,'/Median_H_Score_Model_',smallname,'_',
                                        event.horizon,'_', window.size,'_Index_',median.h_score.model.index,'.rda')
    
    cat(sep='', 'Median H_Score Model Index = ',median.h_score.model.index,'\n')
    save(median.h_score.model,file=median.h_score.model.filename)

    res.info$median.h_score.model.index<-median.h_score.model.index
    res.info$median.h_score.model.filename<-median.h_score.model.filename
    
    # Some final stats and then save the JSON file
    res.info$stop<-time.Now.str()
    end<-Sys.time()
    timeInfo<-ElapsedTime(start,end)
    elapsed.str<-paste('Elapsed Time:',unlist(timeInfo$printStr))
    res.info$elapsed<-elapsed.str
    
    JSON.filename<-paste(sep='',LR.results.dir,'/ROC_',smallname,
                         '_EH_',event.horizon,'_',window.size,'_Info.json')
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


                   
