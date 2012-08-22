# Script for LR Modelling tests
# SVN: $Id: LoopingLRModelsEDA.R 547 2012-03-18 22:53:54Z rob $

# ROC Curve support 
library(ROCR)
library('RJSONIO')



# This is all you can alter

#event.horizon<-10
#num.repeats<-10

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

num.repeats<-10
if(is.na(cmd.args[3]) == FALSE)
{
    num.repeats<-as.numeric(strsplit(cmd.args[3],'=')[[1]][2])
    cat("---> num.repeats: ", mode, "\n", sep='')
}

# Set up logging
source('UtilScripts/Logging.R')
hostname<-system('hostname',intern=T)
smallname<-unlist(strsplit(hostname,'\\.'))[1]
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/MinLRModels-output-',smallname,'EH-',event.horizon,'.log')
log.level<-3

# Start things off 
LogWrite('MinLRModelsEDA.R Starting ...',3)

# Log the command line arguments
LogWrite(paste('all.args',all.args),3)

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

base.LR.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels')

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



plotFileName<-''
if(mode == '3X2')
{
    imageWidth<-150/25.4
    imageHeight<-225/25.4
    plotFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/',
                'min_output_',event.horizon,'/ROC_EH_',event.horizon,'_3X2.pdf')
                
    pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10)
    par(mfrow=c(3,2), mar=c(5.1,4.1,2.5,4.1),font.main=1)
}

model.name.logged<-FALSE

for(window.size in seq(5,30,by=5))
{
    running.auc.model.min<-vector()
    running.auc.model.mean.quadratic<-vector()
    running.auc.model.mean.sd.quadratic<-vector()
    running.selection<-data.frame()
    running.selection.col.names<-vector()
    
    best.model<-NULL
    best.pred<-NULL
    best.model.auc<-0
    best.model.index<-0
    
    if(mode == 'single')
    {
        imageWidth<-95/25.4
        imageHeight<-110/25.4
        plotFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/',
                     'min_output_',event.horizon,'/ROC_EH_',event.horizon,'_', window.size,'_withAUC-FM1.pdf')
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
        
            # Assumes model file conforms to published format
            # model.name is in the 'sourced' file
            model.filename<-'~/PhDStuff/ThesisSoftware/LRModels/MinModel-V1-20120412.R'
            source(model.filename)
            
            if(model.name.logged == FALSE)
            {
                LogWrite(model.name,0)
                cat(sep='',' Processing model: ', model.name, '\n')
                model.name.logged = TRUE
            }
        
        detach(all.trg)
        
        
        # --- Store off the results of the model
        signals<-unlist(attr(model.min$R,'dimnames')[1])
        if(first.run == TRUE)
        {   
            running.selection<-cbind(signals)
            running.selection.col.names[1]<-'Signal'
            first.run<-FALSE
        }
    
        model.res$model.min<-list()
        model.res$model.min$name<-'model.min'
        model.res$model.min$sequence<-sequence
    
        model.res$model.min$aic<-summary(model.min)$aic
        model.res$model.min$null.deviance<-summary(model.min)$null.deviance
        model.res$model.min$deviance<-summary(model.min)$deviance
    
        model.res$model.min$coef<-summary(model.min)$coefficients[,1]
        model.res$model.min$se<-summary(model.min)$coefficients[,2]
    
        signif<-summary(model.min)$coefficients[,4]
        model.res$model.min$signif<-signif
        model.res$model.min$selection<-lapply(signif,SelectionThreshold,selection.cutoff)
        running.selection.col.names[sequence+1]<-paste(sep='','run_',sequence)
        running.selection<-cbind(running.selection,model.res$model.min$selection)
        
        # ------------------
            
        
       
        test.filename<-paste(sep='',TTG_DIR,'/TTG_Test_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
        all.test<-read.table(test.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

        setwd(base.LR.dir)
        source('Standard-LR-Model-Test-Input.R')

        
        # ------------ Prediction phase ---------- #
        pred.results<-AssessModel(model.min,model.input.test,all.test$case_label)
        
        model.res$model.min$auc<-pred.results$auc
        running.auc.model.min[sequence]<-pred.results$auc
        
        if(pred.results$auc > best.model.auc)
        {
            best.model<-model.min
            best.model.auc<-pred.results$auc
            best.model.index<-sequence
            best.pred<-pred.results
        }
        
        # Hook this run's results into the info object
        res.info$single.runs[[sequence]]<-model.res
    
        # --- Build up the ROC plot --- #
        
        title<-paste(sep='','Min Model ROC for ',event.horizon,'_',window.size,' BDS')
        if(sequence == 1)
        {
            plot(pred.results$perf,main=title,lwd=0.5,col='gray70')
        }
        else
        {
            plot(pred.results$perf,add=TRUE,lwd=0.5,col='gray80')
        }
    }
    
    AddNNROC()
    
    # Add the best model ROC as a blue thicker line
    plot(best.pred$perf,avg='threshold',spread.estimate='boxplot',add=TRUE,lwd=1.5,col='blue')
    
    # Log some top level stats
    avg.auc.model.min<-mean(running.auc.model.min)
    sd.auc.model.min<-sd(running.auc.model.min)
    selection.totals<-vector()
    for(count in 1:dim(running.selection)[1])
    {
        selection.totals[count]<-sum(unlist(running.selection[count,-1]))  
    }
    
    res.info$auc.array.model.min<-running.auc.model.min
    res.info$avg.auc.model.min<-avg.auc.model.min
    res.info$sd.auc.model.min<-sd.auc.model.min
    colnames(running.selection)<-running.selection.col.names
    running.selection<-cbind(running.selection,selection.totals)
    res.info$selection.matrix.model.min<-running.selection
    res.info$selection.totals.model.min<-cbind(signals,selection.totals)
    
    
    cat('avg.auc.model.min:',avg.auc.model.min,'\n')
    cat('sd.auc.model.min:',sd.auc.model.min,'\n')
    #-----
    
    
    library(MASS)
    matrixFileName<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/',
                                    'min_output_',event.horizon,'/SelectionMatrix_EH_',event.horizon,'_', window.size,'.txt')
    write.matrix(file=matrixFileName,sep=',',running.selection)
    
    if(mode == 'single')
    {
        dev.off()
    }
    
    # Save off the best model
    best.model.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/',
                                    'min_output_',event.horizon,'/Best_Model_',smallname,'_',
                                        event.horizon,'_', window.size,'_Index_',best.model.index,'.rda')
    
    cat(sep='', 'Best Model Index = ',best.model.index,'\n')
    save(best.model,file=best.model.filename)
    
    res.info$stop<-time.Now.str()
    end<-Sys.time()
    timeInfo<-ElapsedTime(start,end)
    elapsed.str<-paste('Elapsed Time:',unlist(timeInfo$printStr))
    res.info$elapsed<-elapsed.str
    
    JSON.filename<-paste(sep='',INSTALL_DIR,
                         '/PhDStuff/ThesisSoftware/LRModels/','min_output_',event.horizon,'/ROC-',smallname,
                         '-EH-',event.horizon,'-',window.size,'-Info.json')
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
                   
