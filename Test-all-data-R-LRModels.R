# Script for Elasticnet LR Modelling tests on 30 minute *all* data
# SVN: $Id: Test-all-data-R-LRModels.R 625 2012-05-14 22:31:38Z rob $

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels'))

event.horizon<-10
num.repeats<-10
selection.cutoff<-0.05

CodeSignificanceValues<-function(signif.values)
{
    results<-list()
    signif.codes<-vector()
    total.signif<-0
    selection.cutoff<-0.05
    for(count in 1:length(signif.values))
    {
        if(signif.values[[count]][1] <= selection.cutoff)
        {   
            signif.codes[count] = '>>>'
            total.signif<-total.signif+1
        }
        else
        {   
            signif.codes[count] = '---'
        }
    }
    
    results$signif.codes<-signif.codes
    results$total.signif<-total.signif
    return(results)
}

data.area<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware//TrgTestGenerator/output_all_data_10')

library(glmnet)

for(sequence in seq(1,num.repeats))
{
    cat(sep='','\n>>>>\nSequence: ',sequence,'\n')
    trg.filename<-paste(sep='',data.area,'/TTG_Training_10_30_all_data_seq_',sequence,'.csv')
    all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
    
    attach(all.trg)
    
        response.var<-case_label
        # The input data.frame
        source('All-Data-LR-Model-Trg-Input.R')
         
        all.vars<-data.frame(response.var,model.input) 
        all.vars.clean<-na.omit(all.vars) 
        
        # The glm() model
        cat(sep='','glm()\n')
        source('All-Data-LR-glm-Model-Trg-Input.R')
        s.codes<-CodeSignificanceValues(summary(model)$coefficients[,4])
        results<-data.frame(summary(model)$coefficients[,1]
                                ,summary(model)$coefficients[,4]
                                ,s.codes$signif.codes
                                )
        colnames(results)<-c('Coeff','Signif','Code')
        print(results)
        cat(sep='','Seq:',sequence,', total.signif = ',s.codes$total.signif,'\n')
    
    detach(all.trg)
    
    cat(sep='','\n>>>>\nglmnet(alpha=0.5)\n')
    model.elasticnet.1<-cv.glmnet(as.matrix(all.vars.clean[,2:158]),all.vars.clean$response.var
                                        ,alpha=0.5,family='binomial',standardize=FALSE)
                                        
    print(coef(model.elasticnet.1))
    
    cat(sep='','\n>>>>\nglmnet(alpha=0.8)\n')
    model.elasticnet.2<-cv.glmnet(as.matrix(all.vars.clean[,2:158]),all.vars.clean$response.var
                                        ,alpha=0.8,family='binomial',standardize=FALSE)
                                        
    print(coef(model.elasticnet.2))
    
    cat(sep='','\n>>>>\nglmnet(alpha=0.3)\n')
    model.elasticnet.3<-cv.glmnet(as.matrix(all.vars.clean[,2:158]),all.vars.clean$response.var
                                        ,alpha=0.3,family='binomial',standardize=FALSE)
                                        
    print(coef(model.elasticnet.3))
}

end<-Sys.time()
timeInfo<-ElapsedTime(start,end)
elapsed.str<-paste('Elapsed Time:',unlist(timeInfo$printStr))

print(paste('... Processing Complete,',elapsed.str))
print(warnings())