# Script for Elasticnet LR Modelling tests on 30 minute *all* data
# SVN: $Id: all-data-R-LR-EDA.R 624 2012-05-14 20:07:51Z rob $

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels'))

event.horizon<-10
num.repeats<-10

data.area<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware//TrgTestGenerator/output_all_data_10')

library(glmnet)

for(sequence in seq(1,num.repeats))
{
    cat(sep='','Sequence: ',sequence,'\n')
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
        results<-data.frame(summary(model)$coefficients[,1], summary(model)$coefficients[,4])
        print(results)
    
    detach(all.trg)
    
    cat(sep='','\n>>>>\nglmnet(alpha=0.5)\n')
    model.elasticnet.1<-cv.glmnet(as.matrix(all.vars.clean[,2:158]),all.vars.clean$response.var
                                        ,alpha=0.5,family='binomial',standardize=FALSE)
                                        
    print(coef(model.elasticnet.1))
    
    cat(sep='','\n>>>>\nglmnet(alpha=0.8)\n')
    model.elasticnet.2<-cv.glmnet(as.matrix(all.vars.clean[,2:158]),all.vars.clean$response.var
                                        ,alpha=0.8,family='binomial',standardize=FALSE)
                                        
    print(coef(model.elasticnet.2))
}