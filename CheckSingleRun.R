event.horizon<-10
window.size<-5
sequence<-6
#model.file<-'Full-Model-V1-20120414.R'
model.file<-'MinModel-V2-20120615.R'

INSTALL_DIR<-'/Users/rob'

base.LR.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels')
TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output_',event.horizon)

######### The loading of the model ##############
setwd(base.LR.dir)
source(model.file)
#################################################



trg.filename<-paste(sep='',TTG_DIR,'/TTG_Training_',event.horizon,'_',window.size,'_seq_',sequence,'.csv')
all.trg<-read.table(trg.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

attach(all.trg)
            
    setwd(base.LR.dir)
    source('Standard-LR-Model-Trg-Input.R')
    
    # Assumes model file conforms to published format
    # model.name is in the 'sourced' file
    # model.code() function is in the 'sourced' file
    
    running.model<-model.code()
                    
detach(all.trg)

summary(running.model)