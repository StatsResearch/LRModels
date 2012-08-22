# Script for generating EH 2D slice 
# model's AUC and H scores
# SVN: $Id$
 
# -------------

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

# Set up logging
source('UtilScripts/Logging.R')
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/EH_10_Model_AUC_Comparison.log')
log.level<-3

# Start things off 
LogWrite('EH_10_Model_AUC_Comparison.R Starting ...',3)

EH.data<-c(rep(10,6),rep(15,6),rep(20,6),rep(25,6),rep(30,6))
WS.data<-rep(seq(5,30,by=5),5)

imageWidth<-150/25.4
imageHeight<-150/25.4

plot.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels')
plotFileName<-paste(sep='',plot.dir,'/EH_10_Model_AUC_Comparison.pdf')               
pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10,colormodel='srgb')

# The section that gets repeated
model.name<-'Full'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

full.model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)

ylimits<-c(-0.1,0.02)
EH<-10
colour.code<-'red'
full.x<-full.model.AUC.3D$WS.data[full.model.AUC.3D$EH.data==EH]
full.y<-full.model.AUC.3D$AUC.data[full.model.AUC.3D$EH.data==EH]

baseline.x<-full.x
baseline.y<-rep(0,length(full.x))
plot(baseline.x,baseline.y, ylim=ylimits,type='n'
                ,xlab='Window Size', ylab='AUC Performance Difference'
                ,cex.axis=1.2
                ,cex.lab=1.2)

points(baseline.x,baseline.y,col='red',pch=15,cex=1.5)
lines(baseline.x,baseline.y,col='red',lwd=1)
##########
model.name<-'FullQuadMean'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

fqm.model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)

EH<-10
colour.code<-'green'
fqm.x<-fqm.model.AUC.3D$WS.data[fqm.model.AUC.3D$EH.data==EH]
fqm.y<-fqm.model.AUC.3D$AUC.data[fqm.model.AUC.3D$EH.data==EH]

points(baseline.x,fqm.y - full.y,col=colour.code,pch=2,cex=1.5)
lines(baseline.x,fqm.y - full.y,col=colour.code,lwd=1)
##########
model.name<-'Full-Lasso'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

fl.model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)

EH<-10
colour.code<-'blue'
fl.x<-fl.model.AUC.3D$WS.data[fl.model.AUC.3D$EH.data==EH]
fl.y<-fl.model.AUC.3D$AUC.data[fl.model.AUC.3D$EH.data==EH]

points(baseline.x,fl.y - full.y,col=colour.code,pch=4,cex=1.5)
lines(baseline.x,fl.y - full.y,col=colour.code,lwd=1)
##########
model.name<-'Minimum'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

min.model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)


EH<-10
colour.code<-'black'
min.x<-min.model.AUC.3D$WS.data[min.model.AUC.3D$EH.data==EH]
min.y<-min.model.AUC.3D$AUC.data[min.model.AUC.3D$EH.data==EH]

points(baseline.x,min.y - full.y,col=colour.code,pch=6,cex=1.5)
lines(baseline.x,min.y - full.y,col=colour.code,lwd=1)

legend(x=5,y=-0.04,pch=c(15,2,4,6),col=c('red','green','blue','black'),legend=c('Full','FullQuadMean','Full-Lasso','Minimum'),cex=1.5, bty='n')


dev.off()
