# Script for generating EH 2D slice 
# model's AUC and H scores
# SVN: $Id$
 
# -------------

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))


AddAUCLine<-function(EH,colour.code,plot.ch=20,line.style=1)
{
    x<-model.AUC.3D$WS.data[model.AUC.3D$EH.data==EH]
    y<-model.AUC.3D$AUC.data[model.AUC.3D$EH.data==EH]
    points(x,y,col=colour.code,pch=plot.ch,lty=line.style)
    lines(x,y,col=colour.code,pch=plot.ch,lty=line.style)
}

AddHScoreLine<-function(EH,colour.code,plot.ch=4,line.style=4)
{
    x<-model.H_score.3D$WS.data[model.H_score.3D$EH.data==EH]
    y<-model.H_score.3D$H_score.data[model.H_score.3D$EH.data==EH]
    points(x,y,col=colour.code,pch=plot.ch,lty=line.style)
    lines(x,y,col=colour.code,pch=plot.ch,lty=line.style)
}

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()

# Set up logging
source('UtilScripts/Logging.R')
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/EH_2D_Slice.log')
log.level<-3

# Start things off 
LogWrite('EH_2D_Slice.R Starting ...',3)

EH.data<-c(rep(10,6),rep(15,6),rep(20,6),rep(25,6),rep(30,6))
WS.data<-rep(seq(5,30,by=5),5)

imageWidth<-150/25.4
imageHeight<-150/25.4

plot.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels')
plotFileName<-paste(sep='',plot.dir,'/EH_10_2D_4_models.pdf')               
pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10,colormodel='srgb')

# The section that gets repeated
model.name<-'Full'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)


ylimits<-c(0.05,0.8)
EH<-10
colour.code<-'red'
x<-model.AUC.3D$WS.data[model.AUC.3D$EH.data==EH]
y<-model.AUC.3D$AUC.data[model.AUC.3D$EH.data==EH]
plot(x,y, ylim=ylimits,type='n',xlab='Window Size', ylab='Performance Score')

AddAUCLine(EH,colour.code)
AddHScoreLine(EH,colour.code)

EH<-15
#AddAUCLine(EH,colour.code,line.style=3)
##########
model.name<-'FullQuadMean'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

EH<-10
colour.code<-'green'
AddAUCLine(EH,colour.code)
AddHScoreLine(EH,colour.code)

EH<-15
#AddAUCLine(EH,colour.code,line.style=3)
##########
model.name<-'Full-Lasso'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

EH<-10
colour.code<-'blue'
AddAUCLine(EH,colour.code)
AddHScoreLine(EH,colour.code)

EH<-15
#AddAUCLine(EH,colour.code,line.style=3)
##########
model.name<-'Minimum'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

EH<-10
colour.code<-'black'
AddAUCLine(EH,colour.code)
AddHScoreLine(EH,colour.code)

EH<-15
#AddAUCLine(EH,colour.code,line.style=3)

dev.off()
