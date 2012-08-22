# Script for generating 3D surface plots from a 
# model's AUC and H scores
# SVN: $Id: FourModelScoring3D.R 662 2012-06-24 22:00:51Z rob $
 
library(lattice)
library(grid)





# -------------

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtilsNoLubridate.R')
start<-Sys.time()




Generate3DPlot<-function(model.name,zlimits,AUC.data,WS.data,EH.data,model.data)
{
    theme.novpadding <-
       list(layout.heights =
            list(top.padding = 0,
            main.key.padding = 0,
            key.axis.padding = 0,
            axis.xlab.padding = 0,
            xlab.key.padding = 0,
            key.sub.padding = 0,
            bottom.padding = 0),
            layout.widths =
            list(left.padding = 0,
            key.ylab.padding = 0,
            ylab.axis.padding = 0,
            axis.key.padding = 0,
            right.padding = 0))
            
    #auc.title<-paste(sep='','AUC ',model.name, ' Model')
    auc.title<-model.name
    auc.figure<-wireframe(AUC.data~WS.data*EH.data
                    ,data=model.data
                    ,screen=list(z = -200, x = -65, y = 5)
                    ,xlab= list('Window Size',cex=0.8,rot= -10)
                    ,ylab= list('Event Horizon',cex=0.8,rot=67)
                    ,zlim=zlimits
                    ,zlab=list('AUC',cex=0.8, rot=90)
                    ,drape = TRUE
                    ,colorkey = FALSE
                    ,scales = list(arrows=FALSE,cex=0.60,col="black",font=3,tck=1.2
                                                        ,x = list(labels=c(30,25,20,15,10,5)))
                    ,aspect = c(1, 0.65)
                    #,main=list(label=auc.title,cex=0.8,font=1)
                    ,zoom=0.8
                    ,par.settings = theme.novpadding
                    ,panel = function(...)
                    {
                      panel.wireframe(...)
                      grid.text(auc.title, 0.5, 0.9, default.units = "npc")
                    }

                    )

    return(auc.figure)
}

# Set up logging
source('UtilScripts/Logging.R')
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/FourModelScoring3D.log')
log.level<-3

# Start things off 
LogWrite('FourModelScoring3D.R Starting ...',3)

EH.data<-c(rep(10,6),rep(15,6),rep(20,6),rep(25,6),rep(30,6))
#WS.data<-rep(seq(5,30,by=5),5)
WS.data<-rep(seq(30,5,by=-5),5)

imageWidth<-150/25.4
imageHeight<-150/25.4

plot.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels')
plotFileName<-paste(sep='',plot.dir,'/AUC_3D_4_models.pdf')               
pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10,colormodel='srgb')
trellis.par.set(theme = col.whitebg())
trellis.par.set("axis.line", list(col="transparent"))
lattice.options(layout.widths = list(left.padding = list(x = 0.5, units = "inches")))


# The section that gets repeated
model.name<-'Full'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

zlimits<-c(0.4,0.8)
full.plot<-Generate3DPlot(model.name,zlimits,AUC.data,WS.data,EH.data,model.AUC.3D)
##########
model.name<-'FullQuadMean'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

zlimits<-c(0.4,0.8)
full.quad.mean.plot<-Generate3DPlot(model.name,zlimits,AUC.data,WS.data,EH.data,model.AUC.3D)
##########
model.name<-'Full-Lasso'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

zlimits<-c(0.4,0.8)
full.lasso.plot<-Generate3DPlot(model.name,zlimits,AUC.data,WS.data,EH.data,model.AUC.3D)
##########
model.name<-'Minimum'
model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

zlimits<-c(0.4,0.8)
minimum.plot<-Generate3DPlot(model.name,zlimits,AUC.data,WS.data,EH.data,model.AUC.3D)

                                                           
print(full.plot, position=c(0,0.5,0.5,1),more=TRUE)
print(full.quad.mean.plot, position=c(0.5,0.5,1,1),more=TRUE)
print(full.lasso.plot, position=c(0,0,0.5,0.5),more=TRUE)
print(minimum.plot, position=c(0.5,0,1,0.5))


dev.off()
