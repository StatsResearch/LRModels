# Script for generating 3D surface plots from a 
# model's AUC and H scores
# SVN: $Id$
 

DEBUG<-TRUE

# This is all you can alter

if(DEBUG == TRUE)
{
    model.name<-'Minimum'
}

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
    
    # cmd.args[1] style, model.name=Minimum
    model.name<-strsplit(cmd.args[1],'=')[[1]][2]
    cat("---> model.name: ", model.name, "\n", sep='')
    
}

# Set up logging
source('UtilScripts/Logging.R')
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/ModelScoring3D.log')
log.level<-3

# Start things off 
LogWrite('ModelScoring3D.R Starting ...',3)

if(DEBUG == FALSE)
{
    # Log the command line arguments
    LogWrite(paste('all.args',all.args),3)
}


EH.data<-c(rep(10,6),rep(15,6),rep(20,6),rep(25,6),rep(30,6))
#WS.data<-rep(seq(5,30,by=5),5)
WS.data<-rep(seq(30,5,by=-5),5)

model.info.dir<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/LRModels/Model_Info_',model.name) 
score.data.file<-paste(sep='',model.info.dir,'/Output_AUC_H_Score_model_',model.name,'.R')

setwd(model.info.dir)

# This will bring in the objects AUC.data and H_score.data 
source(score.data.file)

# AUC.data<-c(0.7635 , 0.7249 , 0.6853 , 0.6814 , 0.6430 , 0.6274 ,
#  0.6975 , 0.6810 , 0.6451 , 0.6511 , 0.6019 , 0.5870 ,
#  0.6627 , 0.6435 , 0.6186 , 0.5902 , 0.5926 , 0.5510 ,
#  0.6189 , 0.6183 , 0.5903 , 0.5635 , 0.5402 , 0.5132 ,
#  0.6116 , 0.5750 , 0.5716 , 0.5461 , 0.5173 , 0.5105 ) 
#            
# H_score.data<-c(0.0687 , 0.0410 , 0.0275 , 0.0261 , 0.0178 , 0.0130 ,
#  0.0326 , 0.0267 , 0.0177 , 0.0187 , 0.0087 , 0.0082 ,
#  0.0202 , 0.0159 , 0.0136 , 0.0097 , 0.0084 , 0.0045 ,
#  0.0123 , 0.0130 , 0.0086 , 0.0054 , 0.0044 , 0.0031 ,
#  0.0130 , 0.0076 , 0.0061 , 0.0035 , 0.0034 , 0.0021 )  
           
#,main=list('H Score Minimum Model',cex=0.8,font=1))


model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

library(lattice)

imageWidth<-150/25.4
imageHeight<-150/25.4

plotFileName<-paste(sep='','AUC_3D_',model.name,'.pdf')               
#pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10,colormodel='srgb')
trellis.par.set(theme = col.whitebg())
trellis.par.set("axis.line", list(col="transparent"))
lattice.options(layout.widths = list(left.padding = list(x = 0.5, units = "inches")))

zlimits<-c(0.4,0.8)
auc.figure<-wireframe(AUC.data~WS.data*EH.data
                    ,data=model.AUC.3D
                    ,screen=list(z = -200, x = -65, y = 5)
                    ,xlab= list('Window Size',cex=1.2,rot= -10)
                    ,ylab= list('Event Horizon',cex=1.2,rot=67)
                    ,zlim=zlimits
                    ,zlab=list('AUC',cex=1.2, rot=90)
                    ,drape = TRUE
                    ,colorkey = FALSE
                    ,scales = list(arrows=FALSE,cex=0.75,col="black",font=3,tck=1,x = list(labels=c(30,25,20,15,10,5)))
                    ,aspect = c(1, 0.65)
                    )
                    
print(auc.figure)
#dev.off()                    
 
plotFileName<-paste(sep='','H_Score_3D_',model.name,'.pdf')               
pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10,colormodel='srgb')
trellis.par.set(theme = col.whitebg())
trellis.par.set("axis.line", list(col="transparent"))
lattice.options(layout.widths = list(left.padding = list(x = 0.5, units = "inches")))
                   
zlimits<-c(0.0,0.08)
h_score.figure<-wireframe(H_score.data~WS.data*EH.data
                    ,data=model.H_score.3D
                    ,screen=list(z = -200, x = -65, y = 5)
                    ,xlab= list('Window Size',cex=1.2,rot= -10)
                    ,ylab= list('Event Horizon',cex=1.2,rot=67)
                    ,zlim=zlimits
                    ,zlab=list('H Score',cex=1.2, rot=90)
                    ,drape = TRUE
                    ,colorkey = FALSE
                    ,scales = list(arrows=FALSE,cex=0.75,col="black",font=3,tck=1)
                    ,aspect = c(1, 0.65)
                    )

print(h_score.figure)
dev.off()

# The two images together
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

plotFileName<-paste(sep='','Both_Scores_3D_',model.name,'.pdf')               
pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10,colormodel='srgb')
trellis.par.set(theme = col.whitebg())
trellis.par.set("axis.line", list(col="transparent"))
lattice.options(layout.widths = list(left.padding=list(x=0.5,units="inches")))
 


zlimits<-c(0.4,0.8)
auc.title<-paste(sep='','AUC ',model.name,' Model')
auc.figure<-wireframe(AUC.data~WS.data*EH.data
                    ,data=model.AUC.3D
                    ,screen=list(z = -200, x = -65, y = 5)
                    ,xlab= list('Window Size',cex=0.8,rot= -10)
                    ,ylab= list('Event Horizon',cex=0.8,rot=67)
                    ,zlim=zlimits
                    ,zlab=list('AUC',cex=0.8, rot=90)
                    ,drape = TRUE
                    ,colorkey = FALSE
                    ,scales = list(arrows=FALSE,cex=0.60,col="black",font=3,tck=1.2)
                    ,aspect = c(1, 0.65)
                    ,main=list(auc.title,cex=0.8,font=1)
                    ,zoom=0.8
                    ,par.settings = theme.novpadding
                    )
                    
                    
zlimits<-c(0.0,0.08)
h_score.title<-paste(sep='','H Score ',model.name,' Model')
h_score.figure<-wireframe(H_score.data~WS.data*EH.data
                    ,data=model.H_score.3D
                    ,screen=list(z = -200, x = -65, y = 5)
                    ,xlab= list('Window Size',cex=0.8,rot= -10)
                    ,ylab= list('Event Horizon',cex=0.8,rot=67)
                    ,zlim=zlimits
                    ,zlab=list('H Score x 10',cex=0.8, rot=90)
                    ,drape = TRUE
                    ,colorkey = FALSE
                    ,scales = list(arrows=FALSE,cex=0.60,col="black",font=3,tck=1.2)
                    ,aspect = c(1, 0.65)
                    ,main=list(h_score.title,cex=0.8,font=1)
                    ,zoom=0.8
                    ,par.settings = theme.novpadding
                    )
                                                           
print(auc.figure, position=c(0,0,0.5,1),more=TRUE)
print(h_score.figure, position=c(0.5,0,1,1))

# print(auc.figure, position=c(0,0.5,1,1),more=TRUE)
# print(h_score.figure, position=c(0,0,1,0.5))

dev.off()
