# 		         Horizon& 5 & 10 & 15 & 20 & 25 & 30  \tabularnewline
# 		\hline \\ [-2.5ex]
# 10 & 0.76 (0.01) & 0.71 (0.01) & 0.69 (0.02) & 0.68 (0.03) & 0.66 (0.02) & 0.64 (0.03) \tabularnewline 
# 15 & 0.68 (0.01) & 0.66 (0.01) & 0.64 (0.02) & 0.65 (0.02) & 0.61 (0.02) & 0.62 (0.03) \tabularnewline 
# 20 & 0.64 (0.02) & 0.63 (0.02) & 0.61 (0.02) & 0.59 (0.03) & 0.59 (0.02) & 0.58 (0.03) \tabularnewline 
# 25 & 0.60 (0.02) & 0.61 (0.02) & 0.57 (0.02) & 0.55 (0.03) & 0.54 (0.02) & 0.51 (0.04) \tabularnewline 
# 30 & 0.58 (0.01) & 0.55 (0.01) & 0.54 (0.02) & 0.53 (0.03) & 0.51 (0.02) & 0.48 (0.03) \tabularnewline 


EH.data<-c(rep(10,6),rep(15,6),rep(20,6),rep(25,6),rep(30,6))
WS.data<-rep(seq(5,30,by=5),5)

# AUC.data<-c( 0.76, 0.71, 0.69, 0.68, 0.66, 0.64
#             ,0.68, 0.66, 0.64, 0.65, 0.61, 0.62 
#             ,0.64, 0.63, 0.61, 0.59, 0.59, 0.58 
#             ,0.60, 0.61, 0.57, 0.55, 0.54, 0.51
#             ,0.58, 0.55, 0.54, 0.53, 0.51, 0.48 
#            )

AUC.data<-c(0.7635 , 0.7249 , 0.6853 , 0.6814 , 0.6430 , 0.6274 ,
 0.6975 , 0.6810 , 0.6451 , 0.6511 , 0.6019 , 0.5870 ,
 0.6627 , 0.6435 , 0.6186 , 0.5902 , 0.5926 , 0.5510 ,
 0.6189 , 0.6183 , 0.5903 , 0.5635 , 0.5402 , 0.5132 ,
 0.6116 , 0.5750 , 0.5716 , 0.5461 , 0.5173 , 0.5105 ) 
           
H_score.data<-c(0.0687 , 0.0410 , 0.0275 , 0.0261 , 0.0178 , 0.0130 ,
 0.0326 , 0.0267 , 0.0177 , 0.0187 , 0.0087 , 0.0082 ,
 0.0202 , 0.0159 , 0.0136 , 0.0097 , 0.0084 , 0.0045 ,
 0.0123 , 0.0130 , 0.0086 , 0.0054 , 0.0044 , 0.0031 ,
 0.0130 , 0.0076 , 0.0061 , 0.0035 , 0.0034 , 0.0021 )  
           
model.AUC.3D<-data.frame(EH.data,WS.data,AUC.data)
model.H_score.3D<-data.frame(EH.data,WS.data,H_score.data)

library(lattice)

setwd('~/PhDStuff/ThesisSoftware/LRModels')
imageWidth<-150/25.4
imageHeight<-150/25.4
plotFileName<-'Test3D.pdf'
                
#pdf(plotFileName,width=imageWidth,height=imageHeight,pointsize=10,colormodel='srgb')
par(mar=c(5.1,5.1,2.5,4.1),font.main=1)
trellis.par.set(theme = col.whitebg())
trellis.par.set("axis.line", list(col="transparent"))
lattice.options(layout.widths = list(left.padding = list(x = 0.5, units = "inches")))
#trellis.par.set("left.padding",list(x = 0.5, units = "inches"))

par(omi=c(0.1,0.1,0.05,0.1))
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
                    ,scales = list(arrows=FALSE,cex=0.75,col="black",font=3,tck=1)
                    ,aspect = c(1, 0.65)
                    ,zoom=0.8
                    ,trellis.par.set('omi',c(0.01,0.01,0.01,0.01))
                    )
                    
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
                    ,main=list('H Score Minimum Model',cex=0.8,font=1))
                    

#plot(fig1)
print(auc.figure)
#print(h_score.figure)

#dev.off()