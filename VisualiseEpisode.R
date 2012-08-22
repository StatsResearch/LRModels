# Script for Visualising an Episode
# SVN: $Id: VisualiseEpisode.R 662 2012-06-24 22:00:51Z rob $


library('RSQLite')
library('lubridate')

PlotChannel<-function(buffer,label,trace.col=black,min=min(buffer),max=max(buffer),
                                    precursor.mins,after.mins,plot.bw,subwindow.shade,event.horizon.shade)
{
    timeIndex<-seq((-1*precursor.mins),after.mins)
    plot(timeIndex,buffer,type='l',col=trace.col,lwd=1.5, ylim=c(min,max),ylab=label,xlab='')
    if(event.horizon.shade == TRUE)
    {
        polygon(x=c(-10,-10,0,0),y=c(min,max,max,min),col='gray50',density=20,angle=45)
        lines(timeIndex,buffer,type='l',col=trace.col,lwd=1.5)
    }
    if(subwindow.shade == TRUE)
    {
        polygon(x=c(-20,-20,-10,-10),y=c(min,max,max,min),col='gray70')
        polygon(x=c(-30,-30,-20,-20),y=c(min,max,max,min),col='gray80')
        polygon(x=c(-40,-40,-30,-30),y=c(min,max,max,min),col='gray90')
        lines(timeIndex,buffer,type='l',col=trace.col,lwd=1.5)
    }
    pt.col<-'green'
    if(plot.bw == TRUE)
    {
        pt.col<-'black'
    }
    points(timeIndex,buffer,pch=20,col=pt.col)
    abline(v=0, lty='solid',lwd=2)
    abline(v=-10, lty='dotted')
}

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up logging
source('UtilScripts/Logging.R')
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/VisualiseEpisode.log')
log.level<-3

setwd("./BaseSetGenerator")
source('DBSupport.R')
source('EpisodeSupport.R')

setwd("../../BrainIT/EventDetectionDir")



#db.name<-'PDB-3705040.sdb'

### Lines to produce Fig 2.2 in my thesis
thesis.plotfilename<-'/Users/rob/PhDStuff/RobDonaldThesis/MedicalBackground/images/Episode-1-73704046.pdf'
db.name<-'PDB-73704046.sdb'
PlotEpisode(db.name,episode.number=1
                ,save.plot=TRUE
                ,plot.filename=thesis.plotfilename
                ,event.horizon.shade=TRUE)

### Lines to produce Fig 2.3 in my thesis
thesis.plotfilename<-'/Users/rob/PhDStuff/RobDonaldThesis/MedicalBackground/images/Episode-1-73704046-120mins.pdf'
db.name<-'PDB-73704046.sdb'

                
PlotEpisode<-function(db.name,episode.number,save.plot=FALSE,plot.filename='PlotFile.pdf',
                    fixed.ylims=FALSE,precursor.mins=40,after.mins=10,
                    plot.bw=FALSE,add.title=TRUE,
                    subwindow.shade=FALSE,event.horizon.shade=FALSE
                    ,remove.BPp = FALSE
                    ,write.buffer = FALSE)
{

    # create a SQLite instance and create one connection.
    sqlite.drv <- dbDriver("SQLite")
    con <- dbConnect(sqlite.drv, dbname = db.name)
    
    all.phys<-GetPhysData(con)
    
    all.demographic<-GetPatientData(con)
    
    dbDisconnect(con)
    
    attach(all.phys) ### ATTACH
        
        #data.4research<-data.frame(stringsAsFactors = FALSE,Time_Stamp,HRT,BPs,BPd,BPm,TC,ICPm,SaO2,ETCO2)
        # Calculate the pulse pressure BPs - BPd
        BPp<-(BPs-BPd)
        data.4research<-data.frame(stringsAsFactors = FALSE,Time_Stamp,HRT,BPs,BPd,BPm,BPp)
    
    detach(all.phys) ### DETACH
    
    # Convert the Time_Stamp strings to data objects
    timetag<-as.POSIXct(strptime(all.phys$Time_Stamp,'%Y-%m-%d %H:%M'))
     
    working.data<-data.frame(timetag,data.4research)
    
    db.stem<-substr(db.name,1,nchar(db.name)-4)
    episodeJSON<-paste(sep='',db.stem,'-EpiDtls.json')
    episode.times<-LoadEpisodeTimes(episodeJSON)
    
    
    # episode.start.date<-'2004-07-18T19:54'
#     epi.start<-as.POSIXct(strptime(episode.start.date,'%Y-%m-%dT%H:%M'))

    epi.start<-episode.times$starts[episode.number]
    precursor.start<-epi.start-dminutes(precursor.mins)
    
    just.past.start<-epi.start+dminutes(after.mins)
    
    # Note the condition to limit the rows but then the comma to give you all columns
    working.buffer<-working.data[(timetag>= precursor.start & timetag <= just.past.start),]

    imageWidth<-150/25.4
    imageHeight<-200/25.4
    #dpiRes<-600
    bitMapDeviceID<-0
    
    
    if(save.plot == TRUE)
    {
        #plotFileName<-paste(sep='',INSTALL_DIR,
        #            '/PhDStuff/ThesisSoftware/LRModels/Episode-',episode.number,'-',db.stem,'.png')
        #bitmap(plot.filename,width=imageWidth,height=imageHeight,res= dpiRes,units='mm')
        pdf(plot.filename,width=imageWidth,height=imageHeight)
        bitMapDeviceID<-dev.cur()
        LogWrite(paste(sep='','Opened: ',bitMapDeviceID),0)
    }
    
    if(remove.BPp == TRUE)
    {
        par(mfrow=c(4,1), mar=c(2,6.1,2.5,4.1),oma=c(3,1,1,1),font.main=1,cex.main=1.2,cex.lab=2.5,cex.axis=1.5)
    }
    else
    {
        par(mfrow=c(5,1), mar=c(2,6.1,2.5,4.1),oma=c(3,1,1,1),font.main=1,cex.main=1.2,cex.lab=2.5,cex.axis=1.5)
    }
    
    # The default is to auto-range
    ylim.factor<-0.1
    HRT.min<-floor(min(working.buffer$HRT - (ylim.factor*working.buffer$HRT)))
    HRT.max<-ceiling(max(working.buffer$HRT + (ylim.factor*working.buffer$HRT)))
    
    BPs.min<-floor(min(working.buffer$BPs - (ylim.factor*working.buffer$BPs)))
    if(BPs.min > 90)
    {
        BPs.min = 80
    }
    BPs.max<-ceiling(max(working.buffer$BPs + (ylim.factor*working.buffer$BPs)))
    
    BPd.min<-floor(min(working.buffer$BPd - (ylim.factor*working.buffer$BPd)))
    BPd.max<-ceiling(max(working.buffer$BPd + (ylim.factor*working.buffer$BPd)))
    
    BPm.min<-floor(min(working.buffer$BPm - (ylim.factor*working.buffer$BPm)))
    BPm.max<-ceiling(max(working.buffer$BPm + (ylim.factor*working.buffer$BPm)))
    
    BPp.min<-floor(min(working.buffer$BPp - (ylim.factor*working.buffer$BPp)))
    BPp.max<-ceiling(max(working.buffer$BPp + (ylim.factor*working.buffer$BPp)))

    if(fixed.ylims == TRUE)
    {
        HRT.min<-30
        HRT.max<-70
        
        BPs.min<-80
        BPs.max<-160
        
        BPd.min<-40
        BPd.max<-90
        
        BPm.min<-50
        BPm.max<-100
        
        BPp.min<-30
        BPp.max<-150
    }

    HRT.col='red'
    BPs.col='blue'
    BPd.col='orange'
    BPm.col='green'
    BPp.col='black'
    
    if(plot.bw == TRUE)
    {
        HRT.col='black'
        BPs.col='black'
        BPd.col='black'
        BPm.col='black'
        BPp.col='black'    
    }

    PlotChannel(working.buffer$HRT,'HRT',HRT.col,HRT.min,HRT.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
    if(add.title == TRUE)
    {
        epi.start.str<-strftime(epi.start,'%Y-%m-%dT%H:%M')
        mtext(paste(sep='','Episode ',episode.number,' : ',db.stem,' [t0=',epi.start.str,']'),line=1)
    }
    
    PlotChannel(working.buffer$BPs,'BPs',BPs.col,BPs.min,BPs.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
    abline(h=90,lty='dotdash')
    PlotChannel(working.buffer$BPd,'BPd',BPd.col,BPd.min,BPd.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
    PlotChannel(working.buffer$BPm,'BPm',BPm.col,BPm.min,BPm.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
    abline(h=70,lty='dotdash')
    
    if(remove.BPp == FALSE)
    {   
        PlotChannel(working.buffer$BPp,'BPp',BPp.col,BPp.min,BPp.max,precursor.mins,after.mins,plot.bw,
                                                                           subwindow.shade,event.horizon.shade)
    }
    
    if(save.plot == TRUE)
    {
        LogWrite(paste(sep='','Closing: ',bitMapDeviceID),0)
        dev.off(which=bitMapDeviceID)
    }
    
    if(write.buffer == TRUE)
    {
        working.buffer
    
    }
    
}

# PlotEpisode(db.name,episode.number=1
#                 ,save.plot=TRUE
#                 ,plot.filename=thesis.plotfilename
#                 ,precursor.mins=120
#                 ,subwindow.shade=TRUE
#                 ,event.horizon.shade=TRUE)


