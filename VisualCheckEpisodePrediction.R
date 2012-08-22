# Script for Visualising an Episode
# SVN: $Id: VisualCheckEpisodePrediction.R 667 2012-07-02 21:24:21Z rob $

library('RSQLite')
library('lubridate')

PlotChannel<-function(buffer,label,trace.col=black,min=min(buffer),max=max(buffer),
                                    precursor.mins,after.mins,plot.bw,subwindow.shade,event.horizon.shade)
{
    cat(label,'length(buffer) =',length(buffer),'\n')
    if(is.na(length(buffer)))
    {
        cat('No Data\n')
        return
    }
    
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

PlotPredChannel<-function(buffer,label,trace.col=black,min=min(buffer),max=max(buffer),
                                    precursor.mins,after.mins,plot.bw,subwindow.shade,event.horizon.shade)
{
    timeIndex<-seq((-1*precursor.mins),after.mins)
    plot(timeIndex,buffer,type='h',col=trace.col,lwd=1.5, ylim=c(min,max),ylab=label,xlab='')
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
    pt.col<-'red'
    if(plot.bw == TRUE)
    {
        pt.col<-'black'
    }
    points(timeIndex,buffer,pch=20,col=pt.col)
    abline(v=0, lty='solid',lwd=2)
    abline(v=-10, lty='dotted')
}


BuildPredictionVector<-function(timetags.str,predictions.dataframe)
{

    predictions<-vector()
    
    # Create a hashmap from the predictions.dataframe
    pred.hashmap<-list()
    
    pred.hashmap[as.character(predictions.dataframe$timetag)]<-predictions.dataframe$prediction
    
    for( count in 1:length(timetags.str) )
    {
        key<-timetags.str[count]
        val<-pred.hashmap[[key]][1]
        if(is.null(val) == TRUE)
        {
            #cat('Setting to NA\n')
            predictions[count]<-NA
        }
        else
        {
            predictions[count]<-val
        }
    }

    return(as.numeric(predictions))
}

PlotEpisode<-function(db.name,episode.number,save.plot=FALSE,plot.filename='PlotFile.pdf'
                    ,fixed.ylims=FALSE,precursor.mins=40,after.mins=10
                    ,plot.bw=FALSE,add.title=TRUE
                    ,subwindow.shade=FALSE,event.horizon.shade=FALSE
                    ,include.BPs=FALSE
                    ,WT=0.2
                    ,prediction.data
                    ,use.pred.data=TRUE)
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
    timetag.str<-strftime(timetag,'%Y-%m-%dT%H:%M')
    pred<-BuildPredictionVector(timetag.str,prediction.data)
     
    working.data<-data.frame(timetag,data.4research,pred)
    
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
    
    num.channels<-3
    if(include.BPs==TRUE)
    {
        num.channels<-4
    }
    
    par(mfrow=c(num.channels,1), mar=c(2,6.1,2.5,4.1),oma=c(3,1,1,1),font.main=1,cex.main=1.2,cex.lab=2.5,cex.axis=1.5)
    
    # The default is to auto-range
    ylim.factor<-0.1
    HRT.min<-floor(min(working.buffer$HRT - (ylim.factor*working.buffer$HRT)))
    HRT.max<-ceiling(max(working.buffer$HRT + (ylim.factor*working.buffer$HRT)))
    
    BPs.min<-floor(min(working.buffer$BPs - (ylim.factor*working.buffer$BPs)))
    cat('BPs.min =', BPs.min,'\n')
    if(is.na(BPs.min) | BPs.min > 90)
    {
        cat('BPs.min override\n')
        BPs.min = 80
        BPs.max = 160
    }
    else
    {
        BPs.max<-ceiling(max(working.buffer$BPs + (ylim.factor*working.buffer$BPs)))
    }
    
    BPd.min<-floor(min(working.buffer$BPd - (ylim.factor*working.buffer$BPd)))
    BPd.max<-ceiling(max(working.buffer$BPd + (ylim.factor*working.buffer$BPd)))
    
    
    BPm.min<-floor(min(working.buffer$BPm - (ylim.factor*working.buffer$BPm)))
    cat('BPm.min =', BPm.min,'\n')
    if(is.na(BPm.min) | BPm.min > 90)
    {
        cat('BPs.min override\n')
        BPm.min<-50
        BPm.max<-100
    }
    else
    {
        BPm.max<-ceiling(max(working.buffer$BPm + (ylim.factor*working.buffer$BPm)))
    }
    
    
    BPp.min<-floor(min(working.buffer$BPp - (ylim.factor*working.buffer$BPp)))
    BPp.max<-ceiling(max(working.buffer$BPp + (ylim.factor*working.buffer$BPp)))
    
    pred.min<-0
    
    pred.max<-0.5
    
    if(fixed.ylims == TRUE)
    {
        HRT.min<-10
        HRT.max<-100
        
        BPs.min<-80
        BPs.max<-160
        
        BPd.min<-40
        BPd.max<-90
        
        BPm.min<-50
        BPm.max<-100
        
        BPp.min<-30
        BPp.max<-150
        
        pred.min<-0
        pred.max<-1.0
    }

    HRT.col='red'
    BPs.col='blue'
    BPd.col='orange'
    BPm.col='green'
    BPp.col='black'
    pred.col='blue'
    
    if(plot.bw == TRUE)
    {
        HRT.col='black'
        BPs.col='black'
        BPd.col='black'
        BPm.col='black'
        BPp.col='black' 
        pred.col='black'
    }

    PlotChannel(working.buffer$HRT,'HRT',HRT.col,HRT.min,HRT.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
    if(add.title == TRUE)
    {
        epi.start.str<-strftime(epi.start,'%Y-%m-%dT%H:%M')
        mtext(paste(sep='','Episode ',episode.number,' : ',db.stem,' [t0=',epi.start.str,']'),line=1)
    }
    
    if(include.BPs == TRUE)
    {
        PlotChannel(working.buffer$BPs,'BPs',BPs.col,BPs.min,BPs.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
        abline(h=90,lty='dotdash')
    }

#     PlotChannel(working.buffer$BPd,'BPd',BPd.col,BPd.min,BPd.max,precursor.mins,after.mins,plot.bw,
#                                                                             subwindow.shade,event.horizon.shade)


    PlotChannel(working.buffer$BPm,'BPm',BPm.col,BPm.min,BPm.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
    abline(h=70,lty='dotdash')
    
#     PlotChannel(working.buffer$BPp,'BPp',BPp.col,BPp.min,BPp.max,precursor.mins,after.mins,plot.bw,
#                                                                             subwindow.shade,event.horizon.shade)

    PlotPredChannel(working.buffer$pred,'pred',pred.col,pred.min,pred.max,precursor.mins,after.mins,plot.bw,
                                                                            subwindow.shade,event.horizon.shade)
                                                                            
    abline(h=0.3,lty='solid',col='gray70')
    abline(h=WT,lty='solid',col='red')
    abline(h=0.1,lty='solid',col='gray70')
    
    if(save.plot == TRUE)
    {
        LogWrite(paste(sep='','Closing: ',bitMapDeviceID),0)
        dev.off(which=bitMapDeviceID)
    }
    
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

# Load in predictions

# # # Figure 6.8
# # setwd('/Users/rob/PhDStuff/ThesisSoftware/ICUDataStream/output/MN_Minimum_10_5')
# # pred.data<-read.table('ICU-DataStream-PDB-84884962.csv',sep=',',header=TRUE)
# # setwd('/Users/rob/PhDStuff/BrainIT/Rel_2011')
# # db.name<-'PDB-84884962.sdb'
# # pfn<-'/Users/rob/PhDStuff/RobDonaldThesis/ModelAssess/images/PDB-84884962-Epi-07.pdf'
# # PlotEpisode(db.name,7,prediction.data=pred.data,precursor.mins=120,WT=0.15,include.BPs=TRUE,save.plot=TRUE,plot.filename=pfn)

# Figure 6.8
setwd('/Users/rob/PhDStuff/ThesisSoftware/ICUDataStream/output/MN_Minimum_10_5')
pred.data<-read.table('ICU-DataStream-PDB-84884961.csv',sep=',',header=TRUE)
setwd('/Users/rob/PhDStuff/BrainIT/Rel_2011')
db.name<-'PDB-84884961.sdb'
pfn<-'/Users/rob/PhDStuff/RobDonaldThesis/ModelAssess/images/PDB-84884961-Epi-11.pdf'
PlotEpisode(db.name,11,prediction.data=pred.data,precursor.mins=60,WT=0.15,include.BPs=TRUE,save.plot=TRUE,plot.filename=pfn)

# # Figure 6.9
# setwd('/Users/rob/PhDStuff/ThesisSoftware/ICUDataStream/output/MN_Minimum_10_5')
# pred.data<-read.table('ICU-DataStream-PDB-84885068.csv',sep=',',header=TRUE)
# setwd('/Users/rob/PhDStuff/BrainIT/Rel_2011')
# db.name<-'PDB-84885068.sdb'
# pfn<-'/Users/rob/PhDStuff/RobDonaldThesis/ModelAssess/images/PDB-84885068-Epi-05-60.pdf'
# PlotEpisode(db.name,5,prediction.data=pred.data,precursor.mins=60,WT=0.15,include.BPs=TRUE,save.plot=TRUE,plot.filename=pfn)

# # Figure 6.10
# setwd('/Users/rob/PhDStuff/ThesisSoftware/ICUDataStream/output/MN_Minimum_10_5')
# pred.data<-read.table('ICU-DataStream-PDB-84885068.csv',sep=',',header=TRUE)
# setwd('/Users/rob/PhDStuff/BrainIT/Rel_2011')
# db.name<-'PDB-84885068.sdb'
# pfn<-'/Users/rob/PhDStuff/RobDonaldThesis/ModelAssess/images/PDB-84885068-Epi-05-180.pdf'
# PlotEpisode(db.name,5,prediction.data=pred.data,precursor.mins=180,WT=0.15,include.BPs=TRUE,save.plot=TRUE,plot.filename=pfn)

# setwd('/Users/rob/PhDStuff/ThesisSoftware/ICUDataStream/output/MN_Minimum_10_5')
# pred.data<-read.table('ICU-DataStream-PDB-84884958.csv',sep=',',header=TRUE)
# setwd('/Users/rob/PhDStuff/BrainIT/Rel_2011')
# db.name<-'PDB-84884958.sdb'
# #PlotEpisode(db.name,1,prediction.data=pred.data,WT=0.15,include.BPs=TRUE)
# 
# for(count in seq(1,40))
# {
#     dev.new()
#     print(count)
#     try(PlotEpisode(db.name,count,prediction.data=pred.data,WT=0.15,include.BPs=TRUE))
# }