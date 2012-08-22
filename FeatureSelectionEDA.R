library(sm)

INSTALL_DIR<-'/Users/rob'
setwd(paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/'))

# Set up timing code
source('UtilScripts/TimeUtils.R')
start<-now()

# Set up logging
source('UtilScripts/Logging.R')
log.filename<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/Logs/FeatureSelection.log')
log.level<-3

# Read in the command line arguments
LogWrite('LoopingLRModelsEDA.R Starting ...',3)
LogWrite('-- reading arguments',3)

cmd.args<-commandArgs(TRUE);
all.args<-'Command Line Args:'
for (arg in cmd.args)
{
    all.args<-paste(sep='', all.args, '  ', arg);
}

LogWrite(all.args,0)

WriteTableHeader<-function(filename,process)
{
    LogWrite(paste('WriteTableHeader:',process),0)
    zz <- file(filename, open="wt")
    sink(zz)
    #------#

    cat("{\\small\n")
        cat("\\begin{table}[H]\n")
		cat("\\begin{center}\n")
		
		cat("\\begin{tabular}{SlSrSrSr}\n")
		cat("\\toprule\n")

                cat("Measure & Diff & Pos Class & Neg Class \\tabularnewline \n")
            
		cat("\\hline\n")

    # back to the console
    sink()
    #------#
}

WriteTableClose<-function(filename,process,event.horizon,window.size)
{
    LogWrite(paste('WriteTableClose:',process),0)
    zz <- file(filename, open="at")
    sink(zz)
    #------#

    cat("\\bottomrule\n")
		cat("\\end{tabular}\n")
		cat(sep='',"\\caption{BDS ",event.horizon,'\\_', window.size,', Feature ', process, ' values}\n')
		cat(sep='',"\\label{tbl:BDSEH",event.horizon,'WS',window.size,process,"}\n")
		
		cat("\\end{center}\n")
	cat("\\end{table}\n")
	cat("} % End of Small\n")
		
    # back to the console
    sink()
    #------#
}

WriteTableRow<-function(filename,measure.name,process)
{
    LogWrite(paste('WriteTableRow:',measure),0)
    
    measure<-get(paste(sep='',measure.name,'_',process,'_',event.horizon,'_',window.size)) 
    
    #quartz()
    xlabel<-paste(sep='',measure.name,'; ',process,' values')
    densityCalc<-sm.density(measure[case_label==1],col='red',xlab=xlabel)
    modeCalc<-densityCalc$eval.points[order(densityCalc$estimate)]
    modeIndex<-length(modeCalc)
    modeLine.pos<-modeCalc[modeIndex]
    abline(v=modeLine.pos,lty=2)
    
    densityCalc<-sm.density(measure[case_label==0],add=T)
    modeCalc<-densityCalc$eval.points[order(densityCalc$estimate)]
    modeIndex<-length(modeCalc) 
    modeLine.neg<-modeCalc[modeIndex]
    abline(v=modeLine.neg,lty=2)
    
    diff<-modeLine.pos - modeLine.neg
    LogWrite(paste('diff = ',diff),0)
    
    zz <- file(filename, open="at")
    sink(zz)
    #------#
    if(abs(diff) > 2)
    {
        cat(measure.name, " & {\\bf{", sprintf('%.2f',diff), "}} & " ,sprintf('%.2f',modeLine.pos), " & ",sprintf('%.2f',modeLine.neg), "\\tabularnewline\n")
    }
    else
    {
        cat(measure.name, " & ", sprintf('%.2f',diff), " & " ,sprintf('%.2f',modeLine.pos), " & ",sprintf('%.2f',modeLine.neg), "\\tabularnewline\n")
    }
    # back to the console
    sink()
    #------#
}

TTG_DIR<-paste(sep='',INSTALL_DIR,'/PhDStuff/ThesisSoftware/TrgTestGenerator/output')

all.event.horizons<-10
all.window.sizes<-c(5,10,15)
all.processes<-c('mean','sd','slope')
all.measures<-c('HRT','BPs','BPd','BPm','BPp')

imageWidth<-95
imageHeight<-110
dpiRes<-600

for(event.horizon in all.event.horizons)
{
    for(process in all.processes)
    {

        
        for(window.size in all.window.sizes)
        {
            plotFileName<-paste(sep='',INSTALL_DIR,
                '/PhDStuff/RobDonaldThesis/Chp3/images/Features_',process,'_',event.horizon,'_',window.size,
                '.png')
            bitmap(plotFileName,width=imageWidth,height=imageHeight,res= dpiRes,units='mm')
            par(mfrow=c(3,2), mar=c(5.1,4.1,2.5,4.1),font.main=1,cex.main=1.2,cex.lab=1.5)
            
            test.filename<-paste(sep='', TTG_DIR,'/TTG_Test_',event.horizon,'_',window.size,'_seq_2.csv')
    
            all.test.raw<-read.table(test.filename,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
            all.test<-na.omit(all.test.raw)
    
            table.filename<-paste(sep='',INSTALL_DIR,
                '/PhDStuff/RobDonaldThesis/Chp3/tblsrc/FeaturesTbl_',process,'_',event.horizon,'_',window.size,
                '.tex')
            WriteTableHeader(table.filename,process)
            attach(all.test)
        
                for(measure in all.measures)
                {
                    WriteTableRow(table.filename,measure,process)
                }
                
            detach(all.test)
            WriteTableClose(table.filename,process,event.horizon,window.size)
            
            dev.off()

        }
    }
}   

end<-now()
timeInfo<-ElapsedTime(start,end)
LogWrite(unlist(timeInfo$printStr),0)


        




