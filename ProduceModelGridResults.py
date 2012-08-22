# Python Script to process the results of the model building

# SVN: $Id: ProduceModelGridResults.py 667 2012-07-02 21:24:21Z rob $

import getopt
import sys

import os
import logging 
import glob

from time import strftime
from datetime import datetime
from datetime import timedelta 

import json
from pprint import pprint


def MakeTableHeader():
    sb=[]
    sb.append('\\begin{table}[H]' + '\n')
    sb.append('\\begin{center}' + '\n')                
    sb.append('	{\\small' + '\n')                    
    sb.append('	\\begin{tabular}{ScScScScScScSc}' + '\n')
    sb.append('	\\toprule' + '\n')
    sb.append('             Event         & \\multicolumn{6}{Sc}{Window Size}\\tabularnewline' + '\n')
    sb.append('	         Horizon & 5 & 10 & 15 & 20 & 25 & 30  \\tabularnewline'  + '\n')
    sb.append('	\\hline \\\\ [-2.5ex]' + '\n\n')
    
    return(''.join(sb))


def MakeTableClose(score_type):

    sb2=[]
    sb2.append('	\\bottomrule' + '\n')
    sb2.append('	\\end{tabular}' + '\n')
    sb2.append('	} % End of Small' + '\n')
    sb2.append('	\\caption[ROC Assessment, Model: ' + modelName + ', ' + score_type +'] {ROC assessment results for Model: ' + modelName + '. Each cell represents ')
    sb2.append('the average ' + score_type + ' for 10 iterations with (std dev).}'  + '\n')
    sb2.append('	\\label{tbl:' + modelName + '-dataSetsResults-'+ score_type.replace(' ','-') +'}' + '\n')	
    sb2.append('\\end{center}' + '\n')
    sb2.append('\\end{table}' + '\n')
    
    return(''.join(sb2))

# The main code
INSTALL_DIR = '/Users/rob'
LOGGING_DIR = INSTALL_DIR + '/PhDStuff/ThesisSoftware/Logs'
LOG_FILE = LOGGING_DIR + '/PMGR.log'

# Get the command line options
def usage():
    print('ProduceModelGridResults.py -m, --MN=<model name> ' 
                + '--help')
                
    print 'Long Options (--) *must* have no space after \'=\' sign'
    print 'example ProduceModelGridResults.py --MN=FullQuadMean'
    print 'example ProduceModelGridResults.py -m FullQuadMean'
try:
    opts, args = getopt.getopt(sys.argv[1:], "m:h", ['MN=','help'])
except getopt.GetoptError, err:
    #print help information and exit:
    print str(err) # will #print something like "option -a not recognized"
    usage()
    sys.exit(2)
    
input_modelName = 'NOT-SET'

for option, val in opts:
    if option in ("-m", "--MN"):
        input_modelName = val
        #print 'val = ' + val
    elif option in ("-h", "--help"):
        usage()
        sys.exit()
    else:
        assert False, "unhandled option"


# USE FOR TESTING NEW COMMAND LINE OPTIONS sys.exit()
# ------ End of command line processing

logging.basicConfig(filename=LOG_FILE,level=logging.INFO,\
                format='%(asctime)s - %(levelname)s - [%(name)s] %(message)s')

log = logging.getLogger('PMGR')

start = datetime.now()
msg = 'Start: ' + str(start)
print msg
log.info(msg)

resultsTopLevel = INSTALL_DIR + '/PhDStuff/ThesisSoftware/LRModels'

#allModels=['Minimum','Full','FullQuadMean','FullQuadMeanSD']
allModels=[]
allModels.append(input_modelName)

allEventHorizons=[10,15,20,25,30]
allWindowSizes=[5,10,15,20,25,30]


# AUC.data<-c( 0.76, 0.71, 0.69, 0.68, 0.66, 0.64
#             ,0.68, 0.66, 0.64, 0.65, 0.61, 0.62 
#             ,0.64, 0.63, 0.61, 0.59, 0.59, 0.58 
#             ,0.60, 0.61, 0.57, 0.55, 0.54, 0.51
#             ,0.58, 0.55, 0.54, 0.53, 0.51, 0.48 
#            )

for modelName in allModels:

    print 'Processing model: ' + modelName

    outputDir = INSTALL_DIR + '/PhDStuff/ThesisSoftware/LRModels/Model_Info_' + modelName
    if not os.path.exists(outputDir):
        os.makedirs(outputDir)
        
    log.info('Created dir %s',outputDir)
 
    outAUC_FileName = outputDir + '/AUC_table_model_' + modelName + '.tex'
    out_AUC = open(outAUC_FileName,'w')
    
    outH_FileName = outputDir + '/H_score_table_model_' + modelName + '.tex'
    out_H = open(outH_FileName,'w')

    out_AUC.write(MakeTableHeader())
    out_H.write(MakeTableHeader())
    
    sb_auc = []
    sb_auc.append('AUC.data<-c(')
    
    sb_h_score = [] 
    sb_h_score.append('H_score.data<-c(')  

    for EH in allEventHorizons:
        
        out_AUC.write( str(EH) + ' & ')
        out_H.write( str(EH) + ' & ')
        
        for WS in allWindowSizes:
        
            fileDir = resultsTopLevel + '/output_' + str(EH) + '/MN_' + modelName 
            fileName = 'ROC_Juno_EH_' + str(EH) + '_' + str(WS) + '_Info.json'
            fullFile = fileDir + '/' + fileName                
                        
            print 'Processing JSON for ... ' + fileName
            
            json_data=open(fullFile)
            data = json.load(json_data)
            
            print 'mean.auc = ' + str(data['mean.auc'])
            print 'mean.auc = ' + "%.2f" % data['mean.auc']
            print 'mean.h_score = ' + "%.4f" % data['mean.h_score']
            out_AUC.write("%.2f" % data['mean.auc'])
            out_H.write("%.2f" % (float(data['mean.h_score'])*10))
            sb_auc.append("%.4f" % data['mean.auc'])
            sb_h_score.append("%.4f" % (float(data['mean.h_score'])*10))
            
            out_AUC.write(' (' + "%.2f" % data['sd.auc'] + ')')
            out_H.write(' (' + "%.2f" % (float(data['sd.h_score'])*10) + ')')
            
            if(WS == 30):
                out_AUC.write(' \\tabularnewline \n')
                out_H.write(' \\tabularnewline \n')
                if(EH == 30):
                    sb_auc.append(' ) \n ')
                    sb_h_score.append(' )  \n ')
                else:
                    sb_auc.append(' ,\n ')
                    sb_h_score.append(' ,\n ')
            else:
                out_AUC.write(' & ')
                out_H.write(' & ')
                sb_auc.append(' , ')
                sb_h_score.append(' , ')
            
            print 'sd.auc = ' + str(data['sd.auc'])
            print 'sd.auc = ' + "%.2f" % data['sd.auc']
            print 'sd.h_score = ' + "%.2f" % data['sd.h_score']
            json_data.close()
    
        next
        
    out_AUC.write('\n')
    out_H.write('\n')
    
    out_AUC.write(MakeTableClose('AUC'))
    out_AUC.close()
	
    out_H.write(MakeTableClose('H Score x 10'))
    out_H.close()
    
    
    # Now the AUC and H scores to a .R file
    out_R_FileName = outputDir + '/Output_AUC_H_Score_model_' + modelName + '.R'
    out_R = open(out_R_FileName,'w')
    
    out_R.write(''.join(sb_auc))
    out_R.write('\n\n')
    out_R.write(''.join(sb_h_score))
    out_R.write('\n\n')
    
    out_R.close()

end = datetime.now()
elapsed = end - start
msg = 'End: ' + str(end)
print msg
log.info(msg)

formattedTime = datetime.strftime(end, '%Y-%m-%dT%H:%M')
msg = 'formattedTime: ' + formattedTime
#print msg
log.info(msg)

msg = 'Elasped: ' + str(elapsed)
print msg
log.info(msg)