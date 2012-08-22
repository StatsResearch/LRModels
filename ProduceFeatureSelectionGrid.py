# Python Script to process the feature selection output of the model building

# SVN: $Id: ProduceFeatureSelectionGrid.py 669 2012-07-17 19:26:21Z rob $

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
    sb.append('	\\begin{tabular}{SlSlScScScScScSc}' + '\n')
    sb.append('	\\toprule' + '\n')
    sb.append('             Event &        & \\multicolumn{6}{Sc}{Window Size}\\tabularnewline' + '\n')
    sb.append('	         Horizon & signal & 5 & 10 & 15 & 20 & 25 & 30  \\tabularnewline'  + '\n')
    sb.append('	\\hline \\\\ [-2.5ex]' + '\n\n')
    
    return(''.join(sb))


def MakeTableClose(modelName,signal):

    sb2=[]
    sb2.append('	\\bottomrule' + '\n')
    sb2.append('	\\end{tabular}' + '\n')
    sb2.append('	} % End of Small' + '\n')
    sb2.append('	\\caption[Feature Selection, Model: ' + modelName + ', Signal: ' + signal + '] {Feature selection results for Model: ' + modelName + ', signal: ' + signal + '. Each cell represents ')
    sb2.append('the proportion of significant hits within 10 iterations.}'  + '\n')
    sb2.append('	\\label{tbl:' + modelName + '-featureSelection-'+ signal +'}' + '\n')	
    sb2.append('\\end{center}' + '\n')
    sb2.append('\\end{table}' + '\n')
    
    return(''.join(sb2))
    
# The main code
INSTALL_DIR = '/Users/rob'
LOGGING_DIR = INSTALL_DIR + '/PhDStuff/ThesisSoftware/Logs'
LOG_FILE = LOGGING_DIR + '/PFSR.log'

# Get the command line options
def usage():
    print('ProduceFeatureSelectionGrid.py -m, --MN=<model name> ' 
                + '--help')
                
    print 'Long Options (--) *must* have no space after \'=\' sign'
    print 'example ProduceFeatureSelectionGrid.py --MN=FullQuadMean'
    print 'example ProduceFeatureSelectionGrid.py -m FullQuadMean'
try:
    opts, args = getopt.getopt(sys.argv[1:], "m:s:h", ['MN=','ST=','help'])
except getopt.GetoptError, err:
    #print help information and exit:
    print str(err) # will #print something like "option -a not recognized"
    usage()
    sys.exit(2)
    
input_modelName = 'NOT-SET'
selectionThreshold = 0.5

for option, val in opts:
    if option in ("-m", "--MN"):
        input_modelName = val
        #print 'val = ' + val      
    elif option in ("-s", "--ST"):
        selectionThreshold = float(val)
    elif option in ("-h", "--help"):
        usage()
        sys.exit()
    else:
        assert False, "unhandled option"


# USE FOR TESTING NEW COMMAND LINE OPTIONS sys.exit()
# ------ End of command line processing

logging.basicConfig(filename=LOG_FILE,level=logging.INFO,\
                format='%(asctime)s - %(levelname)s - [%(name)s] %(message)s')

log = logging.getLogger('PFSG')

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

allSignals=[]
allProcesses=[]

#allSignals=['(Intercept)','Age','Sex']
if 'Full' in allModels:
    allSignals=['HRT','BPs','BPd','BPm','BPp']
    allProcesses=['spot','mean','sd','slope']

if 'Full-Lasso' in allModels:
    allSignals=['HRT','BPs','BPd','BPm','BPp']
    allProcesses=['spot','mean','sd','slope']

if 'FullQuadMean' in allModels:
    allSignals=['HRT','BPs','BPd','BPm','BPp']
    allProcesses=['spot','mean','mean^2','sd','slope']
    
if 'FullQuadMeanSD' in allModels:
    allSignals=['HRT','BPs','BPd','BPm','BPp']
    allProcesses=['spot','mean','mean^2','sd','sd^2','slope']
    
if 'Minimum' in allModels:
    allSignals=['HRT','BPm']    
    allProcesses=['mean','sd']
    



for modelName in allModels:

    print 'Processing model: ' + modelName

    outputDir = INSTALL_DIR + '/PhDStuff/ThesisSoftware/LRModels/Model_Info_' + modelName
    if not os.path.exists(outputDir):
        os.makedirs(outputDir)
        
    log.info('Created dir %s',outputDir)
    
    tickCount = 0
    for signal in allSignals:
    
        outFileName = outputDir + '/Output_table_feature_selection_model_' + modelName.replace('.','_') + '_' + signal + '.tex'
        out = open(outFileName,'w')
        out.write('%Data for Signal: ' + signal + '\n')

        out.write(MakeTableHeader())
        
        for EH in allEventHorizons:
        
            EH_printed = False
            for process in allProcesses:

                if '^2' in process:
                    #I(BPs_mean^2)
                    processSignal = 'I(' + signal + '_' + process + ')'
                else:
                    processSignal = signal + '_' + process
                    
                #print 'Signal to look for = ' + processSignal
            
                for WS in allWindowSizes:
                
                    fileDir = resultsTopLevel + '/output_' + str(EH) + '/MN_' + modelName 
                    fileName = 'ROC_Juno_EH_' + str(EH) + '_' + str(WS) + '_Info.json'
                    fullFile = fileDir + '/' + fileName                
                                
                    #print 'Processing JSON for ... ' + fileName
                    # Some UI feedback
                    tickCount += 1
                    if tickCount % 10 == 0:
                        print '.',
                    if tickCount % 100 == 0:
                        print '\n',
                    
                    
                    json_data=open(fullFile)
                    data = json.load(json_data)
                    
                    numRepeats = data['num.repeats']
                    #print 'numRepeats = ' + str(numRepeats)
                    
                    selectionTotals = data['selection.totals.model']
                            
                    for count in range(len(selectionTotals)):
                        currentSignal = selectionTotals[count]['signals']
                        
                        if (currentSignal == processSignal):
                            #print 'Found : ' + currentSignal
                            #print 'WS = ' + str(WS)
                            if(WS == 5): 
                                if (EH_printed == False):              
                                    out.write( str(EH) + ' & ' + process + ' & ')
                                    EH_printed = True
                                else:
                                    out.write( '  ' + ' & ' + process.replace('^','') + ' & ')
                            
                                
                            #print selectionTotals[count]['selection.totals']
                            value = float(selectionTotals[count]['selection.totals'])/numRepeats
                            
                            #print 'selection = ' + "%.2f" % value
                            
                            if(value >= selectionThreshold):
                                out.write('{\\large\\bf{' + "%.2f" % value + '}}')
                            else:
                                if(value < 0.001):
                                    out.write(' --- ')
                                else:
                                    out.write("%.2f" % value)
                            
                            if(WS == 30):
                                #print 'End of line \\tabularnewline'                              
                                if(process == 'slope'):
                                    out.write(' \\tabularnewline\\\\ \n')
                                else:
                                    out.write(' \\tabularnewline \n')
                            else:
                                out.write(' & ')
                                break
    
                json_data.close()
        
            next
        
        out.write(MakeTableClose(modelName,signal))    
        out.write('\n')
        out.close()
    
    print '... Processing complete'
