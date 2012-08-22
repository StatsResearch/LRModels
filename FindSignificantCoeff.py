# Python Script to filter out incomplete lines in the all data BDS

# SVN: $Id: FindSignificantCoeff.py 626 2012-05-15 20:35:46Z rob $

import csv
import os
import glob
from time import strftime
from datetime import datetime
from datetime import timedelta 

start = datetime.now()
print 'Start: ' + str(start)

InstallDir = '/Users/rob'
inputDir = InstallDir + '/PhDStuff/ThesisSoftware/LRModels'

res_fileName = 'all-data-results.txt'
res_fullFile = inputDir + '/' + res_fileName

print 'Processing: ' + res_fileName

res_file = open(res_fullFile)
lineCount = 0

# completeLen = 0
# removedLineCount = 0
# removedPosCaseCount = 0
# storedPosCaseCount = 0
    
for line in res_file:
    lineCount += 1

    if 'Sequence:' in line:
        print('\n' + line[:-1]) 
    elif 'glm()' in line:
        #print(str(lineCount) + ' : ' + line[:-1]) 
        print(line[:-1]) 
    elif '  >>>' in line:
            print(line[:-1]) 
    elif 'glmnet' in line:
        #print(str(lineCount) + ' : ' + line[:-1]) 
        if lineCount > 6100:
            print('\n' + line[:-1]) 
        else:
            print(line[:-1]) 
    elif 'Seq:' in line:
        print('+++ ' + line[:-1]) 
    else:
        # split on 6 chars in case of -ve sign
        dataTokens = line.split('      ') 
        if(len(dataTokens) == 2):
            
            try:
                value=float(dataTokens[1][:-1])
            except:
                value = 0.0
            
            if value != 0:
                if 'Intercept' not in line:
                    if '---' not in line:
                        print('Significant:' + line[:-1]) 
                

            
                

# print ''
# print 'lineCount: ' + str(lineCount)
# print 'storedPosCaseCount: ' + str(storedPosCaseCount)
# print 'Total Pos Cases: ' + str(removedPosCaseCount + storedPosCaseCount)
# 
# rlcProp = float(removedLineCount)/float(lineCount)
# print 'removedLineCount: %d (%.4f)' % (removedLineCount, rlcProp) 
# rpcPropTotalLines = float(removedPosCaseCount)/float(lineCount)
# ppcPropPosCases = float(removedPosCaseCount)/float(removedPosCaseCount + storedPosCaseCount)
# print 'removedPosCaseCount: %d (%.4f of total lines), (%.4f of pos cases)' % (removedPosCaseCount, rpcPropTotalLines, ppcPropPosCases) 
# print ''
end = datetime.now()
elapsed = end - start
print 'End: ' + str(end)

formattedTime = datetime.strftime(end, '%Y-%m-%dT%H:%M')
print 'formattedTime: ' + formattedTime
print 'Elasped: ' + str(elapsed)
print '... Processing complete' 