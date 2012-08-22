#!/bin/bash
# SVN: $Id$

if [ $# -gt 1 ]
then
	echo "usage: BuildFourModelSummaryPlots.sh "
	exit -1
else
    if [ "$1" == "--help" ] 
    then
	    echo "usage: BuildFourModelSummaryPlots.sh"
	    exit -1
    fi
fi

timeNow=`date +%Y%m%d%H%M%S`
echo Starting $timeNow ...
logFile=../Logs/BuildFourModelSummaryPlots-$timeNow.log

echo FourModelScoring3D > $logFile
echo FourModelScoring3D
Rscript FourModelScoring3D.R >> $logFile

echo EH_10_Slice_AUC_H_Score > $logFile
echo EH_10_Slice_AUC_H_Score
Rscript EH_10_Slice_AUC_H_Score.R >> $logFile

echo EH_10_EH_15_Slice_AUC > $logFile
echo EH_10_EH_15_Slice_AUC
Rscript EH_10_EH_15_Slice_AUC.R >> $logFile

echo EH_10_EH_15_Slice_AUC > $logFile
echo EH_10_EH_15_Slice_AUC
Rscript EH_10_Model_AUC_Comparison.R >> $logFile


timeNow=`date +%Y%m%d%H%M%S`
echo ... $timeNow Complete

