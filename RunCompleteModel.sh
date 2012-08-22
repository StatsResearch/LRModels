#!/bin/bash
# SVN: $Id: RunCompleteModel.sh 609 2012-04-22 20:58:59Z rob $

if [ $# -lt 2 ]
then
	echo "usage: RunCompleteModel.sh <model file> <model name>"
	exit -1
else
    if [ $1 = "--help" ] 
    then
	    echo "usage: RunCompleteModel.sh <model file> <model name>"
	    exit -1
    fi
fi

modelFile=$1
modelName=$2

timeNow=`date +%Y%m%d%H%M%S`
echo Starting $timeNow ...
logFile=../Logs/RunCompleteModel-$timeNow.log

echo $modelName > $logFile
echo EH 10
Rscript Build_LRModels.R EH=10 model.file=$modelFile >> $logFile
echo EH 15
Rscript Build_LRModels.R EH=15 model.file=$modelFile >> $logFile
echo EH 20
Rscript Build_LRModels.R EH=20 model.file=$modelFile >> $logFile
echo EH 25
Rscript Build_LRModels.R EH=25 model.file=$modelFile >> $logFile
echo EH 30
Rscript Build_LRModels.R EH=30 model.file=$modelFile >> $logFile

echo ProduceModelGridResults
python ProduceModelGridResults.py -m $modelName >> $logFile

echo ModelScoring3D
Rscript ModelScoring3D.R model.name=$modelName >> $logFile

timeNow=`date +%Y%m%d%H%M%S`
echo ... $timeNow Complete

