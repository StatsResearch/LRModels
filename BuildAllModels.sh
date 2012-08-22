#!/bin/bash

# SVN: $Id: BuildAllModels.sh 662 2012-06-24 22:00:51Z rob $

time ./RunCompleteModel.sh Full-Model-V1-20120414.R Full
time ./RunCompleteModel.sh FullQuadMean-Model-V1-20120414.R FullQuadMean
#time ./RunCompleteModel.sh FullQuadMeanSD-Model-V1-20120414.R FullQuadMeanSD
time ./RunCompleteModel.sh MinModel-V2-20120615.R Minimum
