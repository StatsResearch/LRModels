#!/bin/bash

# SVN: $Id: BuildAllFeatureGrids.sh 662 2012-06-24 22:00:51Z rob $

selectionThreshold=0.6

time python ProduceFeatureSelectionGrid.py -m Full -s $selectionThreshold
time python ProduceFeatureSelectionGrid.py -m FullQuadMean -s $selectionThreshold
time python ProduceFeatureSelectionGrid.py -m Full-Lasso -s $selectionThreshold
time python ProduceFeatureSelectionGrid.py -m Minimum -s $selectionThreshold



