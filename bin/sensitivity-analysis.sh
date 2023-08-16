#!/bin/bash

nohup Rscript sensitivity-analyses.r > sensitivity-analyses-r.out 2>&1

cd plots

nohup Rscript outcome-scaling-plot--atmo_damages.r "--atmo_damages-sensitivity-linear--maxit-251" > sensitivity-analysis--atmo_damages-sensitivity-linear.out 2>&1

nohup Rscript outcome-scaling-plot.r "--N-radius-preference-sensitivity--REVISION--maxit-251" > sensitivity-analysis--N-radius-preference-sensitivity--REVISION.out 2>&1

nohup Rscript wtp-sensitivity.r "--preference-sensitivity--REVISION--maxit-251" > sensitivity-analysis--preference-sensitivity--REVISION.out 2>&1
