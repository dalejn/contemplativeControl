# Contemplative Control

Code for the paper, *Mindfulness Promotes Control of Brain Network Dynamics for Self-Regulation and Discontinues the Past from the Present*

## Setup

```
platform       x86_64-pc-linux-gnu         
arch           x86_64                      
os             linux-gnu                   
system         x86_64, linux-gnu           

language       R                           
version        R version 4.0.3 (2020-10-10)

MATLAB:        9.8.0.1451342 (R2020a)
```

### Functions

Download the [IoSR Matlab Toolbox](https://github.com/IoSR-Surrey/MatlabToolbox)
Download the 

Move to /scripts/

## Author

Dale Zhou (dalezhou [at] pennmedicine.upenn.edu)

## Project Organization

```

    ├── data                                    		 <- Data goes here.
    ├── figures                                 		 <- Figures for manuscript.
    ├── results											 <- Results/intermediate files.
    │   
    │ 
    │
    ├── fig2.R                                			 <- Average controllabiltiy vs. drinking
    ├── fig3_dataPrep.R                      			 <- Prep EMA data
    ├── fig3_controlAnalysis.m               			 <- Run network control and stability
    ├── fig3_analyzePlot.R                    			 <- Group diffs in control metrics
    ├── fig4.R   										 <- Timescale correlation with control metrics
    ├── supplement_controlMetrics.R             		 <- Correlations among control metrics
    ├── supplemental_lmAttentionImpulsivity.R            <- Attention impulsivity & emotion regulation
    ├── supplemental_multiLevelAttentionImpulsivity.R    <- Attention impulsivity & drinking
    │
    ├── README.md

```

## Order of scripts

1. fig2.R 
2. fig3_dataPrep.R
3. fig3_controlAnalysis.m 
4. fig3_analyzePlot.R    
5. fig4.R 
6. supplement_controlMetrics.R  
7. supplemental_lmAttentionImpulsivity.R 
8. supplemental_multiLevelAttentionImpulsivity.R

### Notes

Will need cluster to prepare inputs for fig4.R

qlogin -q qlogin.himem.q -l h_vmem=15G,s_vmem=15G