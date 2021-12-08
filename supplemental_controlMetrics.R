rm(list=ls())

library(ggplot2)
library(ppcor)
library(mgcv)
library(visreg)
library(scatterplot3d)
library(metR)
library(imager)
library(tidyverse)
library(lmerTest)
library(lavaan)
library(car)
library(data.table)
library(raincloudplots)

setwd('/Users/dalezhou/Box/contemplativeControlCode/daleManuscriptCodeReplication/')
load('data/df_proc.RData')

optControl = read.csv('results/costFlex_ind_rest400.txt', header=F)
colnames(optControl) = c('pID', 'optControl')
df_proc <- merge(df_proc, optControl, by='pID')

avecont_regional <- as.vector(read.csv('results/ave_cont_regional.txt', header=F))
optcont_regional <- as.vector(read.csv('results/opt_cont_regional.txt', header=F))
stability_regional <- as.vector(read.csv('results/stability_cont_regional.txt', header=F))
yeo_index <- as.vector(read.csv('data/yeo_index_3_4_6.txt', header=F))

df_regional <- as.data.frame(cbind(as.vector(unlist(avecont_regional)), as.vector(unlist(optcont_regional)), as.vector(unlist(stability_regional)), as.vector(yeo_index)))
colnames(df_regional) <- c('avecont', 'optcont', 'stability', 'yeo_index')
df_regional$yeo_index <- as.factor(df_regional$yeo_index)

ggplot(data=df_regional, aes(x=avecont, y=optcont)) + geom_point() + geom_smooth(method='lm') + theme_classic()
cor.test(df_regional$avecont, df_regional$optcont, method='spearman')

ggplot(data=df_regional, aes(x=avecont, y=stability)) + geom_point() + geom_smooth(method='lm') + theme_classic()
cor.test(df_regional$avecont, df_regional$stability, method='spearman')

ggplot(data=df_regional, aes(x=optcont, y=stability)) + geom_point() + geom_smooth(method='lm') + theme_classic()
