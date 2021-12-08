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

setwd('/Users/dalezhou/Box/contemplativeControlCode/daleManuscriptCodeReplication/')
load('data/df_proc.RData')

############
# ANALYSIS #
############

summary(lm(bis_11_attention ~ ders_total + age + gender_numeric + race + educ_self_years,data=df_proc[which(df_proc$condition.x=="mindful" | df_proc$condition.x=='control'),]))

gam2 <- gam(formula = bis_11_attention ~ ders_total + age + gender_numeric + race + educ_self_years,data=df_proc[which(df_proc$condition.x=="mindful" | df_proc$condition.x=='control'),],fx=TRUE,method="REML", na.rm=T)
summary(gam2)
visreg(gam2,"ders_total",  gg=T)+theme_classic(base_size=20)+labs(y="Difficulties in Emotion Regulation", x="Attentional Impulsiveness (BIS-11)")