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
library(raincloudplots) # need to install from GitHub https://github.com/jorvlan/raincloudplots

setwd('/Users/dalezhou/Box/contemplativeControlCode/daleManuscriptCodeReplication/')
load('data/df_proc.RData')

############
# ANALYSIS #
############

# load data
energy_cost_zero_to_control <- read.csv('results/energy_cost_zero_to_control.txt', sep=' ', header=F)
energy_cost_zero_to_mindfulness <- read.csv('results/energy_cost_zero_to_mindfulness.txt', sep=' ', header=F)
energy_cost_zero_to_mindfulness_natural_react <- read.csv('results/energy_cost_zero_to_mindfulness_natural_react.txt', sep=' ', header=F)

stability_control_react <-read.csv('results/stability_control_react.txt', sep=' ', header=F)
stability_mindfulness_alc_react <-read.csv('results/stability_mindfulness_alc_react.txt', sep=' ', header=F)
stability_mindfulness_downreg <-read.csv('results/stability_mindfulness_downreg.txt', sep=' ', header=F)

########################################################################
# energy for between-group control alc react <-> mindfulness alc react #
########################################################################

df_plot <- as.data.frame(cbind(as.numeric(c(energy_cost_zero_to_control, energy_cost_zero_to_mindfulness_natural_react)), c(rep('energy_cost_zero_to_control', 145), rep('energy_cost_zero_to_mindfulness_natural_react', 145)), paste0("region_", 1:145)))
colnames(df_plot) <- c('energy', 'condition', 'region')
df_plot$energy <- log10(as.numeric(df_plot$energy))
df_plot$condition <- as.factor(df_plot$condition)
df_plot$region <- as.factor(df_plot$region)

df_1x1 <- data_1x1(
  array_1 = df_plot$energy[which(df_plot$condition=="energy_cost_zero_to_control")],
  array_2 = df_plot$energy[which(df_plot$condition=="energy_cost_zero_to_mindfulness_natural_react")],
  jit_distance = 0.09,
  jit_seed = 321
)

raincloud_2_aligned <- raincloud_1x1_repmes(
  data = df_1x1,
  colors = (c('dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange')),
  line_color = 'gray',
  line_alpha = .3,
  size = 1,
  alpha = .6,
  align_clouds = TRUE) +
  
  scale_x_continuous(breaks=c(1,2), labels=c("energy_cost_zero_to_control", "energy_cost_zero_to_mindfulness_natural_react"), limits=c(0, 3)) +
  xlab("Time") + 
  ylab("Score") +
  theme_classic()

raincloud_2_aligned

cairo_ps(file = "figs/energyControl_energyMindfulNaturalReact.eps", onefile = FALSE, fallback_resolution = 600)
raincloud_2_aligned
dev.off()

t.test(as.numeric(log10(energy_cost_zero_to_control)), as.numeric(log10(energy_cost_zero_to_mindfulness_natural_react)))

##########################################################################################################
# stability for between-group control condition natural react versus mindfulness condition natural react #
##########################################################################################################

df_plot <- as.data.frame(cbind(as.numeric(c(stability_control_react, stability_mindfulness_alc_react)), c(rep('stability_control_react', 145), rep('stability_mindfulness_alc_react', 145)), paste0("region_", 1:145)))
colnames(df_plot) <- c('energy', 'condition', 'region')
df_plot$energy <- log10(as.numeric(df_plot$energy))
df_plot$condition <- as.factor(df_plot$condition)
df_plot$region <- as.factor(df_plot$region)

df_1x1 <- data_1x1(
  array_1 = 1/(df_plot$energy[which(df_plot$condition=="stability_control_react")]),
  array_2 = 1/(df_plot$energy[which(df_plot$condition=="stability_mindfulness_alc_react")]),
  jit_distance = 0.09,
  jit_seed = 321
)

raincloud_2_aligned <- raincloud_1x1_repmes(
  data = df_1x1,
  colors = (c('dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange')),
  line_color = 'gray',
  line_alpha = .3,
  size = 1,
  alpha = .6,
  align_clouds = TRUE) +
  
  scale_x_continuous(breaks=c(1,2), labels=c("stability_control_react", "stability_mindfulness_alc_react"), limits=c(0, 3)) +
  xlab("Time") + 
  ylab("Score") +
  theme_classic()

raincloud_2_aligned

cairo_ps(file = "figs/energyStability_energyMindfulNaturalReact.eps", onefile = FALSE, fallback_resolution = 600)
raincloud_2_aligned
dev.off()

t.test(as.numeric(1/log10(stability_control_react)), as.numeric(1/log10(stability_mindfulness_alc_react)))

############################################################################
# energy for between-group control alc react <-> mindfulness mindful react #
############################################################################

df_plot <- as.data.frame(cbind(as.numeric(c(energy_cost_zero_to_control, energy_cost_zero_to_mindfulness)), c(rep('energy_cost_zero_to_control', 145), rep('energy_cost_zero_to_mindfulness', 145)), paste0("region_", 1:145)))
colnames(df_plot) <- c('energy', 'condition', 'region')
df_plot$energy <- log10(as.numeric(df_plot$energy))
df_plot$condition <- as.factor(df_plot$condition)
df_plot$region <- as.factor(df_plot$region)

# pd = position_dodge(0.15)
# ggplot(data=df_plot, mapping=aes(x=condition, y=energy, group=region)) + 
#   geom_line(aes(),position=pd) + 
#   geom_point(aes(color=condition),size=1, position=pd) +
#   theme_classic() +  theme(legend.position = "none")

df_1x1 <- data_1x1(
  array_1 = df_plot$energy[which(df_plot$condition=="energy_cost_zero_to_control")],
  array_2 = df_plot$energy[which(df_plot$condition=="energy_cost_zero_to_mindfulness")],
  jit_distance = 0.09,
  jit_seed = 321
)

raincloud_2_aligned <- raincloud_1x1_repmes(
  data = df_1x1,
  colors = (c('dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange')),
  line_color = 'gray',
  line_alpha = .3,
  size = 1,
  alpha = .6,
  align_clouds = TRUE) +
  
  scale_x_continuous(breaks=c(1,2), labels=c("energy_cost_zero_to_control", "energy_cost_zero_to_mindfulness"), limits=c(0, 3)) +
  xlab("Time") + 
  ylab("Score") +
  theme_classic()

raincloud_2_aligned

cairo_ps(file = "figs/energyControl_energyControlvsMindfulReact.eps", onefile = FALSE, fallback_resolution = 600)
raincloud_2_aligned
dev.off()

t.test(as.numeric(log10(energy_cost_zero_to_control)), as.numeric(log10(energy_cost_zero_to_mindfulness)))

################################################################################################
# energy for between-group control stability alc react <-> mindfulness stability mindful react #
################################################################################################

df_plot <- as.data.frame(cbind(as.numeric(c(stability_control_react, stability_mindfulness_downreg)), c(rep('stability_control_react', 145), rep('stability_mindfulness_downreg', 145)), paste0("region_", 1:145)))
colnames(df_plot) <- c('energy', 'condition', 'region')
df_plot$energy <- log10(as.numeric(df_plot$energy))
df_plot$condition <- as.factor(df_plot$condition)
df_plot$region <- as.factor(df_plot$region)

# pd = position_dodge(0.15)
# ggplot(data=df_plot, mapping=aes(x=condition, y=energy, group=region)) + 
#   geom_line(aes(),position=pd) + 
#   geom_point(aes(color=condition),size=1, position=pd) +
#   theme_classic() +  theme(legend.position = "none")

df_1x1 <- data_1x1(
  array_1 = 1/df_plot$energy[which(df_plot$condition=="stability_control_react")],
  array_2 = 1/df_plot$energy[which(df_plot$condition=="stability_mindfulness_downreg")],
  jit_distance = 0.09,
  jit_seed = 321
)

raincloud_2_aligned <- raincloud_1x1_repmes(
  data = df_1x1,
  colors = (c('dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange')),
  line_color = 'gray',
  line_alpha = .3,
  size = 1,
  alpha = .6,
  align_clouds = TRUE) +
  
  scale_x_continuous(breaks=c(1,2), labels=c("stability_control_react", "stability_mindfulness_downreg"), limits=c(0, 3)) +
  xlab("Time") + 
  ylab("Score") +
  theme_classic()

raincloud_2_aligned

cairo_ps(file = "figs/energyStability_ControlvsMindfulReact.eps", onefile = FALSE, fallback_resolution = 600)
raincloud_2_aligned
dev.off()

t.test(as.numeric(1/log10(stability_control_react)), as.numeric(1/log10(stability_mindfulness_downreg)))
