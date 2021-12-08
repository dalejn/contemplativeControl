rm(list=ls())

library(sjmisc)
library(sjPlot)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(DataCombine)
library(reshape)
library(glmmTMB)
library(psych)

setwd('/Users/dalezhou/Box/contemplativeControlCode/daleManuscriptCodeReplication/')
load("data/multiLevel.RData")

#################################################
# controlling for baseline and other covariates #
#################################################

# plot model components
plot_model(Alconv3)

# plot random effects
#plot_model(Alconv1, type = "re")

# plot marginal effects for attention impulsivity drink number
p <- plot_model(Alconv3, type = "eff", terms = "bis_11_attention", show.zeroinf = FALSE)
p + theme_classic()

cairo_ps(file = "/Users/dalezhou/Desktop/Dropbox/Projects/inProgress/2018-11-mindfulControl/figures/multilevelConditionalPlot_impulsivity.eps", onefile = FALSE, fallback_resolution = 600)
p + theme_classic()
dev.off()

# same plot but for attention impulsivity drink probability
plot_zeroinf <- ggpredict(Alconv3, terms = c("bis_11_attention"), type = "zi.prob")
plot_zeroinf$predicted = 1-plot_zeroinf$predicted
plot_zeroinf$conf.low = 1-plot_zeroinf$conf.low
plot_zeroinf$conf.high = 1-plot_zeroinf$conf.high

cairo_ps(file = "/Users/dalezhou/Desktop/Dropbox/Projects/inProgress/2018-11-mindfulControl/figures/multilevelZeroInflatedPlot_bisAttention.eps", onefile = FALSE, fallback_resolution = 600)
plot(plot_zeroinf) + theme_classic()
dev.off()