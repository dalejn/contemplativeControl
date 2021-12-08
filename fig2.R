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

load("/Users/dalezhou/Desktop/Dropbox/Projects/inProgress/2018-11-mindfulControl/data/multiLevel.RData")

#################################################
# controlling for baseline and other covariates #
#################################################

# plot model components
plot_model(Alconv3)

# plot random effects
#plot_model(Alconv1, type = "re")

# plot marginal effects
plot_model(Alconv3, type = "eff", terms = "avecont_log10", show.zeroinf = FALSE)

# plot marginal effects
p <- plot_model(Alconv3, type = "eff", terms = "Condition", show.zeroinf = FALSE)
p + theme_classic()

cairo_ps(file = "/Users/dalezhou/Desktop/Dropbox/Projects/inProgress/2018-11-mindfulControl/figures/multilevelConditionalPlot_mindful.eps", onefile = FALSE, fallback_resolution = 600)
p + theme_classic()
dev.off()

# Another plotting approach
# plotting association between signal count and drinking 
#psych::describe(EMA$signal_count)

#plot_conditional <- ggpredict(Alconv1, terms = c("avecont_log10"))
#plot(plot_conditional)
plot_zeroinf <- ggpredict(Alconv3, terms = c("avecont_log10"), type = "zi.prob")
plot_zeroinf$predicted = 1-plot_zeroinf$predicted
plot_zeroinf$conf.low = 1-plot_zeroinf$conf.low
plot_zeroinf$conf.high = 1-plot_zeroinf$conf.high

cairo_ps(file = "/Users/dalezhou/Desktop/Dropbox/Projects/inProgress/2018-11-mindfulControl/figures/multilevelZeroInflatedPlot_aveControl.eps", onefile = FALSE, fallback_resolution = 600)
plot(plot_zeroinf) + theme_classic()
dev.off()