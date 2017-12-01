library(ggplot2)
library(dplyr)
library(tidyr)
library(daewr)
library(multcomp)
library(agricolae)
library(xtable)
options(xtable.comment = FALSE)
library(FrF2)
library(AlgDesign)
library(lme4)
library(rsm)
library(car)

candidates = expand.grid(compression = c(0, 0.4, 0.8),
                         propedge = c(0, 0.5, 1),
                         thickness = c(1, 2, 3))

optdes <- optBlock(~quad(.), candidates, blocksizes = rep(10, 10), criterion = "Dpc", nRepeats = 5000)

blocks = optdes$Blocks

