library(ggplot2)
library(dplyr)
library(tidyr)
library(xtable)
options(xtable.comment = FALSE)
library(AlgDesign)
library(lme4)
library(data.table)
library(quadprog)
library(lmerTest)

# Create optimal design
candidates = expand.grid(compression = c(0.2, 0.5, 0.8),
                         propedge = c(0, 0.5, 1),
                         thickness = factor(1:3))

optdes <- optBlock(~quad(compression, propedge)*thickness+thickness, candidates, blocksizes = rep(10, 10), criterion = "Dpc", nRepeats = 5000)
blocks = optdes$Blocks

design = mutate(optdes$design, block = c(rep(1, 10),rep(2, 10),rep(3, 10),rep(4, 10),rep(5, 10),rep(6, 10),rep(7, 10),rep(8, 10),rep(9, 10),rep(10, 10)))
ggplot(aes(x = compression, y = jitter(propedge, 0.1), color = thickness), data = design) +
  geom_point(alpha = 0.5, size = 6)+
  facet_grid(.~block)+
  theme_bw()+theme(plot.background = element_rect(fill=rgb(.11,0.549,0.667)))

plot(optdes$design$compression, optdes$design$propedge)

# Actual design that was run
setwd("~/Desktop/STAT-compression")
design <- read.csv('design.csv', header = FALSE)

m <- mapply(c,design[,1:3], design[,4:6], design[,7:9], design[,10:12], design[,13:15],
            design[,16:18], design[,19:21], design[,22:24], design[,25:27], design[,28:30])
design <- data.frame(c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10), rep(5, 10),
                       rep(6, 10), rep(7, 10), rep(8, 10), rep(9, 10), rep(10, 10)), m)
colnames(design) <- c("block", "comp", "prop", "thick")
design <- design %>% mutate(block = as.factor(block), thick = as.factor(thick))

# Results from running the experiment
responses <- read.csv("results.csv", header = FALSE) %>%
  rename(mse = V1, time =V2)

# Complete dataframe for analysis
image_data <- cbind(design, responses)

# Model for MSE
mse_model <- lmer(mse~1 + (1|block) + SO(comp, prop):thick+thick, data = image_data)

# Model for time
time_model <- lmer(time~1 + (1|block) + SO(comp, prop):thick+thick, data = image_data)

# Store the model in equations for each level of thickness
msethick1 <- function(c, p){
  val <- 0.03590 - 0.14937 * c - 0.16333 * p - 0.01738 * p * c + 0.14680 * c^2 + 0.36175 * p^2
  return(val)
}

msethick2 <- function(c, p){
  val <- 0.03590 + 0.02537 - 0.23589 * c - 0.15669 * p + 0.07344 * p * c + 0.18347 * c^2 + 0.29448 * p^2
  return(val)
}

msethick3 <- function(c, p){
  val <- 0.03590 + 0.01115 - 0.17402 * c - 0.09840 * p - 0.02027 * p * c + 0.15943 * c^2 + 0.24080 * p^2
  return(val)
}

timethick1 <- function(c, p){
  val <- 271.07 -526.20 * c - 1659.81 * p + 709.92 * p * c + 217.31 * c^2 + 2845.82 * p^2
  return(val)
}

timethick2 <- function(c, p){
  val <- 271.07 - 97.90 -127.03 * c -1563.96 * p + 454.43 * p * c -93.26 * c^2 + 2851.96 * p^2
  return(val)
}

timethick3 <- function(c, p){
  val <- 271.07 - 37.06 -354.80 * c -1685.30 * p + 768.33 * p * c + 68.44 * c^2 + 2850.30 * p^2
  return(val)
}

cvals = seq(0.2, 0.8, 0.01)
pvals = seq(0, 1, 0.01)

contour(cvals, pvals, outer(cvals, pvals, msethick1))
contour(cvals, pvals, outer(cvals, pvals, msethick2))
contour(cvals, pvals, outer(cvals, pvals, msethick3))

contour(cvals, pvals, outer(cvals, pvals, timethick1))
contour(cvals, pvals, outer(cvals, pvals, timethick2))
contour(cvals, pvals, outer(cvals, pvals, timethick3))


# Lets look at a particular example

plot(seq(0, 1, 0.01), msethick1(0.5, seq(0, 1, 0.01)), type = "l")
curve(msethick1(0.1, x), col = "red")
curve(timethick1(0.1, x), add=TRUE, col = "red")
curve(msethick2(0.1, x), add=TRUE, col = "blue")
curve(timethick2(0.1, x), add=TRUE, col = "blue")
curve(msethick3(0.1, x), add=TRUE, col = "green")
curve(timethick3(0.1, x), add=TRUE, col = "green")

# Solve the quadratic programming problem




