library(ggplot2)
library(dplyr)
library(tidyr)
library(xtable)
options(xtable.comment = FALSE)
library(AlgDesign)
library(lme4)
library(data.table)
library(quadprog)
library(rsm)

# Create optimal design
candidates = expand.grid(compression = seq(0.2, 0.8, 0.01),
                         propedge = seq(0, 0.9, 0.01),
                         thickness = factor(1:3))

optdes <- optBlock(~quad(compression, propedge)*thickness+thickness, candidates, blocksizes = rep(10, 10), criterion = "Dpc", nRepeats = 5000)
blocks = optdes$Blocks

design = mutate(optdes$design, block = c(rep(1, 10),rep(2, 10),rep(3, 10),rep(4, 10),rep(5, 10),rep(6, 10),rep(7, 10),rep(8, 10),rep(9, 10),rep(10, 10)))
ggplot(aes(x = compression, y = propedge, color = thickness), data = design) +
  geom_point(alpha = 0.5, size = 6)+
  facet_grid(.~block)+
  theme_bw()+theme(plot.background = element_rect(fill=rgb(.11,0.549,0.667)))


plot(optdes$design$compression, optdes$design$propedge)

# Actual design that was run
setwd("~/STAT-compression")
design <- read.csv('newdesign.csv', header = FALSE)

m <- mapply(c,design[,1:3], design[,4:6], design[,7:9], design[,10:12], design[,13:15],
            design[,16:18], design[,19:21], design[,22:24], design[,25:27], design[,28:30])
design <- data.frame(c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10), rep(5, 10),
                       rep(6, 10), rep(7, 10), rep(8, 10), rep(9, 10), rep(10, 10)), m)
colnames(design) <- c("block", "comp", "prop", "thick")
design <- design %>% mutate(block = as.factor(block), thick = as.factor(thick))

# Results from running the experiment
responses <- read.csv("newresults.csv", header = FALSE) %>%
  rename(mse = V1, time = V2)

# Complete dataframe for analysis
image_data <- cbind(design, responses)

# Model for MSE
mse_model <- lmer(mse~1 + (1|block) + SO(comp, prop):thick+thick, data = image_data)

# Model for time
time_model <- lmer(time~1 + (1|block) + SO(comp, prop):thick+thick, data = image_data)

# Store the model in equations for each level of thickness
msethick1 <- function(c, p){
  val <- 0.0222961 -0.0369953 * c -0.0069362 * p + 0.0096340 * p * c + 0.0138763 * c^2 - 0.0012087 * p^2
  return(val)
}

msethick2 <- function(c, p){
  val <- 0.0222961 - 0.0009052 - 0.0359361* c -0.0188273 * p + 0.0115831 * p * c + 0.0177358 * c^2 + 0.0128854 * p^2
  return(val)
}

msethick3 <- function(c, p){
  val <- 0.0222961 + 0.0108773 - 0.0703937 * c -0.0319732 * p + 0.0254489 * p * c + 0.0425258 * c^2 + 0.0200453 * p^2
  return(val)
}

timethick1 <- function(c, p){
  val <- 415.6468 -824.9586 * c + 50.3482 * p + 147.7710 * p * c + 426.3995 * c^2 -101.8634 * p^2
  return(val)
}

timethick2 <- function(c, p){
  val <- 415.6468 + 41.0330 -828.3122 * c -63.8428 * p + 170.0446 * p * c + 399.1249 * c^2 + 0.2417 * p^2
  return(val)
}

timethick3 <- function(c, p){
  val <- 415.6468 + 258.3016 -1965.1395 * c + 32.5666 * p -305.0762 * p * c + 1693.4228 * c^2 + 173.8884 * p^2
  return(val)
}

cvals = seq(0.2, 0.8, 0.01)
pvals = seq(0, 0.9, 0.01)

par(mfrow=c(2,3))

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

# Solve the combined optimization problem
responsesurf1 <- function(inputs, c){
  p <- inputs[1]
  f1 = 0
  f2 = 0
  f3 = 0
  if(msethick1(c, p)>0.0125){
    f1 <- (msethick1(c, p)+1)^100
  }
  if (timethick1(c, p)>240){
    f2 <- (1+(timethick1(c, p)/20000))^100
  } 
  f3 <- msethick1(c, p) + timethick1(c, p) / 20000
  return(f1+f2+f3) 
}

responsesurf2 <- function(inputs, c){
  p <- inputs[1]
  f1 = 0
  f2 = 0
  f3 = 0
  if(msethick2(c, p)>0.0125){
    f1 <- (msethick2(c, p)+1)^100
  }
  if (timethick2(c, p)>240){
    f2 <- (1+(timethick2(c, p)/20000))^100
  }
  f3 <- msethick2(c, p) + timethick2(c, p) / 20000
  return(f1+f2+f3) 
}

responsesurf3 <- function(inputs, c){
  p <- inputs[1]
  f1 = 0
  f2 = 0
  f3 = 0
  if(msethick3(c, p)>0.0125){
    f1 <- (msethick3(c, p)+1)^100
  }
  if (timethick3(c, p)>240){
    f2 <- (1+(timethick3(c, p)/20000))^100
  }
  f3 <- msethick3(c, p) + timethick3(c, p) / 20000
  return(f1+f2+f3) 
}

out = NULL
for(i in 1:101){
  cnow = 0.2 + (i-1)*(0.6/100)
  opt1 <- optimize(responsesurf1, c(0,0.9), c = cnow)
  opt2 <- optimize(responsesurf2, c(0,0.9), c = cnow)
  opt3 <- optimize(responsesurf3, c(0,0.9), c = cnow)
  minima <- rbind(as.data.frame(opt1),as.data.frame(opt2),as.data.frame(opt3))
  best <- which.min(minima$objective)
  mincost <- minima[best, 2]
  optp <- minima[best, 1]
  cat(best, mincost, optp)
  optmse <- switch (best, msethick1(cnow, optp), msethick2(cnow, optp), msethick3(cnow, optp))
  opttime <- switch (best, timethick1(cnow, optp), timethick2(cnow, optp), timethick3(cnow, optp))
  out = rbind(out, c(cnow, optp, best, optmse, opttime, mincost))
}

optimumsettings <- data.frame(out)
colnames(optimumsettings) = c("comp","propedge","thick","msepred","timepred","cost")




# Plot with x=0.2, x=0.3 and x = 0.7 for all the different behavior
plot(seq(0,0.9,0.01), apply(matrix(seq(0,0.9,0.01)), 1, responsesurf), type = 'l')


# Compare the mse and time between optimal runs and random runs

simdes <- read.csv('simruns.csv')

randresp <- read.csv('randout.csv', header = FALSE) %>%
  rename(mse = V1, time = V2)

optresp <- read.csv('optout.csv', header = FALSE) %>%
  rename(mse = V1, time = V2)

simresults <- cbind(simdes[rep(c(1:10), 10),],randresp,optresp, c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10), rep(5, 10),
                                                                 rep(6, 10), rep(7, 10), rep(8, 10), rep(9, 10), rep(10, 10)))
colnames(simresults) = c("comp","propedge","thick","msepred","timepred","cost","randmse","randtime","optmse","opttime","block")

plotdata <- gather(simresults, metric, value, randmse, optmse, randtime, opttime) %>%
  cbind(type = c(rep('mse', 200), rep('time', 200)))

ggplot(aes(x = comp, y = value, color = metric), data = plotdata) +
  geom_point() + 
  facet_grid(type~block, scales = "free_y")+
  theme_bw()+theme(plot.background = element_rect(fill=rgb(.11,0.549,0.667)))

cost <- function(mse, time){
  f1 = 0
  f2 = 0
  f3 = 0
  if(mse>0.0125){
    f1 <- (mse+1)^100
  }
  if (time>240){
    f2 <- (1+(time/20000))^100
  }
  f3 <- mse + time/20000
  return(f1+f2+f3) 
}
  
group_by(simresults, block) %>%
  summarize(mean(randmse)-mean(optmse), mean(randtime)-mean(opttime))

mutate(simresults, randcost = mapply(cost, simresults$randmse, simresults$randtime), optcost = mapply(cost, simresults$optmse, simresults$opttime))%>%
  summarize(mean(randmse),mean(optmse), mean(randtime),mean(opttime), mean(randcost), mean(optcost))

