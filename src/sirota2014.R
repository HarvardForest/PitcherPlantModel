### Fitting the pitcherplantmodel output to
### data from the Sirota 2014 pitcherplant model

library(txtplot)
library(RCurl)
library(XML)

### Import from the Harvard Forest Archive
### Data should include the daily trend

meta <- htmlTreeParse('http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf205')

### 4 enrichment levels (0.125, 0.25, 0.5, 1.0 mg prey added ml-1 d-1)

hf205 <- read.csv('http://harvardforest.fas.harvard.edu/data/p20/hf205/hf205-01-TPexp1.csv')
hf205 <- split(hf205,hf205$run.num)

abio.levels <- c("air.temp","par","water.temp")
o2.levels <- c("control","high","low","med.high","med.low")

abio <- do.call(rbind,lapply(hf205,function(x,y) x[as.character(x$variable) %in% y,], y = as.character(abio.levels)))
o2 <- do.call(rbind,lapply(hf205,function(x,y) x[x$variable %in% y,], y = o2.levels))

o2 <- o2[o2[,'run.num'] == 1,]

high.sim <- pitcherPlantSim(5,c(0,rep(1,4)),beta=4.5e-6)

plot(o2[o2[,'variable'] == 'high','value.i']/max(o2[o2[,'variable'] == 'high','value.i']),type = 'l',ylim = c(0,1))
lines(high.sim[-1:-720,'Oxygen'])

obs <- o2[o2[,'variable'] == 'high','value.i']/max(o2[o2[,'variable'] == 'high','value.i'])
mod <- high.sim[-1:-720,'Oxygen']
mod <- mod[1:length(obs)]

plot(obs,type = 'l',ylim = c(0,1))
lines(mod)

plot(obs,mod)
hist((obs-mod))

plot(o2[o2[,'variable'] == 'control','value.i'])

### A pitcher with 5ml liquid digests 75ug wasp in 48 hours
### 75 ug == 0.075 mg
### c(0.125, 0.25, 0.5, 1.0) mg prey added ml-1 d-1)
### c(0.625, 1.250, 2.500, 5.000)  mg prey added d-1)


### simulation
sim <- list()
F.level <- c(0, 0.625, 1.250, 2.500, 5.000)
for (i in 1:length(F.level)){
    sim[[i]] <- pitcherPlantSim(5,rep(F.level[i],5),beta=5e-06,k=3)
}
length(sim)

plot(sim[[1]]$Oxygen,ylab = c(0,1))
plot(sim[[2]]$Oxygen,ylab = c(0,1))
plot(sim[[3]]$Oxygen,ylab = c(0,1))
plot(sim[[4]]$Oxygen,ylab = c(0,1))
plot(sim[[5]]$Oxygen,ylab = c(0,1))

