### Fitting the pitcherplantmodel output to
### data from the Sirota 2014 pitcherplant model

library(magrittr)
library(txtplot)

### Import from the Harvard Forest Archive
### Data should include the daily trend

hf205 <- read.csv('http://harvardforest.fas.harvard.edu/data/p20/hf205/hf205-01-TPexp1.csv')
hf205 <- split(hf205,hf205$run.num)
dat <- hf205[[1]]
dat$datetime <- as.numeric(dat$datetime)
dat$datetime <- order(dat$datetime)

attach(dat)
txtplot(datetime,value.i,pch='.')

ts.x <- ts(value.i,frequency = 1440)
txtplot(ts.x)

tsd.x <- decompose(ts.x)
names(tsd.x)
txtplot(tsd.x$seasonal)
txtplot(tsd.x$trend)
txtplot(tsd.x$random)

txtdensity(na.omit(tsd.x$random))

detach(dat)

### Check for model fit

