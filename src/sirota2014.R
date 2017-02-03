### Fitting the pitcherplantmodel output to
### data from the Sirota 2014 pitcherplant model

library(magrittr)
library(txtplot)

### Import from the Harvard Forest Archive
### Data should include the daily trend


hf205 <- read.csv('http://harvardforest.fas.harvard.edu/data/p20/hf205/hf205-01-TPexp1.csv')
summary(hf205)
hf205 <- split(hf205,hf205$variable)
hf205 <- lapply(hf205,function(x) split(x,f = x$run.num))

## "air.temp"   "control"    "high"       "low"        "med.high"  
## "med.low"    "par"        "water.temp"

dat <- hf205[['high']][[4]]
dat$datetime <- as.numeric(dat$datetime)
dat$datetime <- order(dat$datetime)
ts.x <- ts(dat$value.i,frequency = 1440)
tsd.x <- decompose(ts.x)
plot(tsd.x)

### Check for model fit

