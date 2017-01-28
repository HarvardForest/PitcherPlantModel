### Fitting the pitcherplantmodel output to
### data from the Sirota 2014 pitcherplant model

### Import from the Harvard Forest Archive
### Data should include the daily trend

### Check for model fit

library(magrittr)
hf205 <- read.csv('http://harvardforest.fas.harvard.edu/data/p20/hf205/hf205-01-TPexp1.csv')
hf205$datetime <- as.numeric(hf205$datetime)
hf205 <- split(hf205,hf205$run.num)

dat <- hf205[[1]]
attach(dat)
txtplot(datetime,value.i)

detach(dat)
