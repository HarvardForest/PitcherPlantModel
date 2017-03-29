source('../../../src/sirota2014.R')

mu.control <- apply(do.call(cbind,
                            lapply(dat[['control']], function(x) x$value.i)),
                    1,mean)
sd.control <- apply(do.call(cbind,
                            lapply(dat[['control']], function(x) x$value.i)),
                    1,sd)
Amax <- max(mu.control[((1440*3):length(mu.control))]) 
Amin <- min(mu.control[((1440*3):length(mu.control))]) 

Amax / Amin

test <- ppSim(5,10,Amin = Amin,Amax = Amax,beta = 4.5e-4,k=2)

plot(mu.control,ylim = c(0,35),type = 'l')
lines(mu.control + sd.control,col = 'grey')
lines(mu.control - sd.control,col = 'grey')
abline(h = min(mu.control[((1440*3):length(mu.control))]))
lines(test$Oxygen[-1:-720])

fit.con <- predict(lm(mu.control~I(1:length(mu.control))),newdata = data.frame(x = 1:length(mu.control)))

### Detrended recalibrated control mean
drc <- mu.control - fit.con + Amin
dsim <- (test$Oxygen - mean(test$Oxygen))/sd(test$Oxygen)

plot((drc-mean(drc))/sd(drc))
lines(dsim[-1:-720])

mudd[[1]]

