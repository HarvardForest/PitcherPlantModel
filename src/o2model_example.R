###Example: how to use the functions from o2model.R.
source('o2model.R')

### photosynthesis
photo <- f.A()
plot(photo,type='l')

### decomp
decomp <- f.w()
plot(decomp,type='l')

### simulation
pitcher <- simPitcher()
pairs(pitcher)

### slope of the rate of change in O2
dx <- diff(diff(pitcher$x))
plot(dx[dx>0.025])
abline(lm(dx[dx>0.025]~I(1:length(dx[dx>0.025]))))
cor(I(1:length(dx[dx>0.025])),dx[dx>0.025])
