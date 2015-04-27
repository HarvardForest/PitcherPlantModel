###Example: how to use the functions from o2model.R.
source('o2model.R')

### photosynthesis
photo <- f.A()
plot(photo,type='l')

### decomp
decomp <- f.w(w0=100)
plot(decomp,type='l')

### simulation
pitcher <- simPitcher(w0=75,a.max=10)
plot(pitcher[,2])
plot(pitcher[,4])
pairs(pitcher)
