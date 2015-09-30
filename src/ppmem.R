### Model for the Pitcher Plant micro-system

## 6:00 sunrise = 360 
## 12:00 noon = 720
## 18:00 sunset = 1080

PAR <- function(days=3,start=0,amp=100){
    amp * sin(2 * pi * rep((1:1440 + 1080 + start),days) * (1/1440))
}

photo <- function(days=3,Amax=4,Amin=0,Aqe=0.3,LCP=0,start=0,amp=50){
    out <- Amax * (1 - exp(-Aqe * (PAR(days,start,amp) - LCP)))
    out[out < Amin] <- Amin
    return(out)
}

pitcherPlantSim <- function(days=3, feedingTime=720, foodWeight=5, beta=0.001, k=1, Bscaler=10,
                            aMax=10, aMin=1, s=10, d=1, c=100) {

minute <- vector(mode="numeric") # t/time variable
x <- vector(mode="numeric") # amount of o2
a <- vector(mode="numeric") # augmentation function
P <- vector(mode="numeric") # photosynthesis
B <- vector(mode="numeric") # biological o2 demand
n <- vector(mode="numeric") # amount of nutrients
w <- vector(mode="numeric") # amount of food

if (length(foodWeight) < days){
    foodWeight <- rep(foodWeight,days)[1:days]
}

## Initialization ##

# simulate photosynthesis as fixed values
P <- photo(days)

# initial nutrient value
n <- 0

# initial augmentation value
a <- ((aMax-aMin)/(1+exp((-s*n)-d)))+aMin

# initial biological o2 demand
B <- 0/(k+0)

# o2 at minute=0, P=0 b/c unable to index at minute=0
x <- (a*0)-B

# simulate until food is first added
# loop runs until feedingTime-2 b/c food is added AT the minute
for(i in 1:(feedingTime-2)){
  # augmentation function - default value
  a <- c(a, ((aMax-aMin)/(1+exp((-s*n[i])-d)))+aMin)

  # biological oxygen demand - default value (no food = no microbes)
  B <- c(B, 0/(k+0))

  # calculate o2 amount - product of photosynthesis alone (no food)
  x <- c(x, (a[i]*P[i])-B[i])

  # amount of food - no food
  w <- c(w, 0)

  # amount of nutrients - no nutrients
  n <- c(n, 0)

  # adjust minute
  minute <- c(minute, i)
}

# adjust minute
minute <- c(minute, length(minute)+1)

# adjust amount of food
w <- c(w, w[length(w)])

for(z in 1:days){
  # add food
  w <- c(w, w[length(w)]+foodWeight[z])

  # run simulation for a full day
  for(j in 1:1440){
    # adjust minute
    minute <- c(minute, length(minute)+1)

    # adjust biological o2 demand
    B <- c(B, (w[length(minute)]/(k+w[length(minute)]))*Bscaler)

    # adjust amount of nutrients
    n <- c(n, (w[length(minute)]*x[length(minute)-1])/c)

    # adjust augmentation value
    a <- c(a, ((aMax-aMin)/(1+exp((-s*n[length(minute)])-d)))+aMin)

    # adjust o2 amount
    tempO2 <- (a[length(minute)]*P[length(minute)])-B[length(minute)]
    if(is.na(tempO2) == FALSE && tempO2 > 0){
      x <- c(x, tempO2)
    }
    else{
      x <- c(x, 0)
    }

    if(j < 1440){
        ## adjust amount of food
        w <- c(w, w[length(w)]*exp(-beta*(1)))
    }
  }
}

# trim objects to appropriate time
  # omitted values aren't relevant
minute <- minute[1:length(P)]
B <- B[1:length(P)]
n <- n[1:length(P)]
a <- a[1:length(P)]
x <- x[1:length(P)]
w <- w[1:length(P)]

data <- data.frame(minute, x, P[1:length(x)], B, n, a, w)
colnames(data) <- c("Minute", "Oxygen", "Photosynthesis",
                    "Biological Oxygen Demand", "Nutrients",
                    "Augmentation Value", "Food Amount")
return(data)
}

maxify <- function(x,p=1440){
    out <- list()
    n <- length(x)/p
    for (i in 1:n){out[[i]] <- max(x[(1+(p*(i-1))):((p*i)-1)])}
    return(unlist(out))
}

minify <- function(x,p=1440){
    out <- list()
    n <- length(x)/p
    for (i in 1:n){out[[i]] <- min(x[(1+(p*(i-1))):((p*i)-1)])}
    return(unlist(out))
}

### hysteresis statistics

ppHyst <- function(x,n1,n2,n3,feedingTime=720){
    if (class(x) != 'numeric' & 
        (class(x) == 'data.frame' | class(x) == 'matrix')){
        x <- x$Oxygen
    }

    hyst.start <- ((n1+n2)*1440) 
    base <- max(x[1:(1440*n1)])
    hyst <- x[hyst.start:length(x)]
    max.hyst <- maxify(hyst)

## max - base
    dMB <- max(x[hyst.start:length(x)]) - base

## return rate = time from last feeding to return to base
    r.t <- ((1:length(max.hyst))[max.hyst == base][1])
    rr <- (dMB) / r.t

## integrate area above baseline post feeding days and divide by time
    int.hyst <- sum(hyst[hyst > base]) / ((length(x) - hyst.start) / 1440)

## output
    out <- c(dMB=dMB,return.rate=rr,int.hyst=int.hyst)
    return(out)

}
