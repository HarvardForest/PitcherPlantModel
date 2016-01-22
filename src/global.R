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

pitcherPlantSim <- function(days=3, feedingTime=720, foodWeight=0, beta=0.001, k=1, Bscaler=10,m=0,ai=7.5,aMax=10, aMin=1, s=10, d=1, c=100) {

if (length(foodWeight) < days){
    foodWeight <- rep(foodWeight,days)[1:days]
}

## Initialization ##

# simulate photosynthesis as fixed values
P <- photo(days)
# initial nutrient value
n <- 0
# initial augmentation value
a <- ai 
# initial biological o2 demand
B <- 0/(k+0)
# augmented photosynthesis initialization
A <- a * P[1]
# o2 at minute=0, P=0 b/c unable to index at minute=0
x <- A - m*B

# initialize the time keeper
minute <- 1

for(z in 1:days){

  # run simulation for a full day
  for(j in 1:1440){

    # adjust biological o2 demand
    B <- c(B, (w[length(minute)]/(k+w[length(minute)]))*Bscaler)

    # adjust amount of nutrients
    n <- c(n, (w[length(minute)]*x[length(minute)-1])/c)

    # adjust augmentation value
    a <- c(a, ((aMax-aMin)/(1+exp((-s*n[length(minute)])-d)))+aMin)

    # update A
    A <- c(A,(a[length(minute)] * P[length(minute)]))

    # adjust o2 amount
    tempO2 <- (A[length(minute)])- (m + B[length(minute)])

  
    if(is.na(tempO2) == FALSE && tempO2 > 0){
      x <- c(x, tempO2)
    }
    else{
      x <- c(x, 0)
    }

    # adjust minute
    minute <- c(minute, length(minute)+1)

    if(j < 1440){
        ## adjust amount of food
        w <- c(w, w[length(w)]*exp(-beta*(1)))
    }
    if (j == feedingTime){
        ## add food
        w <- c(w, w[length(w)] + foodWeight[z])
    }else{w <- c(w , w[length(w)])}
  }
}

# trim objects to appropriate time
  # omitted values aren't relevant
minute <- minute[1:length(P)]
A <- A[1:length(P)]
B <- B[1:length(P)]
n <- n[1:length(P)]
a <- a[1:length(P)]
x <- x[1:length(P)]
w <- w[1:length(P)]

data <- data.frame(minute, P, x, A, B, n, a, w)
colnames(data) <- c("Minute", "PAR","Oxygen", "Photosynthesis",
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

ppHyst <- function(x,n1,n2,feedingTime=720,tol=0){
    if (class(x) != 'numeric' & 
        (class(x) == 'data.frame' | class(x) == 'matrix')){
        x <- x$Oxygen
    }

    hyst.start <- ((n1+n2)*1440)
    base <- max(x[1:(1440*n1)])
    hyst <- x[hyst.start:length(x)]
    max.hyst <- maxify(hyst)

## max - base
    dMB <- max(max.hyst) - base

## min - base
    dmB <- min(max.hyst) - base

## return rate = time from last feeding to return to base
    if (all(max.hyst <= (base + tol) &  max.hyst >= (base - tol))){
        r.t <- ((1:length(max.hyst))[max.hyst <= (base + tol) & 
                                         max.hyst >= (base - tol)][1])
    }else{
        r.t <- length(max.hyst)
    }
    
    Mrr <- (dMB) / r.t
    mrr <- (dmB) / r.t

## output
    out <- c(return.time=r.t,dMB=dMB,max.return.rate=Mrr,dmB=dmB,min.return.rate=mrr)
    return(out)
}
