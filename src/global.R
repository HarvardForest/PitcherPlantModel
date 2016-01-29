### Model for the Pitcher Plant micro-system

## 6:00 sunrise = 360 
## 12:00 noon = 720
## 18:00 sunset = 1080

PAR <- function(days=3,start=0,amp=100){
    amp * sin(2 * pi * rep((1:1440 + 1080 + start),days) * (1/1440))
}

photo <- function(days=3,Amax=1,Amin=0,Aqe=0.3,LCP=0,start=0,amp=50){
    out <- Amax * (1 - exp(-Aqe * (PAR(days,start,amp) - LCP)))
    out[out < Amin] <- Amin
    return(out)
}


pitcherPlantSim <- function(days=3, foodWeight=c(0,1,0), beta=0.1, d=0,  k=1, Bscaler=1,m=0,aMax=10, aMin=0, s=10, feedingTime=720, c=100,x0=0,w0=0,bound.max=FALSE,verbose=FALSE){
    if (length(foodWeight) < days){
        foodWeight <- rep(foodWeight,days)[1:days]
    }
    ## Initialization ##
                                        # time keeper
    minute <- 1
                                        # o2 at minute=0, P=0 b/c unable to index at minute=0
    x <- x0
                                        # simulate photosynthesis as fixed values
    P <- photo(days)
                                        # food weight at time 1
    if (feedingTime == 1){
        w <- w0 + foodWeight[1]
    }else{w <- w0}
                                        # initial nutrient value
    n <- w[1] * x[1] / c
                                        # initial augmentation value
    a <- (((aMax-aMin)/(1+exp(-(( s * n[length(minute)]) - d)))) + aMin)
                                        # initial biological o2 demand
    B <- w[1]/(k+w[1])
                                        # augmented photosynthesis initialization
    A <- a * P[1]
                                        #start day loop
    for (z in 1:days){
                                        #star minute loop
        for (j in 1:1440){
            if (verbose){print(c(z,j,minute[length(minute)]))}
                                        # update #
                                        # time keeper
            minute <- c(minute,minute[length(minute)] + 1)
                                        # food weight at time 1
            wt <- w[length(minute) - 1] * exp(w[length(minute) - 1] * -beta)
            if (j == feedingTime){
                w <- c(w,wt + foodWeight[z])
            }else{
                w <- c(w,wt)
            }
                                        # nutrient value
            n <- c(n,w[length(minute)] * x[length(minute) - 1] / c)
                                        # augmentation value
            a <- c(a,(((aMax - aMin)/(1 + exp((-(s * n[length(minute)]) - d)))) + aMin))
                                        # biological o2 demand
            B <- c(B,a[length(minute)] * (w[length(minute)] / (k + w[length(minute)]) * Bscaler))
                                        # augmented photosynthesis initialization
            A <- c(A,a[length(minute)] * P[length(minute)])
                                        # o2 at minute=0, P=0 b/c unable to index at minute=0
            x <- c(x, A[length(minute)] - (m + B[length(minute)]))
            if (is.na(x[length(x)])){x[length(x)] <- 0}
            if (x[length(x)] < 0){x[length(x)] <- 0}
            if (bound.max & x[length(x)] > aMax){x[length(x)] <- aMax}
        } # end minute loop
    } # end day loop

                                        # trim objects to appropriate time
                                        # omitted values aren't relevant
    minute <- minute[1:length(P)]
    A <- A[1:length(P)]
    B <- B[1:length(P)]
    n <- n[1:length(P)]
    a <- a[1:length(P)]
    x <- x[1:length(P)]
    w <- w[1:length(P)]
                                        # prep for export
    data <- data.frame(minute, x, P, A, B, w, n, a)
    colnames(data) <- c("Minute", "Oxygen", "PAR","Photosynthesis",
                        "Biological Oxygen Demand", "Food Amount", "Nutrients",
                        "Augmentation Value")

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
    hyst.start <- (((n1+n2)*1440) + 1) - feedingTime
    hyst <- x[hyst.start:length(x)]
    if (n1 != 1 & any(x[1:1440] != x[1441:2880])){warning('n1! Check base values.')}
    base <- rep(x[1:1440],ceiling(I(length(hyst)/1440)))[-1:-feedingTime]
    dhb <- hyst - base
    min.dhb <- minify(dhb)
    ## return rate = time from last feeding to return to base
    rr.t <- (1:length(min.dhb))[abs(min.dhb) <= tol][1]
    if (is.na(rr.t)){rr.t <- length(min.dhb)}
    ## change in dbase from start of feeding to dbase=0 
    ## or last time when feeding would occur
    ## absolute and proportionate return rate = change 
    ## from initial feeding point
    arr <- (min.dhb[1] - min.dhb[rr.t]) / rr.t 
    prr <- (min.dhb[1] - min.dhb[rr.t]) / min.dhb[1] / rr.t 
    ## proportion cumulative O2 change from base
    ## i.e., how much O2 was porduced under hysteresis
    ## compared to the total that would have been
    ## produced without feeding
    acdb <- sum(hyst) - sum(base)
    pcdb <- (sum(hyst) - sum(base)) / sum(base) 
    ## output
    out <- c(return.time=rr.t,arr=arr,acdb=acdb,prr=prr,pcdb=pcdb)
    return(out)
}
