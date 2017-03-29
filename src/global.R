### Model for the Pitcher Plant micro-system

## 6:00 sunrise = 360 
## 12:00 noon = 720
## 18:00 sunset = 1080


augmentation <- function(n=seq(0,10,by=0.01),s=1,d=5,aMin=1,aMax=2){
    ((aMax-aMin)/(1+exp(-s*(n - d)))) + aMin
}

mineralization <- function(x,w,c=100){
    (w * x)/c
}

rescale <- function(x,ro=c(0,100),rn=c(0,1)){
    or <- max(ro) - min(ro)
    nr <- max(rn) - min(rn)
    (((x - min(ro)) * nr) / or) + min(rn)
}

PAR <- function(days=3,start=0,amp=100){
    amp * sin(2 * pi * rep((1:1440 + 1080 + start),days) * (1/1440))
}

photo <- function(days=3,Amax=1,Amin=0,Aqe=0.3,LCP=0,start=0,amp=50){
    out <- Amax * (1 - exp(-Aqe * (PAR(days,start,amp) - LCP)))
    out[out < Amin] <- Amin
    return(out)
}

decomp <- function(w,beta=4.5e-05,w.w=0.075){
    ### set to decompose a 75 um wasp
    ### over the course of 2880 minutes
    w * exp(-beta*w.w)
}

ppSim <- function(days=5,foodWeight=1,beta=4.5e-06,d=5,
                  k=2,Amax=20,Amin=Amax/1.5,m=0,aMax=2 ,aMin=1,
                  s=1 ,feedingTime=720,c=1,x0=0,w0=0,w.w=75,
                  photo.val = 1,Ascalar = 0.75,Bscalar = 1,
                  bound.max=FALSE,verbose=FALSE ,photo.constant = FALSE 
                  ){
    if (length(foodWeight) < days){
        foodWeight <- rep(foodWeight,days)[1:days]
    }
    ## Initialization ##
                                        # time keeper
    minute <- 1
                                        # simulate photosynthesis as fixed values
    P <- photo(days,(Amax/Amax),(Amax/Amin - 1))
    if (photo.constant){P <- P * 0 + photo.val}
                                        # food weight at time 1
    if (feedingTime == 1){
        w <- w0 + foodWeight[1]
    }else{w <- w0}
                                        # initial nutrient value
    n <- mineralization(x0,w[1],c)
                                        # initial augmentation value
    a <- augmentation(n[length(n)],s,d,aMin,aMax)
                                        # initial biological o2 demand
    B <- Amax * ((a * Bscalar) * (w[1]/(k+w[1]))) 
                                        # augmented photosynthesis initialization
    A <- Amax * ((a * Ascalar) * P[1])
                                        # o2 at minute=0, P=0 b/c unable to index at minute=0
    x <- A[length(minute)] - (m + B[length(minute)])
                                        #start day loop
    for (z in 1:days){
                                        #star minute loop
        for (j in 1:1440){
            if (verbose){print(c(z,j,minute[length(minute)]))}
                                        # update #
                                        # time keeper
            minute <- c(minute,minute[length(minute)] + 1)
                                        # food weight at t
            wt <- decomp(w[length(minute) - 1],beta=beta,w.w=w.w)
            if (j == feedingTime){
                w <- c(w,wt + foodWeight[z])
            }else{
                w <- c(w,wt)
            }
                                        # nutrient value
            n <- c(n,mineralization(x[length(minute) - 1],w[length(minute)], c))
                                        # augmentation value
            a <- c(a,augmentation(n[length(n)],s,d,aMin,aMax))
                                        # biological o2 demand
            B <- c(B,
                   (Amax * 
                        ((a[length(minute)] * Bscalar) * 
                             (w[length(minute)] / (k + w[length(minute)])))))
                                        # augmented photosynthesis initialization
            A <- c(A,(Amax * (a[length(minute)] * Ascalar) * P[length(minute)]))
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
                        "Biological Oxygen Demand", "Food Amount", 
                        "Nutrients","Augmentation Value")
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


carpenterMod <- function(x0,tf=100,a=1,b=1,r=1,FUN,verbose=FALSE){
    x <- x0
    for (t in 2:tf){
        if (verbose){print(t)}
        x[t] <- x[t-1] + (a - b*x[t-1] + r*get(FUN)(x[t-1]))
    }
    return(x)
}

hill <- function(x,p=100,h=150){x^p / (x^p + h^p)}

noiseSimulator <- function(x0,tf=100,a=1,b=1,r=1,eta=0,FUN,NOISE,verbose=FALSE){
    x <- x0
    for (t in 2:tf){
        if (verbose){print(t)}
        x[t] <- x[t-1] + (a - b*x[t-1] + r*get(FUN)(x[t-1])) + eta*get(NOISE)()
    }
    return(x)
}

normal.noise <- function(){rnorm(1,sd=3)}

lagit <- function(x,k=1,xlab,ylab,type='l',std=FALSE,add=FALSE,col='grey',pch=19,cex=1,lwd=1){
    if (missing(xlab)){xlab <- 'x'}
    if (missing(ylab)){ylab <- expression('x'[t+k])}
    if (std){x <- (x-mean(x))/sd(x)}
    x1 <- x[1:(length(x) - k)]
    x2 <- x[(k + 1):(length(x))]
    return(data.frame(x=x1,y=x2))
}


lagplot <- function(x,k=1,xlab,ylab,type='l',std=FALSE,add=FALSE,col='grey',pch=19,cex=1,lwd=1){
    if (missing(xlab)){xlab <- 'x'}
    if (missing(ylab)){ylab <- expression('x'[t+k])}
    if (std){x <- (x-mean(x))/sd(x)}
    x1 <- x[1:(length(x) - k)]
    x2 <- x[(k + 1):(length(x))]
    if (add){
        if (type == 'p'){
            points(x1,x2,xlab=xlab,ylab=ylab,col=col,pch=pch,cex=cex)
        }else{
            lines(x1,x2,xlab=xlab,ylab=ylab,col=col,lwd=lwd)
        }
    }else{
        plot(x1,x2,xlab=xlab,ylab=ylab,type=type,col=col,pch=pch,cex=cex,lwd=lwd)
    }
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


### ppAnoxia: identify the amount of time that
### the model spends in the anoxic zone

ppAnoxia <- function(x,thresh=0.05,relative=TRUE){
    o2 <- x$Oxygen
    if (relative){out <- length(o2[o2 <= thresh]) / length(o2)}else{
        out <- length(o2[o2 <= thresh])
    }
    return(out)
}

## ppReturn: return time for the pitcher plant o2

ppReturn <- function(x,feed.time=720,thresh=0.00001,minutes=FALSE){
    o2 <- x$Oxygen
    days <- (length(o2)) / 1440
    mids <- seq((feed.time + 1) , (feed.time + 1 + (1440 *
                                                        (days-1))),by=1440)
    o2.mids <- o2[mids]
    feed.day <- head(((1:length(o2.mids))[o2.mids < o2.mids[1]]),1)
    o2.delta <- sum(o2.mids[c(feed.day,length(o2.mids))] * c(-1,1))
    time.delta <- sum(mids[c(feed.day,length(o2.mids))] * c(-1,1))
    if (!minutes){time.delta <- time.delta / 1440}
    rr <- o2.delta / time.delta
    return(rr)
}

#' Function for pitcher plant prey addition
#' MKLau 02February2017
#' @examples
#' sim <- ppSim(days, (fw + fw.perturb) , beta = 4e-06, verbose = TRUE)

ppSimPrey <- function(days = 30, prey.mass = 10,prey.rate = 3,perturb.mass = 10, perturb.rate = 3){

    fw <- rep(c(rep(0,prey.rate - 1),prey.mass),ceiling(days / prey.rate))[1:days]
    fw.perturb <- rep(c(rep(0,perturb.rate - 1),perturb.mass),ceiling(days / perturb.rate))[1:days]
    fw + fw.perturb

}

ddsoSim <- function(days,fW,beta,k){
    x <- ppSim(days=days, 
                    beta = beta, 
                    k = k,
                    foodWeight = fW,
                    )$Oxygen - 
        ppSim(days=days, 
                        beta = beta, 
                        k = k,
                        foodWeight = fW*0,
                        )$Oxygen
    (x - mean(x))/sd(x)
}

min.rss <- function(data,par){
    with(data, sum((ddso -  
                        ddoSim(days,
                               fW,
                               beta = par[1],
                               k = par[2])[1:length(ddso)])^2))
}
