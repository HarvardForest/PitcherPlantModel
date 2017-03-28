source('global.R')

days=3
foodWeight=c(0,1,0)
beta=4.5e-05
d=5
k=1
Amin=0.2
Amax=1
m=0
aMax=2 
aMin=1 
s=1 
feedingTime=720
c=1
x0=0
w0=0
w.w=75
bound.max=FALSE
verbose=FALSE 
photo.constant = FALSE 
photo.val = 1

    if (length(foodWeight) < days){
        foodWeight <- rep(foodWeight,days)[1:days]
    }
    ## Initialization ##
                                        # time keeper
    minute <- 1
                                        # simulate photosynthesis as fixed values
    P <- photo(days,Amax,Amin)
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
    B <- a * (w[1]/(k+w[1]))
                                        # augmented photosynthesis initialization
    A <- a * P[1]
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
            B <- c(B,a[length(minute)] * 
                       (w[length(minute)] / (k + w[length(minute)])))
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
