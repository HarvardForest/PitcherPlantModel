source('ppme.R')
## out.dir <- '../results/ppSim'

### Sensitivity

## set length of time and parameter space
n1 <- 5;n2 <- 3;n3 <- 30
days <- n1 + n2 + n3

foodWeight <- sapply(seq(0,10,by=1),
                     function(x,n1,n2,n3) c(rep(0,n1),rep(x,n2),rep(0,n3)),
                     n1=n1,n2=n2,n3=n3)
d <- seq(-5,5,by=1)
beta <- seq(0.001,0.003,length=11)

## loop over all combos of foodWeight, d and 
## beta, to simulate the oxygen output from the
## pitcher plant micro-ecosystem

for (i in 1:ncol(foodWeight)){
    for (j in d){
        for (k in beta){
            out <- pitcherPlantSim(days=days,foodWeight=foodWeight[,i],d=j,beta=k)
            write.csv(out,file=paste(out.dir,max(foodWeight[,i]),j,k,sep='_'),row.names=FALSE)
        }
    }
}
