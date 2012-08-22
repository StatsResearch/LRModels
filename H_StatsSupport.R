# Script for H Statistic support
# SVN: $Id: H_StatsSupport.R 595 2012-04-08 21:39:22Z rob $

# Adapted from ...
#   DAVID J. HAND, DEPARTMENT OF MATHEMATICS, IMPERIAL COLLEGE, LONDON
#   d.j.hand@imperial.ac.uk

# This is R code for H, AUC, AUCH, GINI, and KS statistic
# In addition to these statistics, the output includes
#	- the kernel smoothed score distributions of the two classes
#	- the ROC curve and convex hull
# 	- a plot of the minimum loss produced for each value of c
# 	- the weight function implicitly used by the AUC, as a function of score
#	- the weight function implicitly used by the AUC, as a function of c
#	- the weight function used by the AUC measure

# data is in a matrix called 'inp' with two columns
# column 1: classes, labelled 0 or 1
# column 2: classifier scores

Calc_H_Score<-function(case.labels,predictions)
{
    inp.df<-na.omit(data.frame(case.labels,predictions))
    inp<-as.matrix(inp.df)

    n0n1 <- nrow(inp)
    
    x <- t(inp)
    
    # alpha and betad are the parameters in the beta 
    # cost distribution ~ c^alpha * (1-c)^betad
    
    alpha <- 2
    betad <- 2

    #par(mfrow=c(3,2))

    # Smoothed histograms
    class0 <- x[,x[1,]==0]
    class1 <- x[,x[1,]==1]
    
    xmin <- min(x[2,])
    xmax <- max(x[2,])
    
    #plot(density(class0[2,]),xlim=c(xmin,xmax),main= "Kernel smoothed score distributions ",xlab= "Score ")
    #lines(density(class1[2,]),lty=4)

    # order data into increasing scores
    zord <- order(x[2,])
    
    sc <- x[,zord]
    
    n1 <- sum(sc[1,])
    n0 <- n0n1 - n1
    pi0 <- n0/n0n1
    pi1 <- n1/n0n1
    
    # Calculate the raw ROC, replacing any tied
    # sequences by a 'diagonal' in the ROC curve.
    
    # The raw ROC starts at F0[1]=0, F1[1]=0, and ends at
    # F0[K1]=n0, F1[K1]=n1.
    
    F0 <- c(0:n0n1)
    F1 <- c(0:n0n1)
    
    sc <- cbind(sc,sc[,n0n1])
    
    K1 <- 1
    k <- 2
    for (i in 1:n0n1)
    {
        F0[k] <- F0[K1]+(1-sc[1,i])
        F1[k] <- F1[K1]+sc[1,i]
        K1 <- k
        k <- if (sc[2,i+1] == sc[2,i]) (k) else (k+1)
    }
    
    F0 <- F0[1:K1]
    F1 <- F1[1:K1]

    # Plot the ROC
    #plot(F1/n1,F0/n0, xlab= "F1 ",ylab= "F0 ",type= "l", main= "ROC curve and convex hull ")
    #lines(c(0,1),c(0,1),type= "l")

    # Compute KS statistic
    KS <- max((F0/n0) - (F1/n1))

    # Find the upper concave hull
    
    G0 <- c(0:(K1-1))
    G1 <- c(0:(K1-1))


    i <- 1
    hc <- 1
    #cat('K1:',K1,'\n')
    while (i < K1)
    {
        c1 <- c((i+1):K1)
        for (j in (i+1):K1)
        {
            # if(j%%100 == 0) cat('+')
#             if(j%%10000 == 0) cat('\n')
            u1 <- (F1[j]-F1[i])
            u0 <- (F0[j]-F0[i])
            c1[j] <- u1/(u1+u0)
        }
    
        argmin <- i+1
        c1min <- c1[i+1]
        for (k in (i+1):K1)
        {
            # if(k%%100 == 0) cat('.')
#             if(k%%10000 == 0) cat('\n')
            argmin <- if (c1[k] <= c1min) (k) else (argmin)
            c1min <- c1[argmin]
        }
        hc <- hc+1
        G0[hc] <- F0[argmin]
        G1[hc] <- F1[argmin]
        i <- argmin
        #cat('i:',i,'\n')
    }

    G0 <- G0[1:hc]/n0
    G1 <- G1[1:hc]/n1

    # Draw hull
    #lines(G1,G0,type= "l",lty=2,col='red')
    
    # Calculate the LHalpha value

    cost <- c(1:(hc+1))
    b0 <- c(1:hc+1)
    b1 <- c(1:hc+1)
    
    cost[1] <- 0
    cost[hc+1] <- 1
    
    b0[1] <-
      pbeta(cost[1],shape1=(1+alpha), shape2=betad)*
      beta((1+alpha), betad)/   beta(alpha, betad)
    
    b1[1] <-
      pbeta(cost[1],shape1=alpha, shape2=(1+betad))*
      beta(alpha, (1+betad))/   beta(alpha, betad)
    
    b0[hc+1] <-
      pbeta(cost[hc+1],shape1=(1+alpha), shape2=betad)*
      beta((1+alpha), betad)/ beta(alpha, betad)
    
    b1[hc+1] <-
      pbeta(cost[hc+1],shape1=alpha, shape2=(1+betad))*
      beta(alpha, (1+betad))/ beta(alpha, betad)
    
    for (i in 2:hc)
    {
        cost[i] <- pi1*(G1[i]-G1[i-1]) / 
           (pi0*(G0[i]-G0[i-1]) + pi1*(G1[i]-G1[i-1]))
        
        b0[i] <-
          pbeta(cost[i],shape1=(1+alpha), shape2=betad)*
          beta((1+alpha), betad)/ beta(alpha, betad)
        
        b1[i] <-
          pbeta(cost[i],shape1=alpha, shape2=(1+betad))*
          beta(alpha, (1+betad))/ beta(alpha, betad)
    }


    LHalpha <- 0
    for (i in 1:hc)
    {
        LHalpha <- LHalpha + pi0*(1-G0[i])*(b0[(i+1)]-b0[i]) + pi1*G1[i]*(b1[(i+1)]-b1[i])
    }
    
    B0 <- 
      pbeta(pi1,shape1=(1+alpha), shape2=betad)*
      beta((1+alpha), betad)/ beta(alpha, betad)
    
    B1 <-
      pbeta(1,shape1=alpha, shape2=(1+betad))*
      beta(alpha, (1+betad))/ beta(alpha, betad) -
    
      pbeta(pi1,shape1=alpha, shape2=(1+betad))*
      beta(alpha, (1+betad))/ beta(alpha, betad)
    
    H <- 1 - LHalpha/(pi0*B0 + pi1*B1) 


    # Calculate the area under the ROC curve, AUC
    
    K11 <- K1+1
    
    F0[K11] <- n0
    F1[K11] <- n1
    
    F0 <- F0[1:K11]
    F1 <- F1[1:K11]
    
    F0A <- F0[2:K11]
    F0B <- F0[1:K1]
    
    F1A <- F1[2:K11]
    F1B <- F1[1:K1]
    
    AUC <- sum((F0A-F0B)*(n1-(F1A+F1B)/2))/(n0*n1)
    Gini <- 2*AUC - 1
    
    # CALCULATE THE AREA UNDER THE CONVEX HULL, AUCH
    
    AUCH <- 0
    for (i in 1:(hc-1))
    {
    AUCH <- AUCH + G0[i]*(G1[i+1]-G1[i]) + 0.5*(G0[i+1]-G0[i])*(G1[i+1]-G1[i])
    }
    
    # CALCULATE THE MINIMUM LOSS VS c CURVE
    
    Q <- c(1:(hc+1))
    
    for (i in 1:hc)
    {
    Q[i] <- cost[i]*pi0*(1-G0[i]) + (1-cost[i])*pi1*G1[i]
    }
    Q[(hc+1)] <- 0

    #plot(cost,Q,type= "l", main= "Minimum loss by cost ",xlab= "cost ",ylab= "Minimum achievable loss ")
    
    # PLOT THE AUC MIXTURE WEIGHT FUNCTION IN TERMS
    # OF THE SCORE
    
    #plot(density(x[2,]),lty=1,xlab= "Score ",main= " AUC measure weight function of T", ylab= "W(t)")
    
    # PLOT THE AUC MIXTURE WEIGHT FUNCTION IN TERMS
    # OF THE COST

    aucd <- c((n0*G0 + n1*G1),1)
    aucd2 <- c(1, (n0*G0 + n1*G1))
    aucf <- (aucd-aucd2)/n0n1

    #plot(cost[2:hc],aucf[2:hc],type= "h", xlim=c(0,1), ylim=c(0,1),main= "AUC measure weight function of c",xlab= "Cost",ylab= "w(c)")

    # PLOT THE BETA WEIGHT FUNCTION IN TERMS OF THE COST
    
    b <- c(1:100)/100
    y <- dbeta(b,alpha,betad)
    #plot(b,y,type= "l",xlab= "Cost ", main= "H measure weight function of c",ylab= "w(c) ")

    H
    AUC
    Gini
    AUCH
    KS
    
    
    return(list('H'=H,'AUC'=AUC,'Gini'=Gini,'AUCH'=AUCH,'KS'=KS))
    
}
