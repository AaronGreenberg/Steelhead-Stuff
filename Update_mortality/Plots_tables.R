library(xtable)
table_hessian <- function(out,z,sdcon)
{
return(as.data.frame(eigen(z$hessian)$values))

}   


table_z <- function(out,z,sdcon)
{

    e <- z$par
    s <- sdcon
    out <-as.data.frame(rbind(e,s))

    if(length(s)==3)
    {
  
        colnames(out) <- c("Fresh Survival","Init","SD")
        rownames(out) <- c("Estimate","SE")
        }
    if(length(s)==4)
        {
        colnames(out) <- c("Fresh Survival 1","Fresh Survival 2","Init","SD")
        rownames(out) <- c("Estimate","SE")
        }

    if(length(s)>4)
    {
            e <- z$par[1:3]
            s <- sdcon[1:3]
           out <-as.data.frame(rbind(e,s))

        colnames(out) <- c("Fresh Survival 1","Init","SD")
        rownames(out) <- c("Estimate","SE")
        }

    
return(out)

}   

table_covariance <- function(out,z,sdcon)
{

    cov <- as.data.frame(solve(z$hessian))

    if(length(sdcon)==3)
        {
        colnames(cov) <- c("Fresh Survival","Init","SD")
        }
    if(length(sdcon)==4)
    {
 colnames(cov) <- c("Fresh Survival 1","Fresh Survival 2","Init","SD")
        }

    return(cov)
}

table_dev <- function(out,z,sdcon)
{

    out <- as.data.frame(z$par[4:length(z$par)])

    if(length(sdcon)>4)
    {
 colnames(out) <- c("Dev")
        }

    return(out)
}

table_corr <- function(out,z,sdcon)
{
     c <- as.data.frame(solve(z$hessian))
    corr=c/(sdcon%o%sdcon)

    if(length(sdcon)==3)
        {
        colnames(corr) <- c("Fresh Survival","Init","SD")
        }
    if(length(sdcon)==4)
    {
 colnames(corr) <- c("Fresh Survival 1","Fresh Survival 2","Init","SD")
        }
return(corr)
}
table_ages <- function(out,z,sdcon)
{
return(as.data.frame(out$ages))
}   
table_data <- function(out,z,sdcon)
{
return(as.data.frame(out$data))
}   



plot_magic <- function(out,sdcon,z)
{

    print("plot magic")
    print(sdcon)

    if(length(sdcon)==3){
       
        conmat <-matrix(0,nrow=1000,ncol=(years))

        conmat <-matrix(0,nrow=1000,ncol=(years))
        tab= MASS::mvrnorm(1000,z$par,solve(z$hessian))
        confi <- tab[,1]
        confi2 <- tab[,2]
       
        for( i in 1:1000)
        {
            outm <- modelinits(confi[i],confi2[i],DPSDPE,years,Percent4,Percent5,Percent6,Repeat6,data)
            conmat[i,] <- outm$ages$SpawnerAdult
        }
    }


    
    if(length(sdcon)==4){
        print("WTF")
        print(z$par)
        print(sdcon)
        print("sowhat")
    print("sillygoose")
    print(length(data$SpawnerYr))
    print(length(data$FosterCount))
       
        conmat <-matrix(0,nrow=1000,ncol=(years))
        tab= MASS::mvrnorm(1000,z$par,solve(z$hessian))
        confi <- tab[,1]
        confi2 <- tab[,2]
        confi3 <- tab[,3]
        print("So am I an idiot")
        print(years)
        print(length(conmat[1,]))
        
        for( i in 1:1000)
        {
            outm2 <- modelinit(confi[i],confi2[i],confi3[i],DPSDPE,years,Percent4,Percent5,Percent6,Repeat6,data)
            conmat[i,] <- outm2$ages$SpawnerAdult
        }
        print(length(outm2$ages$SpawnerAdult))
    }



    if(length(sdcon)>4){
        print("WTF")
        print(z$par)
        print(sdcon)
        print("sowhat")
    print("sillygoose")
    print(length(data$SpawnerYr))
    print(length(data$FosterCount))
       
        conmat <-matrix(0,nrow=1000,ncol=(years))
        tab= MASS::mvrnorm(1000,z$par,solve(z$hessian))
        confi <- tab[,1]
        confi2 <- tab[,2]
        confi3 <- tab[,4:length(z$par)]
        print("So am I an idiot")
        print(years)
        print(length(conmat[1,]))
        
        for( i in 1:1000)
        {
            outm2 <- modelinitdev(confi[i],confi2[i],confi3[i,],DPSDPE,years,Percent4,Percent5,Percent6,Repeat6,data)
            conmat[i,] <- outm2$ages$SpawnerAdult
        }
        
    }

    
     par(mfrow=c(1,2))
    plot(data$SpawnerYr,data$FosterCount,ylim=c(0,600),xlab="Time", ylab="Foster Count Observed")
        u <- colQuantiles(conmat,probs=.975,na.rm=TRUE)
        l<- colQuantiles(conmat,probs=.0275,na.rm=TRUE)
        uu <- colQuantiles(conmat,probs=.80,na.rm=TRUE)
        ll<- colQuantiles(conmat,probs=.20,na.rm=TRUE)
        lines(data$SpawnerYr,u,col="black",lwd=.5)
        lines(data$SpawnerYr,l,col="black",lwd=.5)
        lines(data$SpawnerYr,uu,col="grey",lwd=1)
        lines(data$SpawnerYr,ll,col="grey",lwd=1)
    legend("topright",legend=paste("NLL (with prior)::",signif(z$value,2)),box.col="white")
    points(data$SpawnerYr,data$FosterCount)
    points(data$SpawnerYr[1:7],out$ages$SpawnerAdult[1:7],col="magenta")
    lines(out$data$SpawnerYr,out$ages$SpawnerAdult,type="l",col="red")
    lines(out$data$SpawnerYr,c(data$smoltsurvival)*1000,type="l",col="green")
    #abline(v=2006,col="blue")
    ## plot(data$FosterCount[8:(years-1)],out$ages$SpawnerAdult[8:(years-1)],ylim=c(0,400),type="o",xlab="Foster Count Observed", ylab="Foster Count Predicted")
    ## linm=lm(out$ages$SpawnerAdult[8:(years-1)]~data$FosterCount[8:(years-1)])
    ## abline(linm,col="red")
    ## print(summary(linm))
    if(length(z$par)<=4){
    yres=(-log(out$ages$SpawnerAdult[8:(years)])+log(data$FosterCount[8:(years)]))
    plot(data$SpawnerYr[1:(years)],data$SpawnerYr[1:(years)]*NA,type="h",col="blue",ylab="log(obs)-log(pred)",xlab="Time",ylim=c(min(yres,na.rm=TRUE),max(yres,na.rm=TRUE)))
    points(data$SpawnerYr[8:(years)],yres,type="h",col="blue",ylab="residual",xlab="Time",ylab=NA,xlab=NA)
    }
    if(length(z$par)>4)
    {   plot(data$SpawnerYr[1:(years)],(z$par[4:(length(z$par))]),col="red",type="o",ylab="(Dev)",xlab="Time")
        ## axis(side = 1, at = pretty(range(z$par[4:(length(z$par))])))
        ## mtext("exp(Dev)", side = 4, line = 1)  
        }
    
                                               
}







wormplot <- function(mat,main,main2,years=years,ymax=-1,time)
{
    u <- colQuantiles(mat,probs=.975,na.rm=TRUE)
    l<- colQuantiles(mat,probs=.0275,na.rm=TRUE)
    uu <- colQuantiles(mat,probs=.80,na.rm=TRUE)
    ll<- colQuantiles(mat,probs=.20,na.rm=TRUE)
    yearvec <- 1984:(1985+years)
    print(max(u,na.rm=TRUE))
    if(ymax<0){
        ymax=min(c(1.1*max(u,na.rm=TRUE),10000))
        ymin=max(c(.9*min(l,na.rm=TRUE),-10))
    print("ymin ymax")
    print(c(ymin,ymax))

       plot(yearvec[1:(years)],colMedians(mat,na.rm=TRUE)[1:(years)],ylim=c(ymin,ymax),type="l",xlab="Time", ylab=main2,main=main)
    }else{
       ymax=min(c(1.1*max(u,na.rm=TRUE),10000))
       ymin=max(c(0,.9*min(l,na.rm=TRUE)),na.rm=TRUE)
           print("ymin ymax")
    print(c(ymin,ymax))
       plot(yearvec[1:(years)],colMedians(mat,na.rm=TRUE)[1:(years)],ylim=c(ymin,ymax),type="l",xlab="Time", ylab=main2,main=main)

    }
    print("ymin ymax")
    print(c(ymin,ymax))
    polygon(c(which(ll>=0), rev(which(ll>=0))), c(ll[which(ll>=0)], rev(uu[which(ll>=0)])), col = "darkgrey")
    lines(yearvec[1:(years)],colMedians(mat,na.rm=TRUE)[1:(years)],type="l",lwd=1.4,col="black")
    for(i in sample(1:600,15))
    {       
        lines(yearvec[1:(years)],mat[i,1:(years)],col="orange",type="l",lwd=.5)
    }
    lines(yearvec[1:(years)],u[1:(years)],col="red",lwd=1.5)
    lines(yearvec[1:(years)],l[1:(years)],col="red",lwd=1.5)
    lines(yearvec[1:(years)],uu[1:(years)],col="black",lwd=1)
    lines(yearvec[1:(years)],ll[1:(years)],col="black",lwd=1)
        abline(v=time$start0,col="red")
    abline(v=time$start1,col="blue")
    abline(v=time$start2,col="green")
    abline(v=time$start3,col="purple")
       axis(side=1,at=yearvec[1:(years)],labels=FALSE,col="red")
       axis(side=1,at=yearvec[seq(7,years,by=10)],col="black")
    
}

histplot <- function(vec,main,main2,med=TRUE)
{   print("histplot maxes")
    print(max(vec))
    print(min(vec))
    print(max(vec,na.rm=TRUE))
    print(min(vec,na.rm=TRUE))
    print("head vec")
    head(vec)
    print(main)
    print(main2)
    hist(vec,xlab=main,freq=FALSE,main=main2,breaks=20)#xlim=c(min(vec,na.rm=TRUE)-10^6,max(vec,na.rm=TRUE)+10^6))
    if(med==TRUE){
        leg1=paste("25%-",signif(quantile(vec,probs=c(.25),na.rm=TRUE),2),"\n","50%-",signif(quantile(vec,probs=c(.5),na.rm=TRUE),2),"\n","75%-",signif(quantile(vec,probs=c(.75),na.rm=TRUE),2),sep="")
    }
   if(med==FALSE){
        leg1=paste("mean-",signif(mean(vec,na.rm=TRUE),2))
    }
    
    legend("topright",legend=leg1,box.col="white")

}    


plot_forcast <- function(list,worm,main,years=73)
    {
if(worm==1){
        par(mfcol=c(2,2))
        wormplot(list$conmat,main,"Spawners",years,ymax=-1,list$time)
        wormplot(list$simmat,main,"Simulated Marine Surival",years,-1,list$time)
        wormplot((list$SARmat),main,"Simulated SAR",years,-1,list$time) 
        wormplot(list$RSmat,main,"Recruits/Spawner",years,-1,list$time)
        #wormplot(list$RSmat[,30:45],main,"Recruits/Spawner",15,-1)
        
}
if(worm==2){        
    par(mfrow=c(3,3))
     v=colMeans(list$QETmat)
        plot(v[6:(years)],type="o",xlab="time",ylab="Percent less than 116 Spawners")
        histplot(list$RSvec,"Recruits Per Spawner",main2=main)
       histplot(list$Abunvec,"Geomean Spawners",main2=main)
        histplot(list$QETvec,"QET Threshold 116",main2=main,med=FALSE)
        histplot(list$SARvec,"SAR",main2=main)
    histplot(list$marineSvec,"Last 15 Years Average Marine Survival",main2=main)
    histplot(list$marineSvecShort,"First 5 Years Average Marine Survival",main2=main)
    ## histplot(list$DPEvec,"DPE",main2=main)
    ## histplot(list$DPSvec,"DPS",main2=main)
    histplot(list$DPEvec*list$DPSvec,"DPE*DPS",main2=main)


}


if(worm==3){
        par(mfcol=c(2,3))
        wormplot(list$QETamat,main,"Moving Average QET",years,-1,list$time)
        wormplot(list$DPSmat,main,"DPS",years,1,list$time)
        wormplot(list$DPEmat,main,"DPE",years,1,list$time)
        wormplot(list$simmat,main,"Sims",years,1,list$time)
        histplot(apply(list$simmat,2,mean),"mean",main2=main)
        
fnacf <- function(vv)
{

    m <- acf(vv,lag.max=1,plot=FALSE)
    return(unlist(m[1]$acf))
    
    }

        histplot(apply(log(list$simmat),1,fnacf),"rho",main2=main)
}

}

table_forcast <- function(list,main){ #Note only report mean of QET
    s=data.frame(PM=c("Recruits Per Spawner (R/S)","Geomean Spawners (NOR)","QET Threshold -116 (Mean not median)","SAR","Last 15 years Average Marine Survival","First 5 Years Average Marine Survival","DPS","DPE","DPE*DPS"),
                 means=c(mean(list$RSvec,na.rm=TRUE),mean(list$Abunvec,na.rm=TRUE),mean(list$QETvec,na.rm=TRUE),mean(list$SARvec,na.rm=TRUE),mean(list$marineSvec,na.rm=TRUE),mean(list$marineSvecShort,na.rm=TRUE)
                        ,mean(list$DPSvec,na.rm=TRUE),mean(list$DPEvec,na.rm=TRUE),mean(list$DPEvec*list$DPSvec,na.rm=TRUE)),
                 sd=c(sd(list$RSvec,na.rm=TRUE),sd(list$Abunvec,na.rm=TRUE),sd(list$QETvec,na.rm=TRUE),sd(list$SARvec,na.rm=TRUE),sd(list$marineSvec,na.rm=TRUE),sd(list$marineSvecShort,na.rm=TRUE),
                      sd(list$DPSvec,na.rm=TRUE),sd(list$DPEvec,na.rm=TRUE),sd(list$DPEvec*list$DPSvec,na.rm=TRUE)),
                 cv=c(sd(list$RSvec,na.rm=TRUE),sd(list$Abunvec,na.rm=TRUE),sd(list$QETvec,na.rm=TRUE),sd(list$SARvec,na.rm=TRUE),sd(list$marineSvec,na.rm=TRUE),sd(list$marineSvecShort,na.rm=TRUE),
                      sd(list$DPSvec,na.rm=TRUE),sd(list$DPEvec,na.rm=TRUE),sd(list$DPEvec*list$DPSvec,na.rm=TRUE))/c(mean(list$RSvec,na.rm=TRUE),mean(list$Abunvec,na.rm=TRUE),mean(list$QETvec,na.rm=TRUE),mean(list$SARvec,na.rm=TRUE),mean(list$marineSvec,na.rm=TRUE),mean(list$marineSvecShort,na.rm=TRUE)
                                                                                                                     ,mean(list$DPSvec,na.rm=TRUE),mean(list$DPEvec,na.rm=TRUE),mean(list$DPEvec*list$DPSvec,na.rm=TRUE)),
                 lower=c(quantile(list$RSvec,.025,na.rm=TRUE),quantile(list$Abunvec,.025,na.rm=TRUE),quantile(list$QETvec,.025,na.rm=TRUE),
                         quantile(list$SARvec,.025,na.rm=TRUE),quantile(list$marineSvec,.025,na.rm=TRUE),quantile(list$marineSvecShort,.025,na.rm=TRUE),quantile(list$DPSvec,.25,na.rm=TRUE),quantile(list$DPEvec,.25,na.rm=TRUE),quantile(list$DPEvec*list$DPSvec,.25,na.rm=TRUE)),
                 median=c(median(list$RSvec,na.rm=TRUE),median(list$Abunvec,na.rm=TRUE),mean(list$QETvec,na.rm=TRUE),median(list$SARvec,na.rm=TRUE),
                          median(list$marineSvec,na.rm=TRUE),median(list$marineSvecShort,na.rm=TRUE),median(list$DPSvec,na.rm=TRUE),median(list$DPEvec,na.rm=TRUE),median(list$DPEvec*list$DPSvec,na.rm=TRUE)),
                 upper=c(quantile(list$RSvec,.975,na.rm=TRUE),quantile(list$Abunvec,.975,na.rm=TRUE),quantile(list$QETvec,.975,na.rm=TRUE),quantile(list$SARvec,.975,na.rm=TRUE),quantile(list$marineSvec,.975,na.rm=TRUE),quantile(list$marineSvecShort,.975,na.rm=TRUE),
                        quantile(list$DPSvec,.975,na.rm=TRUE),quantile(list$DPEvec,.975,na.rm=TRUE),quantile(list$DPEvec*list$DPSvec,.975,na.rm=TRUE)))
                colnames(s) <- c("","Mean","SD","CV",".0275","Median",".0975")
    return(list(table=s,RS=list$RSvec,Abund=list$Abunvec,QET=list$QETvec))
        }



        
   




egg <- function(xx,yy,col1="red",bw=350,title="m",log.vals=FALSE){
  #function for examining trade offs between performance metrics under uncertainty
                                        #original code provided by Aaron Greenberg on 6 May 2022


    xx <- na.omit(xx)
    yy <- na.omit(yy)
  if(log.vals){
    xx <- log(xx)
    yy <- log(yy)
  }
    print("max xx yy")
    print(c(max(xx,na.rm=TRUE), max(yy,na.rm=TRUE)))
      print(c(min(xx,na.rm=TRUE), min(yy,na.rm=TRUE)))      
  bwx <- abs((max(xx,na.rm=TRUE)-min(xx,na.rm=TRUE))/bw)
  bwy <- abs((max(yy,na.rm=TRUE)-min(yy,na.rm=TRUE))/bw)
  if(log.vals){
    est <- bkde2D(cbind(xx,yy),bandwidth=c(bwx,bwy),gridsize=c(100, 100)) #kernel density estimate
  }else{
                                        # est <- bkde2D(cbind(xx,yy),bandwidth=c(bwx,bwy),gridsize=c(81, 81),range.x=list(c(0,max(xx,na.rm=TRUE)+1.5*bwx),c(0,max(yy,na.rm=TRUE)+1.5*bwy))) #kernel density estimate
       est <- bkde2D(cbind(xx,yy),bandwidth=c(bwx,bwy),gridsize=c(300,300)) #kernel density estimate
  }
  est$fhat <- est$fhat/max(est$fhat,na.rm=TRUE)
  lvs=c(0.05,0.2,0.9) #what contour lines where?
  maxct <- max(lvs)
  nlvs <- length(lvs)
  thelines <- contourLines(est$x1,est$x2,est$fhat,levels=lvs)#three contours is enough
 
  if(log.vals){
    polygon(exp(thelines[[1]]$x),exp(thelines[[1]]$y),col=col1,border="red",lwd=1.1) #95% CI
    polygon(exp(thelines[[2]]$x),exp(thelines[[2]]$y),col=NA,border="blue",lwd=0.2) #80% CI
    polygon(exp(thelines[[3]]$x),exp(thelines[[3]]$y),col=NA,border="black",lwd=0.1) #10% CI
    points(median(exp(xx),na.rm=TRUE),median(exp(yy),na.rm=TRUE),pch=1,cex=.25)

    if(title=="NAA"){scalem=.3}
    if(title=="A1"){scalem=-.2}
    if(title=="A2A"){scalem=.1}
    if(title=="A2B"){scalem=.35}
    if(title=="A3A"){scalem=.25}
    if(title=="A3B"){scalem=.15}
    if(title=="A4"){scalem=.3}
    arrows(median(xx,na.rm=TRUE)+scalem*median(xx,na.rm=TRUE),median(yy,na.rm=TRUE)+scalem*median(yy,na.rm=TRUE),median(xx,na.rm=TRUE),median(yy,na.rm=TRUE),cex=.5,length=.05)
  }else{
    polygon(thelines[[1]]$x,thelines[[1]]$y,col=col1,border="red",lwd=1.1) #95% CI
    polygon(thelines[[2]]$x,thelines[[2]]$y,col=NA,border="blue",lwd=0.2) #80% CI
    polygon(thelines[[3]]$x,thelines[[3]]$y,col=NA,border="black",lwd=0.1) #10% CI
    points(median(xx,na.rm=TRUE),median(yy,na.rm=TRUE),pch=1,cex=.25,col="magenta")
    if(title=="NAA"){scalem=.3}
    if(title=="A1"){scalem=-.25}
    if(title=="A2A"){scalem=.15}
    if(title=="A2B"){scalem=.4}
    if(title=="A3A"){scalem=.35}
    if(title=="A3B"){scalem=.2}
    if(title=="A4"){scalem=.15}
 

    us=runif(1,0.5,1.5)
    arrows(median(xx,na.rm=TRUE)+scalem*median(xx,na.rm=TRUE),median(yy,na.rm=TRUE)+us*50*log(median(yy,na.rm=TRUE)),median(xx,na.rm=TRUE),median(yy,na.rm=TRUE),cex=.5,length=.05)
     u=ifelse(us>0,.1,-.1)
    text(median(xx,na.rm=TRUE)+(scalem+u/10)*median(xx,na.rm=TRUE),median(yy,na.rm=TRUE)+(us+u)*50*log(median(yy,na.rm=TRUE)),labels=title,cex=.75)
  }
  #points(median(xx,na.rm=TRUE),median(yy,na.rm=TRUE),pch=3)
}


iso_plot <- function(file,iso,river=1){
    if(river==1)
        {
Alt1 <- read.csv("./Summary_Steelhead/Alt1_FOS_age2.csv",header=TRUE)
Alt1 <- mean(Alt1$x.DPS*Alt1$x.DPE)
Alt2a <- read.csv("./Summary_Steelhead/Alt2a_FOS_age2.csv",header=TRUE)
Alt2a <- mean(Alt2a$x.DPS*Alt2a$x.DPE)
Alt2b <- read.csv("./Summary_Steelhead/Alt2b_FOS_age2.csv",header=TRUE)
Alt2b <- mean(Alt2b$x.DPS*Alt2b$x.DPE)
Alt3a<- read.csv("./Summary_Steelhead/Alt3a_FOS_age2.csv",header=TRUE)
Alt3a <- mean(Alt3a$x.DPS*Alt3a$x.DPE)
Alt3b<- read.csv("./Summary_Steelhead/Alt3b_FOS_age2.csv",header=TRUE)
Alt3b <- mean(Alt3b$x.DPS*Alt3b$x.DPE)
Alt4<- read.csv("Summary_Steelhead/Alt4_FOS_age2.csv",header=TRUE)
Alt4 <- mean(Alt4$x.DPS*Alt4$x.DPE)
NAA <- read.csv("./Summary_Steelhead/NAA_FOS_age2.csv",header=TRUE)
NAA <- mean(NAA$x.DPS*NAA$x.DPE)


svglite(file)
x=(1:30)/15
y=(1:30)/30
par(mar = c(5, 5, 5, 5)) 
image(x,y,iso,ylab="DPS*DPE",xlab="Marine Survival Scaling")
contour(x,y,iso,add=TRUE,labcex=2)

abline(h=Alt1)
axis(4, at = (Alt1+.02), labels = c("Alt1"),las=2,tck=0)
axis(4, at = (Alt1), labels = NA,las=2)
abline(h=Alt2a)
axis(2, at = (Alt2a), labels = c("Alt2a"),las=2)
abline(h=Alt2b)
axis(4, at = (Alt2b-.02), labels = c("Alt2b"),las=2,tck=0)
abline(h=Alt3a)
axis(2, at = Alt3a, labels = c("Alt3a"),las=2)
abline(h=Alt3b)
axis(4, at = (Alt3b+.02), labels = c("Alt3b"),las=2,tck=0)
axis(4, at = (Alt3b), labels = NA,las=2)
abline(h=Alt4)
axis(2, at = Alt4, labels = c("Alt4"),las=2)
abline(h=NAA)
axis(4, at = (NAA-.02), labels = c("NAA"),las=2,tck=0)
dev.off()
}
    if(river==2)
        {
Alt1 <- read.csv("./Summary_Steelhead/Alt1_GPR_age2.csv",header=TRUE)
Alt1 <- mean(Alt1$x.DPS*Alt1$x.DPE)
Alt2a <- read.csv("./Summary_Steelhead/Alt2a_GPR_age2.csv",header=TRUE)
Alt2a <- mean(Alt2a$x.DPS*Alt2a$x.DPE)
Alt2b <- read.csv("./Summary_Steelhead/Alt2b_GPR_age2.csv",header=TRUE)
Alt2b <- mean(Alt2b$x.DPS*Alt2b$x.DPE)
Alt3a<- read.csv("./Summary_Steelhead/Alt3a_GPR_age2.csv",header=TRUE)
Alt3a <- mean(Alt3a$x.DPS*Alt3a$x.DPE)
Alt3b<- read.csv("./Summary_Steelhead/Alt3b_GPR_age2.csv",header=TRUE)
Alt3b <- mean(Alt3b$x.DPS*Alt3b$x.DPE)


svglite(file)
x=(1:30)/15
y=(1:30)/30
par(mar = c(5, 5, 5, 5)) 
image(x,y,iso,ylab="DPS*DPE",xlab="Marine Survival Scaling")
contour(x,y,iso,add=TRUE,labcex=2)

abline(h=Alt1)
axis(4, at = (Alt1), labels = c("Alt1"),las=2,tck=0)
axis(4, at = (Alt1), labels = NA,las=2)
abline(h=Alt2a)
axis(2, at = (Alt2a), labels = c("Alt2a"),las=2)
abline(h=Alt2b)
axis(4, at = (Alt2b), labels = c("Alt2b"),las=2,tck=0)
abline(h=Alt3a)
axis(2, at = (Alt3a-.02), labels = c("Alt3a"),las=2)
abline(h=Alt3b)
axis(4, at = (Alt3b), labels = c("Alt3b"),las=2,tck=0)
axis(4, at = (Alt3b), labels = NA,las=2)
dev.off()
}
    if(river==3)
        {
Alt1 <- read.csv("./Summary_Steelhead/Alt1_DET_age2.csv",header=TRUE)
Alt1 <- mean(Alt1$x.DPS*Alt1$x.DPE)
Alt2a <- read.csv("./Summary_Steelhead/Alt2a_DET_age2.csv",header=TRUE)
Alt2a <- mean(Alt2a$x.DPS*Alt2a$x.DPE)
Alt2b <- read.csv("./Summary_Steelhead/Alt2b_DET_age2.csv",header=TRUE)
Alt2b <- mean(Alt2b$x.DPS*Alt2b$x.DPE)
Alt3a<- read.csv("./Summary_Steelhead/Alt3a_DET_age2.csv",header=TRUE)
Alt3a <- mean(Alt3a$x.DPS*Alt3a$x.DPE)
Alt3b<- read.csv("./Summary_Steelhead/Alt3b_DET_age2.csv",header=TRUE)
Alt3b <- mean(Alt3b$x.DPS*Alt3b$x.DPE)
Alt4<- read.csv("Summary_Steelhead/Alt4_DET_age2.csv",header=TRUE)
Alt4 <- mean(Alt4$x.DPS*Alt4$x.DPE)
NAA <- read.csv("./Summary_Steelhead/NAA_DET_age2.csv",header=TRUE)
NAA <- mean(NAA$x.DPS*NAA$x.DPE)


svglite(file)
x=(1:30)/15
y=(1:30)/30
par(mar = c(5, 5, 5, 5)) 
image(x,y,iso,ylab="DPS*DPE",xlab="Marine Mortality Scaling")
contour(x,y,iso,add=TRUE,labcex=2)

abline(h=Alt1)
axis(4, at = (Alt1), labels = c("Alt1"),las=2,tck=0)
axis(4, at = (Alt1), labels = NA,las=2)
abline(h=Alt2a)
axis(2, at = (Alt2a), labels = c("Alt2a"),las=2)
abline(h=Alt2b)
axis(4, at = (Alt2b), labels = c("Alt2b"),las=2,tck=0)
abline(h=Alt3a)
axis(2, at = Alt3a, labels = c("Alt3a"),las=2)
abline(h=Alt3b)
axis(4, at = (Alt3b), labels = c("Alt3b"),las=2,tck=0)
axis(4, at = (Alt3b), labels = NA,las=2)
abline(h=Alt4)
axis(2, at = Alt4, labels = c("Alt4"),las=2)
abline(h=NAA)
axis(4, at = (NAA), labels = c("NAA"),las=2,tck=0)
dev.off()

if(river==4)
{
svglite(file)
x=(1:30)/15
y=(1:30)/30
par(mar = c(5, 5, 5, 5)) 
image(x,y,iso,ylab="DPS*DPE",xlab="Marine Survival Scaling")
contour(x,y,iso,add=TRUE,labcex=2)
dev.off()
    }

}


}



        
        
   


