graphics.off()

fft0 <- function(z, inverse=FALSE) {
       n <- length(z)
       if(n == 0) return(z)
       k <- 0:(n-1)
       ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n
       vapply(1:n, function(h) sum(z * exp(ff*(h-1))), complex(1))
     }



data<- read.csv("Spawner_recruit/observed3.csv",sep="\t")
print(data)
data$smoltsurvival=data$smoltsurvival/100
fq=fft(data$smoltsurvival[1:26])
## print(fq)
## fq=fft0(data$smoltsurvival[1:26])
## print(fq)



## #x11()
## #plot(data$smoltsurvival,type="l")
## fq2=(fq)*exp(complex(1,real=0,imaginary=1)*(1:26/26+runif(26,0,2*pi)))
## #points(Re(fft(fq2,inverse=TRUE)/26),col="magenta")

## print("omg")
## print(fq2)
## print(fft(fq2,inverse=TRUE)/26)

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))

  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Years?)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}


x11()
par(mfcol=c(1,2))
plot.frequency.spectrum(fq)
plot.frequency.spectrum(fq2)



x11()
plot(data$smoltsurvival,type="l",ylim=c(0,.1))
acf1 <- 1:6*0
acf2 <- 1:6*0
acf3 <- 1:6*0
maxy <- 1:6*0
sumy <- 1:6*0
r <- runif(6,0,2*pi)
for(i in 1:6)
{   
    fq3 <- fq*exp(complex(26,real=0,imaginary=1:26/26+runif(26,0,2*pi))) #complex(modulus=Mod(fq), argument= Arg(fq)+runif(26,0,pi))
#points(Re(fft(fq2,inverse=TRUE)/26),col="magent)
    v=abs(Re(fft(fq3,inverse=TRUE)/length(fq3)))
    lines(v,col="grey",type="l",lwd=.012)
    ## a <- acf(c(v,v,v),lag=3,plot=FALSE)
    ## acf1[i]=(a[1])
    ## acf2[i]=(a[2])
    ## acf3[i]=(a[3])
    ## maxy[i]=max(v)
    ## sumy[i]=sum(v)
}





## d <- acf(data$smoltsurvival[1:26],lag=4,plot=FALSE)
## x11()
## par(mfcol=c(1,5))
## hist(unlist(acf1))
## abline(v=unlist(d[1]),col="red")
## hist(unlist(acf2))
## abline(v=unlist(d[2]),col="red")
## hist(unlist(acf3))
## abline(v=unlist(d[3]),col="red")
## hist(unlist(maxy))
## abline(v=max(data$smoltsurvival[1:26]),col="red")
## hist(unlist(sumy))
## abline(v=sum(data$smoltsurvival[1:26]),col="red")




