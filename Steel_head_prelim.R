recruitperspawner <- function(N1,N0,F,SF,SP,SM,Rr) #The model 
{
    rps_observed <- N1/N0
    rps_predicted <- F*SF*SP*SM*(1+Rr) #only need to estimate SF?
  return(Rps=list(rps_obs=rps_observed,rps_pre=rps_predicted,epsilon=rps_predicted-rps_observed))

}


DPSDPE <-  function(DPS,DPE,N1,N0,F,SF,SM,Rr)
{
    SJUS <- .6 #Foster to SJU survival 
    SP <- DPS*DPE*SJUS
    rps_observed <- N1/N0
    rps_predicted <- F*SF*SP*SM*(1+Rr) 
  return(Rps=list(rps_obs=rps_observed,rps_pre=rps_predicted,epsilon=rps_predicted-rps_observed))

}   


                                        #Obviously the stuff below is just testing the above routines.



print("So lets test out the above routines!")
print("lets get data!")
data=read.csv("steelhead.csv",sep="\t",header=T)
print(data)
print("So do we have rounding error from cutting and pasting data?")
print("data$Virgin/data$Spawner-data$R_S")
data$Virgin/data$Spawner-data$R_S
print("seems so, but checking excel and carrying more digits makes the difference go away.")
print("I think the inputs in this are mostly okay, so lets check it out!")
print("so now we just need to find the value of freshwater survival (fs) that makes things cool.")
fn <- function(fs){recruitperspawner(data$Virgin,data$Spawner,2083,fs,23.8/100,2.66/100,.14)}$eps
fnsum <- function(fs){sum(fn(fs)^2)} #just sum of squares... for now. 
f <- .20
z=optimize(fnsum,c(0,1))
print("So the value of freshwater mortality in equation 1 is:")
print(z$minimum)
print("This seems pretty close to 4.4 %... Look only a few percent off:")
percent=(abs(z$minimum-.044)/((z$minimum+.044)/2)*100)
print(percent)
print(paste(signif(percent,6), "%")) 
print("So I think we are probably making some progress?")
fn <- function(fs){recruitperspawner(data$RV,data$Spawner,2083,fs,23.8/100,2.66/100,.14)}$eps
fnsum <- function(fs){sum(fn(fs)^2)} #just sum of squares... for now. 
f <- .20
z=optimize(fnsum,c(0,1))
print("So the value of freshwater mortality in equation 1 is:")
print(z$minimum)
print("This seems pretty close to 5.06 %... Look only a few percent off:")
percent=(abs(z$minimum-.0506)/((z$minimum+.0506)/2)*100)
print(percent)
print(paste(signif(percent,6), "%")) 
print("So I think we are probably making some progress?")
print("So DPS and DPE?")
print("DPS=.6")
print("DPE=.66")
print("Pretty sure this is the same case as above")
fn <- function(fs){DPSDPE(.6, .6, data$RV,data$Spawner,2083,fs,2.66/100,.14)}$eps
fnsum <- function(fs){sum(fn(fs)^2)} #just sum of squares... for now. 
f <- .20
z=optimize(fnsum,c(0,1))
print("So the value of freshwater mortality in equation 1 is:")
print(z$minimum)
print("This seems pretty close to 5.6 %... Look only a few percent off:")
percent=(abs(z$minimum-.056)/((z$minimum+.056)/2)*100)
print(percent)
print(paste(signif(percent,6), "%")) 

print("Wait! The spreadsheet doesn't seem to do the optimization for DPSDPE?")
print(DPSDPE(.76, .98, data$RV,data$Spawner,2083,.044,2.66/100,.14)$rps_pre)
print(DPSDPE(.39, .7, data$RV,data$Spawner,2083,.044,2.66/100,.14)$rps_pre)

print("Outputs from the spreadsheet version in the \"this is where we want to get to eventually (plus performance vs fraction transplanted above Greenpeter after gen 1, 2, 3, 4)\" email")
print("Wait! The spreadsheet doesn't seem to do the optimization for DPSDPE?")
print("Freshwater survival 6.4 %")
print(DPSDPE(.76, .98, data$RV,data$Spawner,2083,.064,2.66/100,.14)$rps_pre)
print(DPSDPE(.39, .7, data$RV,data$Spawner,2083,.064,2.66/100,.14)$rps_pre)

print("Freshwater survival 5.1 %")
print(DPSDPE(.76, .98, data$RV,data$Spawner,2083,.051,2.66/100,.14)$rps_pre)
print(DPSDPE(.39, .7, data$RV,data$Spawner,2083,.051,2.66/100,.14)$rps_pre)




print("So to include uncertainty... are we basically doing something like this?")
print("With the correct inputted distributions? I just made these distributions up as prototypes. They are not informed by data.")
eggs <- rnorm(5000,2084,50)#the distribution of female eggs
freshwatersurvival <- rnorm(5000,.051,.01)#distribution of freshwater survival 
RS <- DPSDPE(.39, .7, data$RV,data$Spawner,eggs,freshwatersurvival,2.66/100,.14)$rps_pre

par(mfcol=c(1,2))
hist(eggs)
hist(freshwatersurvival)
X11()
hist(RS,xlim=c(0,max(c(1.2,max(RS)))),xlab="N1/N0")
abline(v=1)
