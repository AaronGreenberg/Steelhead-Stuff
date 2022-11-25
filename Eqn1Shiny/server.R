#this script plots cpue vs cumulative catch for each area.
#It omits
library(shiny)
library(dplyr)
source("Steel_head_eqn1.R")

shinyServer(function(input, output){
data=read.csv("steelhead.csv",sep="\t")
print(data)


output$distPlot <- renderPlot({
    #tmp <- dataInput()                  
    DPS <- densitymagic(input$DDPS,input$DPSmean,input$DPSsd)
    DPE <- densitymagic(input$DDPE,input$DPEmean,input$DPEsd)
    eggs <- densitymagic(input$Deggs,input$eggsmean,input$eggssd)
    freshwatersurvival <- densitymagic(input$DFS,input$freshwatersurvivalmean,input$freshwatersurvivalsd)
    marinesurvival <- densitymagic(input$DMS,input$marinesurvivalmean,input$marinesurvivalsd)
    DPSDPEplot1(DPS,DPE,eggs,freshwatersurvival,marinesurvival,.1381,data)

})


output$damPlot <- renderPlot({
    #tmp <- dataInput()                  
    DPS <- densitymagic(input$DDPS,input$DPSmean,input$DPSsd)
    DPE <- densitymagic(input$DDPE,input$DPEmean,input$DPEsd)
    eggs <- densitymagic(input$Deggs,input$eggsmean,input$eggssd)
    freshwatersurvival <- densitymagic(input$DFS,input$freshwatersurvivalmean,input$freshwatersurvivalsd)
    marinesurvival <- densitymagic(input$DMS,input$marinesurvivalmean,input$marinesurvivalsd)
    DPSDPEplot2(DPS,DPE,eggs,freshwatersurvival,marinesurvival,.1381,data)

})


output$recruitPlot <- renderPlot({
    DPS <- densitymagic(input$DDPS,input$DPSmean,input$DPSsd)
    DPE <- densitymagic(input$DDPE,input$DPEmean,input$DPEsd)
    eggs <- densitymagic(input$Deggs,input$eggsmean,input$eggssd)
    freshwatersurvival <- densitymagic(input$DFS,input$freshwatersurvivalmean,input$freshwatersurvivalsd)
    marinesurvival <- densitymagic(input$DMS,input$marinesurvivalmean,input$marinesurvivalsd)

    DPSDPEplot3(DPS,DPE,eggs,freshwatersurvival,marinesurvival,.1381,data)


})


})

