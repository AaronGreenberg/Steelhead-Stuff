#this script plots cpue vs cumulative catch for each area.
#It omits
library(shiny)
library(dplyr)
source("Steel_head_age.R")
library(xtable)
library(stats)
shinyServer(function(input, output){
data=read.csv("steelhead.csv",sep="\t")
print(data)


output$agePlot <- renderPlot({
                                        #tmp <- dataInput()
    N <- c(input$N1,input$N2,input$N3,input$N4s,input$N4o,input$N5s,input$N5o,input$N6)
    print(N)
    print("so")
    print(as.numeric(input$age))
    plot_full_age(input$years,input$SF,input$SP,input$SM,input$F,input$a,input$b,N,input$R,input$type,as.numeric(input$age))

})



output$recruitPlot <- renderPlot({
    N <- c(input$N1,input$N2,input$N3,input$N4s,input$N4o,input$N5s,input$N5o,input$N6)
    plot_recruit_spawner(input$years,input$SF,input$SP,input$SM,input$F,input$a,input$b,N,input$R,input$type)

})

output$age_table <- renderDataTable({
        N <- c(input$N1,input$N2,input$N3,input$N4s,input$N4o,input$N5s,input$N5o,input$N6)
    table_full_age(input$years,input$SF,input$SP,input$SM,input$F,input$a,input$b,N,input$R,input$type)
    
})
})

