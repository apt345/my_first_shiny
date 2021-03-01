#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(Stat2Data)

data(Airlines)
Airlines$IndOHare=as.factor(Airlines$IndOHare)
Airlines$IndDelta=as.factor(Airlines$IndDelta)

list_choices <-  colnames(Airlines)
names(list_choices) <- colnames(Airlines)

# Define UI for application that draws a histogram
#añadir introduccion y conclusiones (y referencias)
ui <- navbarPage("Arturo's first Shiny app",
                 tabPanel("Airlines",
                          fluidPage(
                              sidebarLayout(sidebarPanel(
                                  selectInput("select", label = h3("Plot by variable"), 
                                              choices = list_choices,
                                              selected = 1)
                              ), mainPanel(
                                  h3("Variable distributions"),
                                  plotlyOutput(outputId = "hello")
                              )
                              ))),
                 tabPanel("Correlations",fluidPage(
                   sidebarLayout(sidebarPanel(
                     selectInput("select2", label = h3("Correlation by variable"), 
                                 choices = list_choices,
                                 selected = 1)
                   ), mainPanel(h3("Correlation with chosen variable"), plotOutput(outputId = "correlations"))))

                                             )
) 

col_scale <- scale_colour_discrete(limits = list_choices)


#define function to draw correlations

library(rcompanion)#cramer v for jet num
  
correlationsplot=function(inputvariable){
  y = as.factor(Airlines[[inputvariable]])
  
  corr_OnTime=matrix(nrow=length(Airlines),ncol=length(Airlines))
  rownames(corr_OnTime)=colnames(Airlines)
  colnames(corr_OnTime)=colnames(Airlines)
  for(i in(1:(length(Airlines)))){
    
    corr_OnTime[inputvariable, colnames(Airlines)[i]]=abs(cramerV(Airlines[,i],y))#take absolute value
    
  }
  #corr_OnTime[inputvariable, inputvariable]=1#add manually the correlation with its own
  
  corr_OnTime <- sort(corr_OnTime[inputvariable,], decreasing = T)
  corr=data.frame(corr_OnTime)
  ggplot(corr,aes(x = row.names(corr), y = corr_OnTime)) + 
    geom_bar(stat = "identity", fill = "lightblue") + 
    scale_x_discrete(limits= row.names(corr)) +
    labs(x = "", y = inputvariable, title = "Correlations") + 
    theme(plot.title = element_text(hjust = 0, size = rel(1.5)),
          axis.text.x = element_text(angle = 45, hjust = 1))

}

plotbarplots=function(inputvariable){
  data=as.factor(Airlines[[inputvariable]])
  datalevels=levels(data)
  
  #build a dataframe to use plot_ly
  term1=sum(data==datalevels[1])
  term2=sum(data==datalevels[2])
  
  counts=c(term1,term2)
  classes=datalevels
  
  finaldataframe=as.data.frame(counts,classes)
  a=plot_ly(finaldataframe,x=~classes,  y=~counts,type="bar")
  
  return(a)
}

# Define server logic required to draw a histogram and correlations
server <- function(input, output) {
    
    output$hello <- renderPlotly(plotbarplots(input$select))
    output$correlations=renderPlot(correlationsplot(input$select2))
}

# Run the application 
shinyApp(ui = ui, server = server)
