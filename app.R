#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpart)
library(ranger)

ui <- dashboardPage(
  dashboardHeader(title = "Auckland Bus"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("No Alert", tabName = "before"),
      menuItem("Alert or TL", tabName = "after")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "before",
              fluidRow(
                box(title="Condition",
                    selectInput("Rb","Route",choices=rnbs),
                    selectInput("Hb","Holiday or Workday",choices=c("Workday","Weekend","Holiday")),
                    selectInput("Wb","Weather",choices=c("Good weather","Bad weather"))
                ),
                box(
                  title = "Predicted punctuality rate",
                  verbatimTextOutput("preb")
                ),
                box(
                  title = "Score",
                  verbatimTextOutput("scoreb")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "after",
              fluidRow(
                box(title = "Condition",
                    selectInput("Ra","Route",choices = rnas),
                    selectInput("Aa","Alert level",choices = sort(unique(alert[,2]))),
                    selectInput("Ha","Holiday or Workday",choices=c("Workday","Weekend","Holiday")),
                    selectInput("Wa","Weather",choices=c("Good weather","Bad weather"))
                ),
                box(
                  title = "Predicted punctuality rate",
                  verbatimTextOutput("prea")
                ),
                box(
                  title = "Score",
                  verbatimTextOutput("scorea")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
    load("Models.RData")
    preb<-reactive({
      modelb<-p1mod[[match(input$Rb,rnbs)]]
      routedatab<-p1all[[match(input$Rb,rnbs)]]
      h<-{
        if(input$Hb=="Holiday")
          h=2
        if(input$Hb=="Weekend")
          h=1
        else
          h=0
      }
      if(!h %in% unique(routedatab$h)){
        "Do not have data, please check the bus timetable"}
      else{
      w<-{
        if(input$Wb=="Good weather")
          w<-0
        else
          w<-1
      }
      r<-mean(routedatab[,2][which(routedatab$h==h&routedatab$w==w)])
      b<-mean(routedatab[,3][which(routedatab$h==h&routedatab$w==w)])
      if(is.na(r)){
        "Do not have data, please check the bus timetable"
      }
      else{
      new<-data.frame(r=r,b=b,h=h,w=w)
      predict(modelb,new)$predictions
      }}
    })
    output$preb<-renderText(preb())
    scoreb<-reactive(
      if(is.character(preb()))
       "No score"
      else if(preb()>=0.98){
        "excellent"}
      else if(preb()<0.98&preb()>=0.95){
        "very good"}
      else if(preb()<0.95&preb()>=0.9){
        "normal"}
      else if(preb()<0.9){
        "risk of delay"}
    )
    output$scoreb<-renderText(scoreb())
    
    prea<-reactive({
      modela<-p2mod[[match(input$Ra,rnbs)]]
      routedataa<-p2all[[match(input$Ra,rnbs)]]
      h<-{
        if(input$Hb=="Holiday")
          h=2
        if(input$Hb=="Weekend")
          h=1
        else
          h=0
      }
      if(!h %in% unique(routedataa$h)){
        "Do not have data, please check the bus timetable"}
      else{
      w<-{
        if(input$Wb=="Good weather")
          w<-0
        else
          w<-1
      }
      a<-as.numeric(input$Aa)
      r<-mean(routedataa[,2][which(routedataa$h==h&routedataa$w==w&routedataa$a==a)])
      b<-mean(routedataa[,3][which(routedataa$h==h&routedataa$w==w&routedataa$a==a)])
      if(is.na(r)){
        "Do not have data, please check the bus timetable"
      }
      else{
        new<-data.frame(r=r,b=b,h=h,w=w,a=a)
        predict(modela,new)
      }}
    })
    output$prea<-renderText(prea())
    scorea<-reactive(
      if(is.character(prea()))
        "No score"
      else if(prea()>=0.98){
        "excellent"}
      else if(prea()<0.98&preb()>=0.95){
        "very good"}
      else if(prea()<0.95&preb()>=0.9){
        "normal"}
      else if(prea()<0.9){
        "risk of delay"}
    )
    output$scorea<-renderText(scorea())
}
# Run the application 
shinyApp(ui = ui, server = server)
