library(shiny)
#library(ggplot2)

ui=fluidPage(
  fileInput("file", "Choose CSV file:",
            multiple=TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  #for choosing columns
  actionButton("update","Update"),
  selectInput("days", "Select Day Column", choices=NULL),
  selectInput("trainingLoad", "Select Training Load",choices=NULL),
  selectInput("performance", "Select Performance", choices=NULL),
  #to see whats going on
  tableOutput("Preview")
)



server=function(input,output,session){
  df <- eventReactive(input$update,{
    req(input$file)
    out=read.csv(input$file$datapath)
    names=names(out)
    updateSelectInput(session,"performance", "Select Performance", choices=names)
    updateSelectInput(session,"trainingLoad","Select Training Load",choices=names)
    updateSelectInput(session,"days", "Select Day Column", choices=names)
    out
    })
  output$Preview=renderTable({head(df())})
    
}

shinyApp(ui, server)



