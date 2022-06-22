library(shiny)
library(ggplot2)

#cute functional programing stuff
inputPar=function(id){
  textInput(id, label = paste("Choose",id,sep=" "), value = NULL, placeholder ="number")
}
params_tmp=c("p_0","k_1","k_2","tau_1","tau_2")
inputParams=purrr::map(params_tmp,inputPar)


vars <- tibble::tribble(
  ~ id,   ~ label,
  "days",   "Select Day Variable",
  "trainingLoad", "Select Training Load",   
  "performance",    "Select Performance"
)

myInput=function(id,label){
  selectInput(id,label,choices=NULL)
}
inputsData <- purrr::pmap(vars, myInput)



#model functions
Predicted.Performance=function(params,Training.Load,day=length(Training.Load)){
  p_0=params[1]; k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  out=c(); sum_1=0; sum_2=0
  for(t in 1:(day)){
    for (s in 1:t){
      sum_1=sum_1+exp(-(t-s)/tau_1)*Training.Load[s]
      sum_2=sum_2+exp(-(t-s)/tau_2)*Training.Load[s]
    }
    out=append(out,p_0+k_1*sum_1-k_2*sum_2)
    sum_1=0; sum_2=0
  }
  return(out)
}

SSE=function(params,Training.Load,Performance,day=length(Training.Load)){
  Pred=Predicted.Performance(params,Training.Load,day)
  #Performance=Performance[1:day-1]
  error=Performance[!is.na(Performance)] - Pred[which(!is.na(Performance))]
  error=error[!is.na(error)]
  SSE=sum(error^2)
  return(SSE)
}



#appUI
ui=fluidPage(
  #choosing dataset and appropriate variables
  column(3,
    fileInput("file", "Choose CSV file:",
              multiple=TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    actionButton("updateData","Update"),
    inputsData
  ),
    
  #to get parameters
  column(3,
    actionButton("updateParameters", "Update Parameters"),
    inputParams
  ),
  
  #plotting model
  column(6,
    plotOutput("plot")
  ),
  
  #to see what's up
  tableOutput("Preview"),
  verbatimTextOutput("out")
)


#appServer
server=function(input,output,session){
  
  df <- eventReactive(input$updateData,{
    req(input$file)
    out=read.csv(input$file$datapath)
    names=names(out)
    #clean this up later
    updateSelectInput(session,"performance", "Select Performance", choices=names)
    updateSelectInput(session,"trainingLoad","Select Training Load",choices=names)
    updateSelectInput(session,"days", "Select Day Column", choices=names)
    out
  })
  
  output$Preview=renderTable(head(df()))
  
  parameters=eventReactive(input$updateParameters,{
    tmp=c(as.double(input$p_0),as.double(input$k_1),as.double(input$k_2),
          as.double(input$tau_1),as.double(input$tau_2))
    # tmp=purrr::map(tmp,as.double)
    # tmp=as.vector(tmp)
    # tmp
    tmp
  })
  
  Pred_Perf=reactive({
    Predicted.Performance(parameters(),df()[[input$trainingLoad]])
    })
  #for testing 
  output$out=renderPrint(Pred_Perf())
  
  output$plot=renderPlot({
    ggplot(df(),aes(x=df()[[input$days]],y=Pred_Perf()))+
      geom_line(aes(y=Pred_Perf(),colour="black"), size=1)+
      geom_point(aes(y = df()[[input$performance]], color="red"), shape = 1)+
      scale_color_manual("", values = c("black", "red"), 
                         labels = c("Predicted Performance",
                                    "Actual Performance"))
  })
}
 

shinyApp(ui,server)


