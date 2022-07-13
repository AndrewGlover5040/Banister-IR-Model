library(shiny)
library(ggplot2)

#need to do
#   optionally plot performance
#   automatically create day column
#   add 0 to day column if there is no zero day




#cute functional programing stuff
params_tmp <- c("p_0","k_1","k_2","tau_1","tau_2")

inputPar <- function(id){
  textInput(id, label = paste("Choose",id,sep=" "), value = NULL, placeholder ="number")
}

inputParams <- purrr::map(params_tmp,inputPar)


vars <- tibble::tribble(
  ~ id,   ~ label,
  "days",   "Select Day Variable",
  "trainingLoad", "Select Training Load",   
  "performance",    "Select Performance"
)

myInput <- function(id,label){
  selectInput(id,label,choices=NULL)
}
inputsData <- purrr::pmap(vars, myInput)



#model functions
#clean up parameters to function
predictedPerformance <- function(params,Training.Load,day=length(Training.Load)){
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

SSE <- function(params,Training.Load,Performance,day=length(Training.Load)){
  Pred=predictedPerformance(params,Training.Load,day)
  #Performance=Performance[1:day-1]
  error=Performance[!is.na(Performance)] - Pred[which(!is.na(Performance))]
  error=error[!is.na(error)]
  SSE=sum(error^2)
  return(SSE)
}

optim_par <- function(v,Training.Load,Performance,day=length(Performance)){
  x=optim(par = v,  
          fn = SSE, Training.Load = Training.Load, 
          Performance = Performance,
          day=day)
  return(x$par)
}


#Influence Functions

#could be computing the wrong days
Influence=function(params,startDay,t_p=0){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  out=c(rep(0,(t_p-startDay)))
  n=t_p-startDay
  for(i in 1:n){
    out[i]=k_1*exp(-(n-i)/tau_1)-k_2*exp(-(n-i)/tau_2)
  }
  return(out)
}

get_t_n=function(params){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  t_g=(tau_1*tau_2)/(tau_1-tau_2)*log(k_2/k_1)
}

get_t_g=function(params){
  k_1=params[2]; k_2=params[3]; tau_1=params[4]; tau_2=params[5]
  t_g=(tau_1*tau_2)/(tau_1-tau_2)*log((k_2*tau_1)/(k_1*tau_2))
  return(t_g)
}



#appUI
ui=fluidPage(
  tabsetPanel(
    id="wizard",
    type="hidden",
    
    #choosing dataset...
    tabPanel("page_1",
      column(3,
        fileInput("file", "Choose CSV file:",
                  multiple=TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
    #...which lets you go to the next page when one is selected    
        uiOutput("nextPage"),
      ),
      column(9,tableOutput("head"))
    ),
    
    #Choosing inputs and graphing
    tabPanel("page_2",
      column(3,
             actionButton("page_21", "Change Data"),
             inputsData
             ),
      
      column(2,
             actionButton("optim", "Optimize parameters"),
             inputParams
             ),
    
      column(7,
             actionButton("graph", "Graph!!"),
             plotOutput("plotMain"),
             plotOutput("plotTrainingLoad"),
             plotOutput("plotInfluence")
             )
    ),
      
      
      #to see what's up
      #tableOutput("Preview"),
      # verbatimTextOutput("out")
    
  )
)


#appServer
server=function(input,output,session){
  
  #page wizard
  output$nextPage <- renderUI({
    req(input$file)
    actionButton("page_12", "Change Inputs")
  })
  
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  
  
  #reading dataset 
  df <- eventReactive(input$file,{
    req(input$file)
    out=read.csv(input$file$datapath)
    out
  })
  
  
  name=reactive(names(df()))
  
  observe({
    updateSelectInput(session,"performance", "Select Performance", choices=name())
    updateSelectInput(session,"trainingLoad","Select Training Load",choices=name())
    updateSelectInput(session,"days", "Select Day Variable", choices=name())
  })
  
  
  output$head=renderTable(head(df()))
  
  input_Params <- reactive({
    tmp=c(input$p_0,input$k_1,input$k_2,
          input$tau_1,input$tau_2)
    tmp=purrr::map_dbl(tmp,as.double)
    tmp
  })
  
  pred_perf <- reactive({
    predictedPerformance(input_Params(),df()[[input$trainingLoad]])
  })
  
  days <- reactive({
    df()[[input$days]]
  })
  
  #for testing 
  #output$out=renderPrint(Pred_Perf())
  
  updateInputPar <- function(id,value){
    updateTextInput(session=session,inputId=id, 
                    label=paste("Choose",id,sep=" "), 
                    value=value, 
                    placeholder="number"
                    )
  }

  optimParams <- observeEvent(input$optim,{
    tmp=optim_par(input_Params(),df()[[input$trainingLoad]],
                  df()[[input$performance]])
    purrr::map2(params_tmp,tmp,updateInputPar)
  })
  
  
  
  #Main Plot
  plotMain <- eventReactive(input$graph, {
    ggplot(df(),aes(x=days(),y=pred_perf()))+
      geom_line(aes(y=pred_perf(),colour="black"), size=1)+
      geom_point(aes(y = df()[[input$performance]], color="red"), shape = 1)+
      scale_color_manual("", values = c("black", "red"),
                         labels = c("Predicted Performance",
                                    "Actual Performance"))+
      labs(x="Days", y="Performance")
      
  })

  output$plotMain <- renderPlot({
    plotMain()
  })
  
  
  
  #influence plot
  df_influ <- reactive({
    out=data.frame(rev(-1*days()),
               Influence(input_Params(),-length(days()))
               )
  })
  
  plotInflu <- eventReactive(input$graph,{
    ggplot(df_influ(),aes(df_influ()[[1]],df_influ()[[2]]))+
      geom_line()+
      labs(x="Days",y="Influence")
  })
  
  output$plotInfluence <- renderPlot({plotInflu()})
  
  
  #training load plot
  plotTrain <- eventReactive(input$graph,{
    ggplot(df(),aes(days(),df()[[input$trainingLoad]]))+
      geom_bar(stat = "identity")+
      labs(x="Days",y="Training Load")
  })
  
  output$plotTrainingLoad=renderPlot(plotTrain())
}


shinyApp(ui,server)


