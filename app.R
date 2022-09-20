#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
#
#
#ui <- fluidPage("Hello World!")
#
#server <- function(input, output, session) {
#}
#
## Run the application 
#shinyApp(ui = ui, server = server)


library(plotly)
library(dplyr)
library(shiny)



draw3dGraph <- function(expression="(x1^0.5)*(x2^0.5)", x1min=1, x1max=10, x2min=1, x2max=10, accuracy=1,
                        budget_expression="x1 + x2", budgetMoney=10, showFeasibleSet=TRUE, showMaxPoint=TRUE, zOffset=0.1){

  x1 <- seq(x1min,x1max,accuracy) 
  x2 <- seq(x2min,x2max,accuracy)
  
  #if the specified budget is greater than the maximum cost attainable with current x1, x2: 
  if(budgetMoney>=max(eval(parse(text=budget_expression)))){
    return(NULL)
    stop(paste("Specified budget too big for current context. \n Either increase x1/x2 range or decrease budget to: ",max(eval(parse(text=budget_expression)))-0.1 ))
  } 
  
  
  utilityFxn <- function(x1,x2,expressionfxn=expression){
    
    return(as.numeric(eval(parse(text=expressionfxn))))
  }
  
  feasibleConsumptionSet <- function(x1, x2, budget=budgetMoney,expressionfxn=budget_expression){
    #combination of all x1x2 that retrieve that budget
    
    if(as.numeric(eval(parse(text=expressionfxn)))<=budget){

    return(utilityFxn(x1,x2,expressionfxn=expression)) }else{return(NA)}
    #adjustment prevents clipping ;) trust me. 
  }
  
  maxUtility <- function(x1Arg, x2Arg, zArg){
    #z is the matrix of utilities corresponding to x1 and x2 
    maxValue <- max(zArg, na.rm = TRUE) #find max value of utility in matrix
    indicesMaxValues <- which(zArg==maxValue, arr.ind=TRUE) #find indices of all values equal to the max value
    x1Indices <- indicesMaxValues[,"row"]#separate x1 indices
    x2Indices <- indicesMaxValues[,"col"]#separate x2 indices
    x1Return <- unlist(x1Arg[x1Indices])
    x2Return <- unlist(x2Arg[x2Indices])
    utilReturn <- unlist(rep(maxValue, length(x1Return)))
    
    returnValue <- list("x1"= x1Return,
                        "x2"=x2Return,
                        "u"=utilReturn)
  }
  

  contourBudgetPoints <- function(x1, x2, budget_expressionArg=budget_expression, budgetMoneyArg=budgetMoney){
    budgetSet <- function(x1,x2){
      if(as.numeric(eval(parse(text=budget_expressionArg)))<=budgetMoneyArg){
        return(list(x1,x2))
      }else{return(NA)}
    }
    
    budgetSetV <- outer(x1,x2, Vectorize(budgetSet))
    budgetSetV <- budgetSetV[rowSums(is.na(budgetSetV)) != ncol(budgetSetV), ]
    budgetSetV <- budgetSetV[,colSums(is.na(budgetSetV)) != nrow(budgetSetV) ]
    print(budgetSetV)
    
    
    point1 <- matrix(budgetSetV[nrow(budgetSetV),])
    point1 <- matrix(point1[is.na(point1)==FALSE])
    point1 <- unlist(point1[length(point1)])
    
    
    point2 <-  matrix(matrix(budgetSetV[,ncol(budgetSetV)])[is.na(matrix(budgetSetV[,ncol(budgetSetV)]))==FALSE])
    print("point 2")
    print(point2)
    point2 <- unlist(point2[length(point2)])
    
    x1Return <- c(point1[1], point2[1])
    x2Return <- c(point1[2], point2[2])
    
    message(x1Return)
    message(x2Return)
    

    return(list("x1"=x1Return, "x2"=x2Return))

  }
  

  
  #consumption set: 
  u <- outer(x1, x2, Vectorize(utilityFxn))
  print(u)
  #the outer fxn will generate a u matrix
  #            | x2 value 1 | x2 value 2 | ... | x2 value J
  #            |-------------------------------------------
  # x1 value 1 | 
  # x1 value 2 |
  #    ...
  # x1 value I |
  
  
  #feasible_consumption_set:
  budget <- outer(x1,x2,Vectorize(feasibleConsumptionSet) )
 

  
  
  
  #make 3D figure:
  labelsConsumption <- paste0("Consumption Set <br>Good 2: ", rep(x2, times=length(x1)),
                                 "<br>Good 1: ", rep(x1, each=length(x2)),
                                 "<br>Utility: ", t(u)) %>% matrix(length(x1), length(x2))
  fig_3d <- plot_ly(x=x1, y=x2,z=t(u), name="C set") %>% add_surface(colorscale=list(c(0, 1), c("tan", "blue")),
                                                                     hoverinfo="text",
                                                                     text=labelsConsumption) %>% layout(scene=list(xaxis=list(title="Good 1"),
                                                                            yaxis=list(title="Good 2"),
                                                                            zaxis=list(title="Utility")),
                                                                 showlegend=FALSE,
                                                                 margin = list(
                                                                   l = 0,
                                                                   r = 0,
                                                                   b = 0,
                                                                   t = 50,
                                                                   pad = 0
                                                                 ),
                                                                 title="Utility Function") %>% hide_colorbar()
  
  #option: show feasible consumption set ON consumption set: 
  if(showFeasibleSet==TRUE){
    labelsFeasible <- paste0("Feasible Set <br>Good 2: ", rep(x2, times=length(x1)),
                     "<br>Good 1: ", rep(x1, each=length(x2)),
                     "<br>Utility: ", t(budget)) %>% matrix(length(x1), length(x2))
    fig_3d <- fig_3d %>% add_surface(x=x1,
                                     y=x2,
                                     z=t(budget)+zOffset, #offset is graphical
                                     type="surface",
                                     inherit=FALSE,
                                     colorscale=list(c(0, 1), c("tan", "gold")),
                                     name="Feasible set",
                                     hoverinfo="text",
                                     text=labelsFeasible) %>% hide_colorbar()
  }
  
  if(showMaxPoint==TRUE){
    maxValues <- maxUtility(x1Arg = x1, x2Arg=x2, zArg=budget)
    fig_3d <- fig_3d %>% add_trace(x=unlist(maxValues["x1"]),
                                   y=unlist(maxValues["x2"]),
                                   z=unlist(maxValues["u"]),
                                   inherit=FALSE,
                                   mode="markers",
                                   type="scatter3d",
                                   marker=list(size=10,color ="black"),
                                   name="Utility maximizing point") %>% hide_colorbar()
  }
 

  
  fig_2d <- plot_ly(x=x1, y=x2,z=t(u) , type="contour", contours=list(showlabels=TRUE),
                    colorscale=list(c(0, 1), c("tan", "blue"))) 
  fig_2d <- fig_2d %>% layout(margin = list(l = 0,
                                            r = 0,
                                            b = 0,
                                            t = 50,
                                            pad = 0),
                              title="Indifference Curves",
                              xaxis=list(title="Good 1"),
                              yaxis=list(title="Good 2")) 
  
  if(showFeasibleSet==TRUE){
    budgetLineFxnOutput <- contourBudgetPoints(x1, x2)
    x1BudgetLine <- budgetLineFxnOutput[["x1"]]
    x2BudgetLine <- budgetLineFxnOutput[["x2"]]
    fig_2d <- fig_2d %>% add_trace(x=x1BudgetLine,
                                   y=x2BudgetLine,
                                   mode="lines",
                                   inherit=FALSE,
                                   line=list(color="gold"),
                                   name="Budget Line")
  }
  
  if(showMaxPoint==TRUE){
    maxValues <- maxUtility(x1Arg = x1, x2Arg=x2, zArg=budget)
    fig_2d <- fig_2d %>% add_trace(x=unlist(maxValues["x1"]),
                                   y=unlist(maxValues["x2"]),
                                   inherit=FALSE,
                                   mode="markers",
                                   marker=list(size=10,color ="black"),
                                   name="Utility maximizing point")
  }
  
  
  
  figs <- list()
  figs[["3d"]] <- fig_3d
  figs[["2d"]] <- fig_2d

return(figs)}


ui <- fluidPage(
  titlePanel("Utility Function Visualizer"),
  
  
  fluidRow(
  column(4, div(style = "white-space: nowrap;",
      h5('U(x1,x2)=',style="display:inline-block"),
      div(style="display: inline-block; width: 100%;",
          textInput("mathInput", label = "", value = "8 * x1^(0.25) * x2^(0.75)", placeholder="8 * x1^(0.25) * x2^(0.75)", width = "100%"))))),
  
  
  fluidRow(column(2, 
         div(style = "white-space: nowrap;",
             h5('Budget Constraint:',style="display:inline-block"),
             div(style="display: inline-block; width: 100%;",
                 textInput("budgetInput", label = "expression", value = "x1+(2*x2)", placeholder="x1+(2*x2)", width = "100%")),
             h5('=',style="display:inline-block"),
         div(style="display: inline-block; width: 100%;",
             textInput("budgetConstant", label = "budget constant", value = 16, placeholder=16, width = "100%"))))),

  
  fluidRow(column(width=6,sliderInput("xRange", 
              "Select the range for x1 and x2", 
              min=0, 
              max=100, 
              value=c(0,10),
              width="100%") ),
           column(width=6,sliderInput("specificity",
                                       "How specific should the visualization be?",
                                       min=0.1, 
                                       max=10, 
                                       value=0.1, 
                                       width="100%",
                                       step=0.1))),
  fluidRow(column(width=10, 
                  actionButton("evaluate", "Evaluate!", width="100%"))),
  
  fluidRow(column(width=8,plotlyOutput("utility3D", height="600px", width="100%"),style='padding:0px;', offset=0),
           column(width=4, plotlyOutput("utilityContour", width="100%"),style='padding:0px;', offset=0)),
  

  
)

server <- function(input, output, session){
  

  
  utility3DReactive <- eventReactive(input$evaluate,{ 
    req(input$xRange)
    req(input$mathInput)
    req(input$specificity)
    return(draw3dGraph(expression=input$mathInput, x1min=input$xRange[1],
                                                   x1max=input$xRange[2],
                                                   x2min=input$xRange[1],
                                                   x2max=input$xRange[2],
                                                   accuracy=input$specificity,
                                                   budget_expression=input$budgetInput,
                                                   budgetMoney=as.numeric(input$budgetConstant))[["3d"]])
  })
  output$utility3D <- renderPlotly({utility3DReactive()})
  

  
  utilityContourReactive <- eventReactive(input$evaluate,{
    req(input$xRange)
    req(input$mathInput)
    req(input$specificity)
    return(draw3dGraph(expression=input$mathInput, x1min=input$xRange[1],
                       x1max=input$xRange[2],
                       x2min=input$xRange[1],
                       x2max=input$xRange[2], 
                       accuracy=input$specificity,
                       budget_expression=input$budgetInput,
                       budgetMoney=as.numeric(input$budgetConstant))[["2d"]])
  })
  output$utilityContour <- renderPlotly(utilityContourReactive())
  
  
  observeEvent(input$evaluate, 
               { utilityContourReactive()
                 utility3DReactive()})
  }


shinyApp(ui, server)





