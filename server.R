#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nortest)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    rv <- reactiveValues(data = NULL)
    rj <- reactiveValues(data = NULL)
    
    randBoxU <- reactiveValues(data = NULL)
    randBoxV <- reactiveValues(data = NULL)
    
    d1 <- reactiveValues(data = NULL)
    d2 <- reactiveValues(data = NULL)
    d3 <- reactiveValues(data = NULL)
    
    size <- 100000
    
    observeEvent(input$drawP, {
      rv$data <- 1
      randBoxU$data <- runif(size)
      randBoxV$data <- runif(size)
    })
    
    observeEvent(input$drawR, {
      rj$data <- 1
      d1$data <- runif(1)
      d2$data <- runif(1)
      d3$data <- runif(1)
      
    })
  
    output$distPlot <- renderPlot({
      #size <- 100000
      
      u <- randBoxU$data
      v <- randBoxV$data
      
      x <- rep(0,size)
      y <- rep(0,size)
      
      
      for (i in 1:size){
        x[i] <- sqrt(-2*log(u[i]))*cos(2*pi*v[i])
        y[i] <- sqrt(-2*log(u[i]))*sin(2*pi*v[i])
      }
      
      if(is.null(rv$data)) return()
        
      plot(density(c(x,y)))
      
      
    })
    
    
    
    output$RejPlot <- renderPlot({
      f <- function(x){ #repartia principala
        sqrt(2/pi) * exp(-x ^ 2 / 2) 
      }
      
      g <- function(x){ #repartitia suport
        exp(-x)  
      }
      
      range1 <- seq(-5, 5, by = 0.01)
      range2 <- seq(0, 5, by = 0.01)
      
      if(is.null(rj$data)) return()
      
      plot(range1, f(range1), type = 'l',  ylim = c(0, 2))
      lines(range2, f(1) / g(1) * g(range2))
      lines(-range2, f(1) / g(1)* g(range2))
      
      v <- rep(0, 100000) #vectorul pentru elementele acceptate
      
      for (i in 1 : length(v)){
        repeat{ #testarea unui sample
          x <- -log(runif(1)) 
          y <- runif(1)
          if(y < f(x)/(f(1) / g(1) * g(x))) #conditia de acceptare
          {break;}
        }
        v[i] <- ((-1) ^ (runif(1) < 0.5)) * x
      }
      
      if(is.null(rj$data)) return()
      
      hist(v, prob = T, breaks = 50)
      curve(dnorm(x,0,1), add = T)
      
      
    })
    
    output$variables <- renderUI({
      
      withMathJax(helpText("Grafice"))
      
    })
    
    

})
