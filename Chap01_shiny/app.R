# Interactive based on Borenstein Chapter 1

library(shiny)
library(metafor)
dat <- dat.cannon2006[,1:6]


# Define UI for application that uses Borenstein Chapter 1
ui <- fluidPage(

    # Application title
    titlePanel("How a meta-analysis works"),
    plotOutput("metaPlot"),
    
    fluidRow(
        column(4,
               h4("PROVE IT")
               ),
        
        column(8,
               sliderInput("yi_1",
                           label = NULL, 
                           min = 0.5,
                           max = 1.5,
                           value = 0.84)
               ),
        ),
        
        fluidRow(
            column(4,
                  h4("A-TO-Z")
            ),

            column(8,
                   sliderInput("yi_2",
                               label = NULL, 
                               min = 0.5,
                               max = 1.5,
                               value = 0.860)
            ),
        ),
    
    fluidRow(
        column(4,
               h4("TNT")
        ),
        
        column(8,
               sliderInput("yi_3",
                           label = NULL, 
                           min = 0.5,
                           max = 1.5,
                           value = 0.801)
        ),
       
    ),
    fluidRow(
        column(4,
               h4("IDEAL")
        ),
        
        column(8,
               sliderInput("yi_4",
                           label = NULL, 
                           min = 0.5,
                           max = 1.5,
                           value = 0.890)
        ),
      
    ),
    
    actionButton("reset", "Reset values")
)


# Define server logic 
server <- function(input, output, session) {
    observeEvent(input$reset, {
        # Reset values back to original
        updateSliderInput(session, "yi_1", value = 0.840)
        updateSliderInput(session, "yi_2", value = 0.860)
        updateSliderInput(session, "yi_3", value = 0.801)
        updateSliderInput(session, "yi_4", value = 0.880)
    })

    output$metaPlot <- renderPlot({

        dat <- escalc(measure="RR", ai=ep1t, n1i=nt, ci=ep1c, n2i=nc, data=dat, slab=trial)
        
        # Modify effect sizes (which are Risk Ratios so have to be logs)
        dat[1, "yi"] <- log(input$yi_1)
        dat[2, "yi"] <- log(input$yi_2)
        dat[3, "yi"] <- log(input$yi_3)
        dat[4, "yi"] <- log(input$yi_4)
        
        res <- rma(yi, vi, data=dat, method="DL")
        dat$weights <- paste0(round(weights(res)), "%")   # weights in % (rounded)
        dat$pvals   <- round(summary(dat)$pval, digits=3) # p-values of the individual trials
        forest(res, xlim=c(-1,2), atransf=exp, at=log(c(2/3, 1, 3/2)),
               header=TRUE, top=2, mlab="Summary", efac=c(0,1,3),
               ilab=data.frame(dat$weights, dat$pvals), ilab.xpos=c(0.8,1.2), ilab.pos=2)
        text(0.8, -1, "100%", pos=2)
        text(1.2, -1, formatC(res$pval, format="f", digits=5), pos=2)
        text(0.8,  6, "Weight",  pos=2, font=2)
        text(1.2,  6, "P-Value", pos=2, font=2)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
