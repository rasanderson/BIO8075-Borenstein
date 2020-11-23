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
        column(3,
               sliderInput("nt_1",
                           "nt for PROVE IT",
                           min = 1000,
                           max = 5000,
                           value = 2099)
               ),
        
        column(3, 
               sliderInput("nc_1",
                           "nc for PROVE IT",
                           min = 1000,
                           max = 5000,
                           value = 2063)
               ),
        column(3,
               sliderInput("ep1t_1",
                           "ep1t for PROVEIT",
                           min = 50,
                           max = 250,
                           value = 147)
               ),
        column(3,
               sliderInput("ep1c_1",
                           "ep1c for PROVEIT",
                           min = 50,
                           max = 250,
                           value = 172)
        )
    ),
    
    actionButton("reset", "Reset values")
)


# Define server logic 
server <- function(input, output, session) {
    observeEvent(input$reset, {
        # Reset values back to original
        updateSliderInput(session, "nt_1", value = 2099)
        updateSliderInput(session, "nc_1", value = 2063)
        updateSliderInput(session, "ep1t_1", value = 147)
        updateSliderInput(session, "ep1c_1", value = 172)
    })

    output$metaPlot <- renderPlot({
        # Modify inputs as needed
        dat[1, "nt"] <- input$nt_1
        dat[1, "nc"] <- input$nc_1
        dat[1, "ep1t"] <- input$ep1t_1
        dat[1, "ep1c"] <- input$ep1c_1

        dat <- escalc(measure="RR", ai=ep1t, n1i=nt, ci=ep1c, n2i=nc, data=dat, slab=trial)
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
