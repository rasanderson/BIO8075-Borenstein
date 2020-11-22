#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(metafor)
dat <- dat.cannon2006[,1:6]


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How a meta-analysis works"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
#{
            tags$div(sliderInput("ntPROVEIT",
                        "nt for PROVE IT",
                        min = 1000,
                        max = 5000,
                        value = 2099), style = "display:inline-block"),
            tags$div(sliderInput("ncPROVEIT",
                        "nc for PROVE IT",
                        min = 1000,
                        max = 5000,
                        value = 2063), style = "display:inline-block"),
#}
            actionButton("reset", "Reset values")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("metaPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$reset, {
        updateSliderInput(session, "ntPROVEIT", value = 2099)
        updateSliderInput(session, "ncPROVEIT", value = 2063)
    })

    output$metaPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        dat[1, "nt"] <- input$ntPROVEIT
        dat[1, "nc"] <- input$ncPROVEIT

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
