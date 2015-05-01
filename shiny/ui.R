
fluidPage(
                                        # Application title
    titlePanel("Pitcher Plant Simulator"),

    sidebarLayout(
                                        # Sidebar with a slider and selection inputs
        sidebarPanel(
            selectInput('plot.choice','Plot Type',choices=c('Pairs','Time-Lag')),
            sliderInput("lagtime",
                        "Lag Time (min):",
                        min = 0,  max = 720, value = 1),            
            hr(),
            sliderInput("days",
                        "Model Run Time (days):",
                        min = 0,  max = 10, value = 1),
            sliderInput("prey",
                        "Prey Input:",
                        min = 0,  max = 500, value = 1),
            sliderInput("amax",
                        "Max Oxygen:",
                        min = 0,  max = 50, value = 10),
            sliderInput("Kw",
                        "Carrying Capacity:",
                        min = 0,  max = 1, value = 0.01),
            sliderInput("preytime",
                        "Prey Addition Time (min):",
                        min = 1,  max = 1440, value = 720)),
                                        # Show plot
        mainPanel(
            plotOutput("plot")
            )
        )
    )


