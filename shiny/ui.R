
fluidPage(
                                        # Application title
    titlePanel("Pitcher Plant Simulator"),

    sidebarLayout(
                                        # Sidebar with a slider and selection inputs
        sidebarPanel(
            sliderInput("days",
                        "Model Run Time (days):",
                        min = 0,  max = 100, value = 10),
            sliderInput("prey",
                        "Prey Input:",
                        min = 0,  max = 100, value = 1),
            sliderInput("preytime",
                        "Prey Addition Time (min):",
                        min = 1,  max = 1440, value = 720),
            sliderInput("beta",
                        "Decomp Exponent:",
                        min = 0,  max = 0.001, value = 0.0001),
            sliderInput("lag",
                        "Phase Time Lag:",
                        min = 1,  max = 1339, value = 2),
            selectInput("bodrescale","Rescale BOD",choices=c("FALSE","TRUE")),
            sliderInput("bodscalar",
                        "BOD Scalar:",
                        min = 1,  max = 100, value = 1)),
                                        # Show plot
        mainPanel(
            plotOutput("plot")
            )
        )
    )


