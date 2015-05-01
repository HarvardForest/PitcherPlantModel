
function(input, output, session) {
                                        # Define a reactive expression for the document term matrix
    terms <- reactive({
        simPitcher(days=input$days,
                   w0=input$prey,
                   a.max=input$amax,
                   prey.t=input$preytime,
                   Kw=input$Kw,x0=0,a0=0,m=1,s=10,a.min=0,d=0.5)
    })
    k <- reactive({as.numeric(input$lagtime)})

                                        # Make drawing predictable during a session
    output$plot <- renderPlot({
        if (input$plot.choice == 'Time-Lag'){
            plot(terms()$x[(k()+1):length(terms()$x)]~terms()$x[1:(length(terms()$x)-k())],
                 xlab='x(t)',ylab='x(t + k)',type='l')
        }else{
            pairs(terms())
        }
    })
}
