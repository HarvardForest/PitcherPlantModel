
function(input, output, session) {
                                        # Define a reactive expression for the document term matrix
    terms <- reactive({
        simO2(
            days=input$days,
            prey.add=input$prey,
            t.add=input$preytime,
            beta=input$beta,
            bod.rescale=input$bodrescale,
            bod.scalar=input$bodscalar
            )
    })
    ##time lagging
    k <- reactive({as.numeric(input$lag)})

                                        # Make drawing predictable during a session
    output$plot <- renderPlot({
        par(mfrow=c(2,2))
        plot.ts(terms()[,4],ylab='Prey')
        plot.ts(terms()[,5],ylab='BOD')
        plot.ts(terms()[,2],ylab=expression('O'[2]),main='')
        plot(terms()[,2],
        c(terms()[k():length(terms()[,2]),2],terms()[1:(k()-1),2]),
             xlab='t',
             ylab='t+k',type='l',main=expression('O'[2]))
    },width=700,height=750)
}
