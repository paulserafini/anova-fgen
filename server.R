shinyServer(function(input, output, session) {
  
  output$factortypeInput <- renderUI({ 
    alphabet <- c('A','B','C','D')
    if (input$num_factors %in% 1:8) {
      html_ui <- " "
      for (i in 1:input$num_factors){
        html_ui <- paste0(html_ui, radioButtons(paste0("factortype",i), paste0("Factor ",alphabet[[i]],":"), c("Random" = 0, "Fixed" = 1)))
      }
      HTML(html_ui)
    }
  })
  
  output$foutput <- eventReactive(input$runButton, {
    calc <- dget("ems-gen.R")

    alphabet <- c('A','B','C','D')

    factor_type <- c()
    factors <- c()
    for (i in 1:input$num_factors) {
      factor_type[[i]] <- as.integer(eval(parse(text = paste0('input$factortype',i))))
      factors[[i]] <- alphabet[[i]]
    }
    capture.output(calc(factors, factor_type))
  })

})
