library(shiny)

shinyServer(function(input, output, session) {
  output$recs_cos <- renderDataTable(
    if(! is.null(input$beers)){
      if(input$beers[1]!=''){
        beers_to_recs(
          x = input$beers,
          names = beer_vec,
          method = 'cos',
          sim_cutoff = input$cutoff,
          N = 125
        )
      }
    }
  )
  output$recs_sum <- renderDataTable(
    if(! is.null(input$beers)){
      if(input$beers[1]!=''){
        beers_to_recs(
          x = input$beers,
          names = beer_vec,
          method = 'sum',
          sim_cutoff = input$cutoff,
          N = 125
        )
      }
    }
  )
  output$recs_pca <- renderDataTable(
    if(! is.null(input$beers)){
      if(input$beers[1]!=''){
        beers_to_recs(
          x = input$beers,
          names = beer_vec,
          method = 'pca',
          sim_cutoff = input$cutoff,
          N = 125
        )
      }
    }
  )
})
