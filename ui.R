library(shiny)

load('beer.RData')

shinyUI(
  fluidPage(
    br(),
    fluidRow(
      selectizeInput(
        'beers',
        'Select some beers you like',
        choices = beer_vec_trunc,
        width = '500%',
        multiple = TRUE
      )
    ),
    fluidRow(
      sliderInput(
        "cutoff", "Select user similarity cutoff:", min = 0, max = 1, value = 0.05, step = 0.05
        )
    ),
    fluidRow(
      column(
        6,
        hr(),
        h4("Cosine similarity"),
        dataTableOutput('recs_cos')
      ),
      column(
        6,
        hr(),
        h4("Additive similarity"),
        dataTableOutput('recs_sum')
      ),
      column(
        6,
        hr(),
        h4("PCA similarity"),
        dataTableOutput('recs_pca')
      )
    )
  )
)
