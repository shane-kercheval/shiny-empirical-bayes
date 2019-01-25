library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

#source('helper_scripts/definitions.R')

shinyUI(fluidPage(theme="custom.css",
                  
    useShinyjs(),
    
    titlePanel("Empirical Bayes: A Shiny App Following David Robinson's book."),
    navlistPanel(
        tabPanel(
            'Binomial Distribution',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='binomial_dist__bscollapse', 
                           open=c('Parameters'),
                           multiple=TRUE,
                    bsCollapsePanel(
                        'Parameters',
                        numericInput(inputId='binomial_dist__num_observations',
                                    label="Number of Observations:",
                                    value = 20),
                        sliderInput(inputId='binomial_dist__probability', 
                                    label="Probability",
                                    min=0,
                                    max=1,
                                    value=0.3, 
                                    step = 0.01),
                        checkboxInput(inputId='binomial_dist__zoom',
                                      label="Zoom", value = FALSE),
                        style='default'
                    )
                )
            ),
            column(9,
                plotOutput(outputId='binomial_dist__plot')
                #verbatimTextOutput(outputId='binomial_dist__ggplot_messages')
            )
        ),
        tabPanel(
            'Beta Distribution',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='beta_dist__bscollapse', 
                           open=c('Prior Conversions', 'Additional Conversions'), 
                           multiple=TRUE,
                    bsCollapsePanel(
                        'Prior Conversions',
                        numericInput(inputId='beta_dist__prior_successes',
                                    label="Previous Attemps:",
                                    value = 81),
                        numericInput(inputId='beta_dist__prior_trials',
                                    label="Previous Trials:",
                                    value = 300),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Additional Conversions',
                        numericInput(inputId='beta_dist__additional_successes',
                                    label="New Attemps:",
                                    value = 100),
                        numericInput(inputId='beta_dist__additional_trials',
                                    label="New Trials:",
                                    value = 300),
                        style='default'
                    )
                )
            ),
            column(9,
                plotOutput(outputId='beta_dist__plot')
                #verbatimTextOutput(outputId='beta_dist__ggplot_messages')
            )
        ),
        widths=c(2,10)
    )
))
