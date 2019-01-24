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
            'Beta Distribution',
            column(3,
                class='column-input-control-style',
                bsCollapse(id='var_plots__bscollapse', 
                           open=c('Prior Conversions', 'Additional Conversions'), 
                           multiple=TRUE,
                    bsCollapsePanel(
                        'Prior Conversions',
                        numericInput("beta_dist__prior_successes",
                                    "Previous Attemps:",
                                    value = 81),
                        numericInput("beta_dist__prior_trials",
                                    "Previous Trials:",
                                    value = 300),
                        style='default'
                    ),
                    bsCollapsePanel(
                        'Additional Conversions',
                        numericInput("beta_dist__additional_successes",
                                    "New Attemps:",
                                    value = 100),
                        numericInput("beta_dist__additional_trials",
                                    "New Trials:",
                                    value = 300),
                        style='default'
                    )
                )
            ),
            column(9,
                plotOutput(outputId='beta_dist__plot')
                #verbatimTextOutput(outputId='var_plots__ggplot_messages')
            )
        ),
        widths=c(2,10)
    )
))
