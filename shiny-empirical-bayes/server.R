library(shiny)
library(tidyverse)
library(stringr)
library(scales)

source('helper_scripts/plot_helpers.R')

shinyServer(function(session, input, output) {

    beta_dist_values <- reactiveValues(plot=NULL, dataframe=NULL)

    beta_dist__create_plot__observe_event(session, input, beta_dist_values)

    output$binomial_dist__plot <- binomial_dist__plot__renderPlot(session, input)
    output$beta_dist__plot <- beta_dist__plot__renderPlot(session, input, beta_dist_values)
    output$beta_dist__table <- beta_dist__table__renderTable(input, beta_dist_values)
})
