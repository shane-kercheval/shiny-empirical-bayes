library(shiny)
library(tidyverse)

source('helper_scripts/plot_helpers.R')

shinyServer(function(session, input, output) {

    output$binomial_dist__plot <- binomial_dist__plot__renderPlot(session, input)
    output$beta_dist__plot <- beta_dist__plot__renderPlot(session, input)
})
