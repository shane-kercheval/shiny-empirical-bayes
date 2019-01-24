library(shiny)

shinyServer(function(session, input, output) {
   
  output$beta_dist__plot <- renderPlot({

        req(input$beta_dist__prior_successes)
        req(input$beta_dist__prior_trials)
        req(input$beta_dist__additional_successes)
        req(input$beta_dist__additional_trials)
      
        prior_alpha <- input$beta_dist__prior_successes
        prior_beta <- input$beta_dist__prior_trials - input$beta_dist__prior_successes

        new_alpha <- prior_alpha + input$beta_dist__additional_successes
        new_beta <- prior_beta + (input$beta_dist__additional_trials - input$beta_dist__additional_successes)

        distros <- data_frame(a = c(prior_alpha, new_alpha),
                   b = c(prior_beta, new_beta)) %>%
          group_by(a, b) %>%
          do(data_frame(x = seq(0, 1, .001), y = dbeta(x, .$a, .$b))) %>%
          mutate(Parameters = paste0("\u03B1 = ", a, ", \u03B2 = ", b)) %>%
          ungroup() %>%
          mutate(Parameters = factor(Parameters, levels = unique(Parameters)))


        min_x <- min(c(qbeta(0.01, prior_alpha, prior_beta), qbeta(0.01, new_alpha, new_beta)))
        max_x <- max(c(qbeta(0.99, prior_alpha, prior_beta), qbeta(0.99, new_alpha, new_beta)))

        prior_expected_value <- prior_alpha / (prior_alpha + prior_beta)
        new_conversion_rate <- input$beta_dist__additional_successes / input$beta_dist__additional_trials
        new_expected_value <- new_alpha / (new_alpha + new_beta)

        ggplot(data=distros, aes(x, y, color = Parameters)) +
            geom_line() +
            geom_vline(xintercept = prior_expected_value, linetype='dotted', color='red', size=1) +
            geom_vline(xintercept = new_expected_value, linetype='dotted', color='#00BFC4', size=1) +
            geom_vline(xintercept = new_conversion_rate, linetype='dotted', color='black', size=1) +
            scale_x_continuous(breaks = seq(0, 1, 0.01)) +
            coord_cartesian(xlim=c(min_x, max_x)) +
            labs(title='Prior (red) and updated (blue) probability distributions',
                 x="Conversion Rates",
                 y="Density of beta",
                 caption = paste("\nPrior Expected Value (red):", round(prior_expected_value, 3),
                                 "\nNew Expected Value (teal):", round(new_expected_value, 3),
                                 "\nNew Conversion Rate (black):", round(new_conversion_rate, 3)))
    }, height = function() {

        session$clientData$output_beta_dist__plot_width * 0.66  # set height to % of width
    })
})
