binomial_dist__plot__renderPlot <- function(session, input) {

    renderPlot({

        x <- 0:input$binomial_dist__num_observations
        y <- dbinom(x,
                    size=input$binomial_dist__num_observations,
                    prob=input$binomial_dist__probability)

        expected_value <- input$binomial_dist__num_observations * input$binomial_dist__probability
        dataset <- data.frame(x, y)

        if(input$binomial_dist__zoom) {

            threshold <- 0.0001
            dataset <- dataset %>% filter(y > threshold & y < (1 - threshold))

        }

        dataset %>%
            ggplot(aes(x, y)) +
            geom_bar(stat='identity') +
            geom_vline(xintercept=expected_value, linetype='dotted', color='red', size=1) +
            labs(title=paste0("Binomial Distribution (", input$binomial_dist__num_observations, ":", input$binomial_dist__probability, ")"),
                 subtitle=paste("Expected Value:", round(expected_value, 1)), 
                 x="Observations",
                 y="Probability")

    }, height = function() {

        session$clientData$output_binomial_dist__plot_width * 0.66  # set height to % of width
    })
}

beta_dist__create_plot__observe_event <- function(session, input, beta_dist_values) {

    observeEvent(input$beta_dist__run, {

        withProgress(message = 'Making plot', value = 0, {
    
            req(input$beta_dist__prior_successes)
            req(input$beta_dist__prior_trials)
            req(input$beta_dist__additional_successes_a)
            req(input$beta_dist__additional_trials_a)
            req(input$beta_dist__additional_successes_b)
            req(input$beta_dist__additional_trials_b)

            prior_alpha <- input$beta_dist__prior_successes
            prior_beta <- input$beta_dist__prior_trials - input$beta_dist__prior_successes

            new_alpha_a <- prior_alpha + input$beta_dist__additional_successes_a
            new_beta_a <- prior_beta + (input$beta_dist__additional_trials_a - input$beta_dist__additional_successes_a)

            new_alpha_b <- prior_alpha + input$beta_dist__additional_successes_b
            new_beta_b <- prior_beta + (input$beta_dist__additional_trials_b - input$beta_dist__additional_successes_b)


            alpha_vector <- c(new_alpha_a, new_alpha_b, prior_alpha)
            beta_vector <-  c(new_beta_a, new_beta_b, prior_beta)

            if(input$beta_dist__show_prior_distribution) {

                x_min <- min(qbeta(0.001, alpha_vector, beta_vector))
                x_max <- max(qbeta(0.999, alpha_vector, beta_vector))
            
            } else {
            
                x_min <- min(qbeta(0.001, alpha_vector[1:2], beta_vector[1:2]))
                x_max <- max(qbeta(0.999, alpha_vector[1:2], beta_vector[1:2]))
            }


            x_axis_spread <- x_max - x_min

            # depending on the where we want to graph and how spread out the values are, we will want to get more/less granualar with our plot

            distro_names <- c("A", "B", "Prior")
            distros <- data_frame(alpha = alpha_vector,
                                  beta = beta_vector,
                                  group = distro_names) %>%
                group_by(alpha, beta, group) %>%
                do(data_frame(x = seq(x_min, x_max, x_axis_spread / 1000))) %>%
                ungroup() %>%
                mutate(y = dbeta(x, alpha, beta),
                       Parameters = factor(paste0(group, ": alpha= ", alpha, ", beta= ", beta)))



            x_axis_break_steps <- 0.05


            if(x_axis_spread <= 0.02) {

                x_axis_break_steps <- 0.001

            } else if(x_axis_spread <= 0.05) {

                x_axis_break_steps <- 0.005

            } else if(x_axis_spread <= 0.15) {

                x_axis_break_steps <- 0.01

            } else if(x_axis_spread <= 0.5) {

                x_axis_break_steps <- 0.02
            }

            custom_colors <- rev(hue_pal()(3))

            if(!input$beta_dist__show_prior_distribution) {

                distros <- distros %>%
                    filter(!str_detect(Parameters, "Prior"))

                custom_colors <- custom_colors[1:2]
            }

            a_cred_low <- qbeta(0.025, new_alpha_a, new_beta_a)
            a_cred_high <- qbeta(0.975, new_alpha_a, new_beta_a)

            b_cred_low <- qbeta(0.025, new_alpha_b, new_beta_b)
            b_cred_high <- qbeta(0.975, new_alpha_b, new_beta_b)

            simulation_caption <- ""
            if(input$beta_dist__simulate) {

                a_cr_simulation <- rbeta(1e6, new_alpha_a, new_beta_a)
                b_cr_simulation <- rbeta(1e6, new_alpha_b, new_beta_b)
                percent_of_time_b_wins <- mean(b_cr_simulation > a_cr_simulation)

                simulation_caption <- paste0("With the given probability distributions, if we do 1 million simulations,\nB is better than A ",
                                    round(percent_of_time_b_wins * 100, 1), "% of the time")
            }

            max_distros_20th <- max(distros$y) / 20
            beta_dist_values$plot <- ggplot(data=distros, aes(x, y, color = Parameters)) +
                geom_line() +
                geom_area(aes(fill=Parameters, group=Parameters), alpha=0.3, position = 'identity') +
                geom_errorbarh(aes(xmin = a_cred_low, xmax = a_cred_high, y = max_distros_20th * -1), height = max_distros_20th * 0.75, color = custom_colors[1], alpha=0.3) + 
                geom_errorbarh(aes(xmin = b_cred_low, xmax = b_cred_high, y = max_distros_20th * -2), height = max_distros_20th * 0.75, color = custom_colors[2], alpha=0.3) + 
                scale_x_continuous(breaks = seq(0, 1, x_axis_break_steps)) +
                theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
                coord_cartesian(xlim=c(x_min, x_max)) +
                labs(title='Posterior/Updated Probability Distributions of A & B',
                     x="Conversion Rates",
                     y="Density of beta",
                     caption=simulation_caption) +
                scale_fill_manual(values=custom_colors) +
                scale_color_manual(values=custom_colors)



            
            
            beta_dist_values$dataframe <- data.frame(distributions=distro_names,
                                                     `Raw CRs`= c(input$beta_dist__additional_successes_a / input$beta_dist__additional_trials_a,
                                                                  input$beta_dist__additional_successes_b / input$beta_dist__additional_trials_b,
                                                                  input$beta_dist__prior_successes / input$beta_dist__prior_trials),
                                                     `Estimated CRs`= c((input$beta_dist__additional_successes_a + prior_alpha) / (input$beta_dist__additional_trials_a + prior_alpha + prior_beta),
                                                                        (input$beta_dist__additional_successes_b + prior_alpha) / (input$beta_dist__additional_trials_b + prior_alpha + prior_beta),
                                                                        input$beta_dist__prior_successes / input$beta_dist__prior_trials))

        })

    })
}

beta_dist__plot__renderPlot <- function(session, input, beta_dist_values) {

    renderPlot({

        withProgress(message = 'Making plot1', value = 0, {
    

        beta_dist_values$plot     
        })   

    }, height = function() {

        session$clientData$output_beta_dist__plot_width * 0.66  # set height to % of width
    })

}

beta_dist__table__renderTable <- function(input, beta_dist_values) {

    renderTable({

        beta_dist_values$dataframe

    }, digits=4)
}