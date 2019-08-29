
# SMS - Visualize Messages ------------------------------------------------

fx_sms_visualize <- function(data, plot_type, contact = NULL) {

  source('C:/Users/kputs/OneDrive/Data/kp_messages/r/fx_sms_summarise.R')
  source('C:/Users/kputs/OneDrive/Data/kp_messages/r/fx_sms_filter.R')

  library(dplyr)
  library(ggplot2)
  library(plotly)

  if (plot_type == "scatter_all" && is_null(contact)) {
    # All - Scatterplot -------------------------------------------------------
    # Scatter plot of day by message length, with number of messages and contacts
    plot_generic <-
      data %>%
      mutate(day = date(DateTime)) %>%
      group_by(day) %>%
      fx_sms_summarise("time_period") %>%

      ggplot() +
      aes(x = day, y = length_sum, size = message_n, color = contact_n) +
      geom_point(alpha = 0.75) +
      scale_color_viridis_c() +
      labs(y = NULL, x = NULL, color = NULL, size = NULL)

    plot_generic %>% ggplotly()


  } else if (plot_type == "generic" && !is_null(contact)) {
    # Contact Specific - Overall Plot -----------------------------------------


  } else if (plot_type == "bar_rank") {
    # Top N - Bar Comparison --------------------------------------------------
    .temp_data <-
      data %>%
      mutate_at("Contact", as_factor) %>%
      mutate(display = str_glue("Message Count: {comma(message_n)}
                                Length Sum:{comma(length_sum)}
                                Average Length: {comma(length_avg)}
                                Days of Contact: {comma(contact_days)}
                                Messages per Day: {comma(daily_messages)}
                                Length per Day: {comma(daily_length)}"))

    .temp_fx <- function(y) {
      .temp_data %>%
        plot_ly(x = ~Contact, y = ~get(y), text = ~display,
                marker = list(line = list(color = "black", width = 1.5)),
                type = "bar") %>%
        layout(title = NULL, xaxis = list(title = ""), yaxis = list(title = ""))
    }

    .temp_cols <-
      c("message_n",
        "length_sum",
        "length_avg",
        "contact_days",
        "daily_messages",
        "daily_length")

     map(.temp_cols, .temp_fx) %>%
       set_names(.temp_cols)

  }

}


# Test Function -----------------------------------------------------------
# data <- data_master
# data %>% fx_sms_visualize("scatter_all")
#
# data <- data_top
# output <- data %>% fx_sms_visualize("bar_20")
# output$daily_messages
