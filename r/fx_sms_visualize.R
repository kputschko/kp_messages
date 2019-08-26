
# SMS - Visualize Messages ------------------------------------------------

fx_sms_visualize <- function(data, contact = NULL, plot_type = "generic", date_filter = NULL) {

  source('C:/Users/kputs/OneDrive/Data/kp_messages/r/fx_sms_summarise.R')

  library(dplyr)
  library(ggplot2)
  library(plotly)

  # Apply Date Filter -------------------------------------------------------

  data_filter <-
    if (is_null(date_filter)) {
      data
    } else {
      data %>% filter(DateTime < max(date_filter), DateTime > min(date_filter))
    }


  if (plot_type == "generic" && is_null(contact)) {
    # Generic Overall Plot ----------------------------------------------------
    # Scatter plot of day by message length, with number of messages and contacts

    plot_generic <-
      data_filter %>%
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


  } else if (plot_type == "whatever") {

  }

}


# Test Function -----------------------------------------------------------

# data_updated$new %>% fx_sms_visualize()
# data_updated$master %>% fx_sms_visualize()
