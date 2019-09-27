
# SMS - Visualize Messages ------------------------------------------------

fx_sms_visualize <- function(data, plot_type, contact = NULL) {

  source('r/fx_sms_summarise.R')
  source('r/fx_sms_filter.R')
  source('r/fx_sms_plot_themes.R')

  library(dplyr)
  library(colorspace)
  library(ggplot2)
  library(plotly)


  if (plot_type == "scatter_all" && is_null(contact)) {
    # All - Scatterplot -------------------------------------------------------
    # Scatter plot of day by message length, with number of messages and contacts
    .temp_plot <-
      data %>%
      fx_sms_summarise("time_periods") %>%
      group_by(day) %>%
      fx_sms_summarise("group_summary") %>%

      ggplot() +
      aes(x = day, y = length_sum, size = message_n, color = contact_n) +
      geom_point(alpha = 0.75) +
      scale_color_viridis_c() +
      labs(y = NULL, x = NULL, color = NULL, size = NULL)

    .temp_plot %>% ggplotly()


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

  } else if (plot_type == "send_rec") {

    .temp_plot <-
      data %>%
      ggplot() +
      aes(x = Contact) +
      geom_linerange(aes(ymin = Q1, ymax = Q3, color = `Longer Messages`)) +
      geom_point(aes(y = Median,
                     size = `Days of Contact`,
                     fill = `Longer Messages`),
                 color = "black",
                 shape = 21) +
      geom_hline(aes(yintercept = 0)) +
      labs(x = NULL, color = NULL, y = "Median Difference") +
      guides(color = FALSE, fill = FALSE, size = FALSE) +
      fx_sms_plot_theme()$light_ggtheme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor = element_line(linetype = 3),
            axis.text.x = element_text(angle = -35))

    .temp_plot %>% ggplotly()

  } else if (plot_type == "period_adjustment") {

    data %>%
      ggplot() +
      aes(x = Contact) +
      geom_hline(yintercept = 1, color = "grey80") +
      geom_linerange(aes(ymin = 1, ymax = value, color = flag), size = 1.5) +
      geom_point(aes(y = value, fill = flag), size = 3.5, shape = 21, color = "grey40") +
      facet_wrap(vars(label), labeller = as_labeller(c("adj_freq" = "Change in\nMessage Frequency",
                                                       "adj_length" = "Change in\nMessage Length"))) +
      coord_flip() +
      labs(y = NULL, x = NULL) +
      guides(fill = "none", color = guide_legend(title = NULL)) +
      scale_color_manual(values = c("#f8766d" %>% lighten(0.40),
                                    "#00ba38" %>% lighten(0.40),
                                    "#619cff" %>% lighten(0.40)),
                         breaks = c("pos", "new", "neg"),
                         labels = c("Increase", "New Contact", "Decrease")) +
      scale_fill_manual(values = c("#f8766d", "#00ba38", "#619cff")) +
      scale_y_continuous(limits = c(0.25, NA), labels = unit_format(unit = "x")) +
      fx_sms_plot_theme()$light_theme
  }

}


# Test Function -----------------------------------------------------------
# data <- data_master
# data %>% fx_sms_visualize("scatter_all")
#
# data <- data_top
# output <- data %>% fx_sms_visualize("bar_20")
# output$daily_messages
