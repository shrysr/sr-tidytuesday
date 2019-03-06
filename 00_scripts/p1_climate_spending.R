                                        # Loading libraries
library("easypackages")
libraries("tidyverse", "tidyquant", "gganimate")

                                        # Reading in data directly from github
climate_spend_raw  <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv", col_types = "cin")


                                        # This initial conditioning need not have involved the date manipulation, as the year extracted from a date object is still a double.
climate_spend_conditioned <- climate_spend_raw %>%
  mutate(year_dt = str_glue("{year}-01-01")) %>%
  mutate(year_dt = as.Date(year_dt)) %>%
  mutate(gcc_spending_txt = scales::dollar(gcc_spending,
                                           scale = 1e-09,
                                           suffix = "B"
                                           )
         )

climate_spend_dept_y <- climate_spend_conditioned %>%
  group_by(department, year_dt = year(year_dt)) %>%
  summarise(
    tot_spend_dept_y = sum(gcc_spending)) %>%
  mutate(tot_spend_dept_y_txt = tot_spend_dept_y %>%
           scales::dollar(scale = 1e-09,
                          suffix = "B")
         ) %>%
  ungroup()

glimpse(climate_spend_dept_y)

climate_spend_plt_fn <- function(
                               data,
                               y_range_low = 2000,
                               y_range_hi  = 2010,
                               ncol = 3,
                               caption = ""
                               )
{
  data %>%
    filter(year_dt >= y_range_low & year_dt <= y_range_hi) %>%
    ggplot(aes(y = tot_spend_dept_y_txt, x = department, fill = department ))+
    geom_col() +
    facet_wrap(~ year_dt,
               ncol = 3,
               scales = "free_y") +
    theme_tq() +
    scale_fill_tq(theme = "dark") +
    theme(
      axis.text.x = element_text(angle = 45,
                                 hjust = 1.2),
      legend.position = "none",
      plot.background=element_rect(fill="#f7f7f7"),
    )+
    labs(
      title = str_glue("Federal R&D budget towards Climate Change: {y_range_low}-{y_range_hi}"),
                       x = "Department",
                       y = "Total Budget $ Billion",
                       subtitle = "NASA literally dwarfs all the other departments, getting to spend upwards of 1.1 Billion dollars every year since 2000.",
                       caption = caption
    )

}

climate_spend_plt_fn(climate_spend_dept_y,
                     y_range_low = 2000,
                     y_range_hi = 2017,
                     caption = "#TidyTuesday:\nDataset 2019-02-12\nShreyas Ragavan"
                       )

## The remaining code is partially complete and is in place for further exploration planned in the future.

## Code to download all the data.
## fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
## energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
## climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

## climate_spend_pct_all <- climate_spend_conditioned %>%
##   group_by(year_dt = year(year_dt)) %>%
##   summarise(
##     tot_spend_all_y = sum(gcc_spending)
##   ) %>%
##   mutate(tot_spend_all_y_txt = tot_spend_all_y %>%
##            scales::dollar(scale = 1e-09,
##                           suffix = "B"
##                           )
##          )%>%
##   ungroup() %>%
##   mutate(tot_spend_all_lag = lag(tot_spend_all_y, 1)) %>%
##   tidyr::fill(tot_spend_all_lag ,.direction = "up") %>%
##   mutate(tot_spend_all_pct = (tot_spend_all_y - tot_spend_all_lag)/ tot_spend_all_y,
##          tot_spend_all_pct_txt = scales::percent(tot_spend_all_pct, accuracy = 1e-02)
##          )
