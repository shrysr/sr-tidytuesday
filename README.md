
# Table of Contents

1.  [Introduction](#org7b042a9)
    1.  [Tools and Notes](#orgee291da)
2.  [Project 2: TV's Golden Age is real.](#org266ffc6)
    1.  [Dataset: link](#org6de9319)
    2.  [Questions](#org6f3ab30)
        1.  [How many unique genres?](#org85a407b)
        2.  [How many unique titles per genre?](#org0b8327b)
        3.  [how many shows released every year, for each genre.](#orgef8dc74)
        4.  [Find the average rating of each title across seasons](#orgafd2824)
        5.  [Average rating variation across genres. Which genres seem more popular?](#org033edce)
        6.  [Average number of seasons and the connection with the average rating](#org5805fba)
        7.  [Top 10, based on number of seasons and average rating, and bringing out the connection with genre.](#org2049b51)
    3.  [Code notebook](#orga678270)
    4.  [Todo Alternate and cleaner method of generating flags](#orgedb6f36)
3.  [Project 1: Federal R&D spending by agency.](#org14d0137)
    1.  [Dataset: link](#orgd3d6751)
    2.  [Notes](#org42f9d7a)
    3.  [Viz posted on Twitter](#org34844b9)
    4.  [Code](#orga35e5f9)



<a id="org7b042a9"></a>

# Introduction

This repo will contain my code explorations with datasets available via the [#TidyTuesday](https://github.com/rfordatascience/tidytuesday) project. The idea is to evolve towards a document based on literate programming principles, with the code side by side with the exploration / analysis.

The latest project is on top.


<a id="orgee291da"></a>

## Tools and Notes

-   I use ESS (and Emacs) for all my workflows. This is a single Org document containing each exploration under a heading, and code blocks enclosed within Org-babel headers.
-   Completed code snippets are tangled into scripts in the [00\_scripts](file:///00_scripts/) folder.


<a id="org266ffc6"></a>

# Project 2: TV's Golden Age is real.


<a id="org6de9319"></a>

## Dataset: [link](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-08)


<a id="org6f3ab30"></a>

## Questions


<a id="org85a407b"></a>

### DONE How many unique genres?


<a id="org0b8327b"></a>

### How many unique titles per genre?


<a id="orgef8dc74"></a>

### how many shows released every year, for each genre.


<a id="orgafd2824"></a>

### Find the average rating of each title across seasons


<a id="org033edce"></a>

### Average rating variation across genres. Which genres seem more popular?


<a id="org5805fba"></a>

### Average number of seasons and the connection with the average rating


<a id="org2049b51"></a>

### Top 10, based on number of seasons and average rating, and bringing out the connection with genre.


<a id="orga678270"></a>

## Code notebook

Loading Libraries and reading in data

                                            # Loading libraries
    library("easypackages")
    libraries("tidyverse", "tidyquant")
                                            # Reading in the data directly from github
    tv_ratings_raw  <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

Let's get a quick overview of the data available.

    tv_ratings_raw %>% skimr::skim()
    tv_ratings_raw %>% glimpse()

Observations:

1.  We have 7 variables, and all of them appear to be useful.
2.  There are no missing values.
3.  Each serial has a unique titleId. This can be useful to separate out serials from seasons.
4.  what does the 'share' column imply?

To get deeper insight on these serials: the genres have to be separated out. This will allow analysis of the serials based on the Genre.

Viewing the data will show that individual genres are separated by a comma. Though I initially started with 6 splits - I found the maximum to be 3 and there are several with less than 3 genres.

So how many unique genres do we have?

                                            # Note:  the unique genres can also obtained by starting with map(unique), and with some further processing.

    ## test <- tv_ratings_raw %>%
    ##   separate(genres,
    ##            into = c("genre1_", "genre2_", "genre3_"),
    ##            sep = ',' ,
    ##            remove = FALSE
    ##            ) %>%
    ##   select(ends_with("_")) %>%
    ##   map(unique)

    all_genres <-  tv_ratings_raw %>%
      separate(genres,
               into = c("genre1_", "genre2_", "genre3_"),
               sep = ',' ,
               remove = FALSE
               ) %>%
      select(ends_with("_")) %>%
      gather(
        key = genres_col,
        value = genre_all,
        .,
        na.rm = TRUE
      ) %>%
      select(genre_all) %>%
      unique() %>%
      spread(
        key = genre_all,
        value = genre_all
      )

There are **22 unique genre categories**. To enable further analysis and make the data more suitable to machine learning, the genres could be combined with the dataset as separate columns with a logical value denoting whether the serial falls into the category or Not. In addition, it would nice to standardise the genre names formatting, especially those like Sci-If and Reality-TV.

With 22 genre columns to consider, a tidyeval function might help.

    names(all_genres) <- str_c(rep("g_", length(all_genres)), names(all_genres))
    tv_ratings_conditioned <- bind_rows(tv_ratings_raw, all_genres)

    all_genres %>% glimpse()

    genre_flag_fn <- function(data, search_col = genres, search_pattern, flag_col )
    {
      flag_col <- enquo(flag_col)
      flag_col_name <- quo_name(flag_col)
      search_col <- enquo(search_col)
      data %>%
        mutate(!!flag_col_name := case_when(
                   str_detect(!!search_col, search_pattern)  ~ 1,
                   TRUE ~ 0
                 )
               )
    }

    tv_ratings_genre_sep_tbl <- tv_ratings_conditioned %>%
      genre_flag_fn(search_pattern = "Action"      , flag_col = g_Action      ) %>%
      genre_flag_fn(search_pattern = "Adventure"   , flag_col = g_Adventure   ) %>%
      genre_flag_fn(search_pattern = "Animation"   , flag_col = g_Animation   ) %>%
      genre_flag_fn(search_pattern = "Biography"   , flag_col = g_Biography   ) %>%
      genre_flag_fn(search_pattern = "Comedy"      , flag_col = g_Comedy      ) %>%
      genre_flag_fn(search_pattern = "Crime"       , flag_col = g_Crime       ) %>%
      genre_flag_fn(search_pattern = "Documentary" , flag_col = g_Documentary ) %>%
      genre_flag_fn(search_pattern = "Drama"       , flag_col = g_Drama       ) %>%
      genre_flag_fn(search_pattern = "Family"      , flag_col = g_Family      ) %>%
      genre_flag_fn(search_pattern = "Fantasy"     , flag_col = g_Fantasy     ) %>%
      genre_flag_fn(search_pattern = "History"     , flag_col = g_History     ) %>%
      genre_flag_fn(search_pattern = "Horror"      , flag_col = g_Horror      ) %>%
      genre_flag_fn(search_pattern = "Music"       , flag_col = g_Music       ) %>%
      genre_flag_fn(search_pattern = "Musical"     , flag_col = g_Musical     ) %>%
      genre_flag_fn(search_pattern = "Mystery"     , flag_col = g_Mystery     ) %>%
      genre_flag_fn(search_pattern = "Reality-TV"  , flag_col = `g_Reality-TV`) %>%
      genre_flag_fn(search_pattern = "Romance"     , flag_col = g_Romance     ) %>%
      genre_flag_fn(search_pattern = "Sci-Fi"      , flag_col = `g_Sci-Fi`    ) %>%
      genre_flag_fn(search_pattern = "Sport"       , flag_col = g_Sport       ) %>%
      genre_flag_fn(search_pattern = "Thriller"    , flag_col = g_Thriller    ) %>%
      genre_flag_fn(search_pattern = "War"         , flag_col = g_War         ) %>%
      genre_flag_fn(search_pattern = "Western"     , flag_col = g_Western     )

Considering releases of serials per year, what was the distribution of genres of the released serials per year? This could include a new season of a serial. A serial will generally run atleast a season or two, unles sit is terribly unpopular. Howevever, what was the overall mood and distribution per genre?

    tv_genre_y <- tv_ratings_genre_sep_tbl %>%
      mutate(dt_year = year(date)) %>%
      select(-c(seasonNumber,
                titleId,
                title,
                av_rating,
                share,
                genres,
                date)) %>%
      group_by(dt_year) %>%
      filter(!duplicated(titleId)) %>%
      summarise_if(.predicate = is.numeric,funs(sum(.))) %>%
      ungroup() %>%
      arrange(desc(dt_year))


<a id="orgedb6f36"></a>

## Todo Alternate and cleaner method of generating flags

    all_genres <-  tv_ratings_raw %>%
      separate(genres,
               into = c("genre1_", "genre2_", "genre3_"),
               sep = ',' ,
               remove = FALSE
               ) %>%
      select(ends_with("_")) %>%
      gather(
        key = genres_col,
        value = genre_all,
        .,
        na.rm = TRUE
      ) %>%
      select(genre_all) %>%
      unique() %>%
      spread(
        key = genre_all,
        value = genre_all
      )




    key_terms <- all_genres %>% select(genre_all)  %>% count(genre_all)

    tv_ratings_raw %>% select(genres)

    tv_ratings_conditioned <- bind_rows(tv_ratings_raw, all_genres)

    tv_ratings_conditioned %>%


    genre_flag_multi_fn <- function(data, search_col = genres, ... , flag_col )
    {
      flag_col <- enquo(flag_col)
      flag_col_name <- quo_name(flag_col)
      search_col <- enquo(search_col)
      search_pattern <- enquos(...)
      ## search_pattern
      ## ## print((!!!search_pattern))
      ## vars(!!!search_pattern[2])

      data %>%
        mutate(expr(str_c(!!!flag_col_name, "_g")) := map(~ case_when(
                                        expr(str_detect(!!search_col, !!!search_pattern))  ~ 1,
                                        TRUE ~ 0
                                      )
                                      )
                     )
    }


    tv_ratings_raw %>%
      select(genres) %>%
      genre_flag_multi_fn(search_pattern, flag_col = search_pattern)


    search_pattern <-
      key_terms %>%
      select(genre_all) %>%
      spread(
        key = genre_all,
        value = genre_all
      ) %>%
      names()




      key_terms %>% select(genre_all)


<a id="org14d0137"></a>

# Project 1: Federal R&D spending by agency.


<a id="orgd3d6751"></a>

## Dataset: [link](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-02-12)


<a id="org42f9d7a"></a>

## Notes

The current code only explores only the portion of Climate Spending.


<a id="org34844b9"></a>

## Viz [posted on Twitter](https://twitter.com/ShreyasRagavan/status/1100765886892265472)


<a id="orga35e5f9"></a>

## Code

                                            # Loading libraries
    library("easypackages")
    libraries("tidyverse", "tidyquant")

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
