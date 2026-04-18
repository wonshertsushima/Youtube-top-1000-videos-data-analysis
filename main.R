library(tidymodels)
library(tidyverse)
library(kknn)
library(broom)



videos <- read_csv("most_viewed_videos_1000.csv",
                   col_types = cols(
                     "rank" = col_number(),
                     "title"  =  col_character(),
                     "title_length" = col_number(),
                     "detected_language" = col_character(),
                     "content_type" = col_factor(),
                     "is_short" = col_logical(),
                     "has_hashtags" = col_logical(),
                     "views" = col_character(),
                     "likes"  = col_character()
                   ))


videos_clean <- videos %>%
  mutate(
    views = case_when(
      str_detect(views, "B") ~ as.numeric(str_remove(views, "B")) * 1e9,
      str_detect(views, "M") ~ as.numeric(str_remove(views, "M")) * 1e6,
      str_detect(views, "K") ~ as.numeric(str_remove(views, "K")) * 1e3,
      TRUE ~ as.numeric(views)
    ),
    likes = case_when(
      str_detect(likes, "B") ~ as.numeric(str_remove(likes, "B")) * 1e9,
      str_detect(likes, "M") ~ as.numeric(str_remove(likes, "M")) * 1e6,
      str_detect(likes, "K") ~ as.numeric(str_remove(likes, "K")) * 1e3,
      TRUE ~ as.numeric(likes)
    )
  )


videos_analysis <- videos_clean %>%
  mutate(
    like_ratio = (likes / views) * 100,

    is_viral = as.factor(if_else(like_ratio >= 0.25, "TRUE", "FALSE"))
  )


random_videos <- tibble(
  content_type = c("Kids/Educational", "Vlog", "Comedy/Entertainment", "Comedy/Entertainment", "Music Video", "Music Video"),
  is_short = c("TRUE", "FALSE", "TRUE", "FALSE", "TRUE", "FALSE")
) %>%
  mutate(

    content_type = as.factor(content_type),
    is_short = as.factor(is_short)
  )




