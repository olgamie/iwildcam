library("jsonlite")
library(dplyr)
json_file <- fromJSON("iwildcam2020_train_annotations.json")

a <- json_file["annotations"] %>% as.data.frame()
img <- json_file["images"] %>% as.data.frame()

cat <- json_file["categories"] %>% as.data.frame()

dt <- a %>% left_join(img, by = c("annotations.image_id" = "images.id")) %>%
  left_join(cat, by = c("annotations.category_id" = "categories.id")) %>%
  filter(!categories.name %in% c("end", "star", "unidentifiable", "unknown"))

names(dt)

## count categories and locations
count_cat_loc <- dt %>%
  count(annotations.category_id, images.location)

## count locations per category
categories_in_min_2_loc <- count_cat_loc %>%
  select(annotations.category_id, images.location) %>%
  count(annotations.category_id) %>%
  arrange(-n)

## find categories that are present in 1 location only
category_1_location <- categories_in_min_2_loc %>% filter(n == 1)

## find locations with categories appearing once
location_1_cat <- count_cat_loc %>%
  filter(annotations.category_id %in% category_1_location$annotations.category_id) %>% 
  pull(images.location)

## filter out single locations from main dataset
dt_cats_2_location_or_more <- dt %>%
  filter(!images.location %in% location_1_cat) %>%
  distinct(annotations.category_id)

sample_cat <- sample_n(dt_cats_2_location_or_more, 30)

random_locations <- count_cat_loc %>%
  filter(annotations.category_id %in% sample_cat$annotations.category_id) %>%
  group_by(annotations.category_id) %>%
  sample_frac(0.1) %>%
  distinct()
  
random_unique_loc <- random_locations$images.location %>% unique

dt %>% filter(images.location %in% random_unique_loc) %>% nrow()

dt %>% filter(images.location %in% random_unique_loc) %>% distinct(annotations.category_id) -> valid_cat

dt %>% filter(!images.location %in% random_unique_loc) %>% distinct(annotations.category_id) -> train_cat

dt %>% distinct(annotations.category_id) -> all_cat

all_cat %>% filter(!annotations.category_id %in% train_cat$annotations.category_id)

dt %>% filter(images.location %in% random_unique_loc) %>% count(annotations.category_id) %>%
  ggplot2::ggplot(ggplot2::aes(annotations.category_id, n)
  ) + ggplot2::geom_col()

dt %>% count(annotations.category_id) %>%
  ggplot2::ggplot(ggplot2::aes(annotations.category_id, n)
  ) + ggplot2::geom_col()
