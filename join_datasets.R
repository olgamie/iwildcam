list_train <- list.files(path = "./data/animal_crops_train/", pattern= "*.jpg", recursive = TRUE) %>% as.data.frame()
names(list_train) <- "file_name"

train_set <- list_train %>%
  left_join(dt %>%
              select(annotations.category_id, images.file_name), by = c("file_name" = "images.file_name"))

valid_set <- train_set %>%
  filter(!is.na(annotations.category_id)) %>% 
  group_by(annotations.category_id) %>%
  sample_frac(0.2) %>%
  mutate(is_valid = TRUE)

train_set <- train_set %>% 
  left_join(valid_set)

train_set <- train_set %>% mutate(is_valid = tidyr::replace_na(is_valid, FALSE))

readr::write_csv(train_set, "train_set.csv")

list_test <- list.files(path = "./data/animal_crops_test/", pattern= "*.jpg", recursive = TRUE) %>% as.data.frame()
names(list_test) <- "file_name"

readr::write_csv(list_test, "test_set.csv")
                                  