library(dplyr)
library(readr)
library(purrr)
library(jsonlite)

prob_croped <- read_csv("output/croped/preds.csv")
prob_full <- read_csv("output/full_128/preds.csv")
json_file <- fromJSON("data/iwildcam2020_test_information.json")

prob_croped <- prob_croped %>% mutate(file_names = stringr::str_split(prob_croped$Id, "/")  %>% map_chr(., 5),
                        ID = gsub(".jpg", "", file_names),
                        Id = NULL,
                        file_names = NULL)

prob_full <- prob_full %>% mutate(file_names = stringr::str_split(prob_full$Id, "/")  %>% map_chr(., 7),
                                  ID = gsub(".jpg", "", file_names),
                                  Id = NULL,
                                  file_names = NULL)

cats_cropped_img <- c(0, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 20, 24, 25, 26, 32, 
                           44, 50, 62, 67, 70, 71, 72, 73, 74, 77, 78, 80, 83, 86, 89, 90, 91, 
                           92, 94, 96, 97, 98, 99, 100, 101, 102, 103, 104, 106, 108, 110, 111, 
                           112, 113, 114, 115, 116, 118, 119, 120, 121, 122, 123, 124, 127, 129, 
                           130, 133, 134, 137, 139, 141, 142, 144, 145, 147, 150, 152, 153, 154, 156, 
                           159, 161, 162, 163, 166, 167, 170, 175, 177, 221, 227, 229, 230, 233, 234, 
                           235, 240, 242, 243, 245, 250, 252, 256, 257, 258, 259, 262, 265, 267, 268, 
                           273, 286, 291, 292, 294, 296, 299, 300, 301, 302, 306, 307, 309, 310, 315, 
                           316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 330, 332, 
                           333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 344, 345, 346, 347, 349, 
                           350, 352, 353, 354, 355, 356, 357, 370, 371, 372, 374, 375, 376, 377, 378, 
                           379, 380, 382, 384, 385, 389, 390, 391, 402, 404, 405, 406, 407, 408, 409, 
                           410, 412, 413, 414, 415, 416, 417, 418, 419, 422, 454, 558, 559, 561, 562, 
                           563, 564, 565, 566, 567, 568, 569, 570, 571)

cats_full_img <- c(0, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 20, 24, 25, 26, 32,
               44, 50, 62, 67, 70, 71, 72, 73, 74, 77, 78, 79, 80, 83, 86, 89, 90,
               91, 92, 94, 96, 97, 98, 99, 100, 101, 102, 103, 104, 106, 108, 110,
               111, 112, 113, 114, 115, 116, 118, 119, 120, 121, 122, 123, 124, 127,
               129, 130, 133, 134, 137, 139, 141, 142, 144, 145, 147, 150, 152, 153,
               154, 156, 159, 161, 162, 163, 166, 167, 170, 175, 177, 198, 221, 227,
               229, 230, 233, 234, 235, 240, 242, 243, 245, 250, 251, 252, 253, 256,
               257, 258, 259, 262, 265, 267, 268, 273, 286, 290, 291, 292, 294, 296,
               299, 300, 301, 302, 306, 307, 309, 310, 315, 316, 317, 318, 319, 320,
               321, 322, 323, 324, 325, 326, 327, 328, 330, 332, 333, 334, 335, 336,
               337, 338, 339, 340, 341, 342, 344, 345, 346, 347, 348, 349, 350, 352,
               353, 354, 355, 356, 357, 370, 371, 372, 374, 375, 376, 377, 378, 379,
               380, 382, 384, 385, 389, 390, 391, 402, 404, 405, 406, 407, 408, 409,
               410, 412, 413, 414, 415, 416, 417, 418, 419, 420, 422, 454, 558, 559,
               561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571)

colnames(prob_croped) <- c(cats_cropped_img, "ID")

cats_full_img[!cats_full_img %in% cats_cropped_img]

prob_croped <- prob_croped %>% mutate(
  "79" = 0,
  "198" = 0,
  "251" = 0,
  "253" = 0,
  "290" = 0,
  "348" = 0,
  "420" = 0
)

prob_croped <- prob_croped %>%
  relocate(as.character(cats_full_img))

colnames(prob_full) <- c(cats_full_img, "ID")

names(prob_croped) == names(prob_full)

croped_ext <- tibble(ID = prob_full$ID) %>% left_join(prob_croped)

croped_ext[is.na(croped_ext)] <- 0

new_probs <- croped_ext[,2:ncol(croped_ext)] * 0.7 + prob_full[,1:(ncol(croped_ext) - 1)] * 0.3

argmax_predictions <- tibble(index = new_probs %>% apply(1, which.max))

df_map_cat <- tibble(index = 1:216, cats_full_img)

true_pred_cat <- argmax_predictions %>% left_join(df_map_cat)

predictions_with_ids <- tibble(Id = prob_full$ID, Category = true_pred_cat$cats_full_img)

submission_org <- read_csv("output/submission_fixed_seq.csv")

submission <- submission_org %>% 
  mutate(Category = NULL) %>%
  left_join(predictions_with_ids)

test_images <- json_file["images"] %>% as.data.frame() %>%
  mutate(datetime = as_datetime(images.datetime)) %>%
  group_by(images.location) %>%
  arrange(datetime) %>%
  mutate(lag_datetime = dplyr::lag(datetime, n = 1, default = NA),
         diff_time = difftime(datetime, lag_datetime, units = "secs"))

test_images$new_seq_id <- 0
for (i in 2:nrow(test_images)){
  current_seq_id <-  test_images$new_seq_id[i-1] 
  test_images$new_seq_id[i] <- ifelse(
    test_images$images.location[i-1] != test_images$images.location[i] | test_images$diff_time[i] > 20, 
    current_seq_id + 1, current_seq_id)
}

submission_with_seq_id <- submission %>%
  left_join(test_images %>% ungroup() %>% select(images.id, new_seq_id), 
            by = c("Id" = "images.id"))

most_common_category_with_empty <- submission_with_seq_id %>%
  group_by(new_seq_id) %>% 
  count(new_seq_id, Category) %>%
  slice(which.max(n))

most_common_category_without_empty <- submission_with_seq_id %>%
  filter(Category != 0) %>% 
  group_by(new_seq_id) %>% 
  count(new_seq_id, Category) %>%
  slice(which.max(n))

seq_id_predicted_category <- most_common_category_with_empty %>% 
  left_join(most_common_category_without_empty, by = "new_seq_id") %>%
  select(new_seq_id, Category.x, Category.y) %>%
  mutate(final_category = ifelse(is.na(Category.y), Category.x, Category.y)) %>%
  select(new_seq_id, final_category)

final_pred_with_ids <- submission_with_seq_id %>% 
  left_join(seq_id_predicted_category)

submission_corrected_categories <- final_pred_with_ids %>% 
  mutate(Category = NULL,
         new_seq_id = NULL) %>%
  rename(Category = final_category)

write_csv(submission_corrected_categories, "output/submission_fixed_seq.csv")                                                 

