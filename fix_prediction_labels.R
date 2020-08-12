library(readr)
library(dplyr)
library(purrr)

pred_output <- read_csv("output/croped/preds.csv")
submission <- read_csv("submission_fixed_seq.csv")
test_set <- read_csv("data/test_set.csv")

train_set_categories <- c(0, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 20, 24, 25, 26,
                          32, 44, 50, 62, 67, 70, 71, 72, 73, 74, 77, 78, 80, 83, 86, 89,
                          90, 91, 92, 94, 96, 97, 98, 99, 100, 101, 102, 103, 104, 106, 108,
                          110, 111, 112, 113, 114, 115, 116, 118, 119, 120, 121, 122, 123,
                          124, 127, 129, 130, 133, 134, 137, 139, 141, 142, 144, 145, 147,
                          150, 152, 153, 154, 156, 159, 161, 162, 163, 166, 167, 170, 175,
                          177, 221, 227, 229, 230, 233, 234, 240, 242, 243, 245, 250, 252,
                          256, 257, 258, 259, 262, 265, 267, 268, 273, 286, 291, 292, 294,
                          296, 299, 300, 301, 302, 306, 307, 309, 310, 315, 316, 317, 318,
                          319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 330, 332, 334,
                          335, 336, 337, 338, 339, 340, 341, 342, 344, 345, 346, 347, 349,
                          350, 352, 353, 354, 355, 356, 357, 370, 371, 372, 374, 375, 376,
                          377, 378, 379, 380, 382, 384, 385, 389, 390, 391, 402, 404, 405,
                          406, 407, 409, 410, 413, 414, 415, 416, 417, 418, 419, 422, 454,
                          558, 559, 561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571)

df_map_cat <- tibble(index = 1:205, train_set_categories)
argmax_predictions <- tibble(index = pred_output %>% apply(1, which.max))

true_pred_cat <- argmax_predictions %>% left_join(df_map_cat)

true_pred_cat$file_name <- test_set$file_name

submission_with_predictions <- submission %>% mutate(Category = NULL) %>%
  left_join(true_pred_cat %>%
              mutate(file_name = gsub(".jpg", "", file_name)),
            by = c("Id" = "file_name")) %>%
  select(Id, Category = train_set_categories)

write_csv(submission_with_predictions, "output/submission_croped_fixed_labels.csv")
