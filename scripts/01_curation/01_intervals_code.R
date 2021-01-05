#  Data digitisation code for 1 m intervals

library(dplyr)
library(readr)

# Lentijo 2005 
lentijo_df <- read_csv("data/stripped_data/original/lentijo_2005.csv")



## Richness
lentijo_df_rich <- lentijo_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, 1, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, 1, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, 1, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, 1, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, 1, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, 1, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, 1, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, 1, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, 1, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, 1, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, 1, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, 1, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, 1, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, 1, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, 1, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, 1, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, 1, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, 1, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, 1, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, 1, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, 1, 0),
                in_bin_21 = ifelse(range_min <= 21 & range_max > 20, 1, 0),
                in_bin_22 = ifelse(range_min <= 22 & range_max > 21, 1, 0),
                in_bin_23 = ifelse(range_min <= 23 & range_max > 22, 1, 0),
                in_bin_24 = ifelse(range_min <= 24 & range_max > 23, 1, 0),
                in_bin_25 = ifelse(range_min <= 25 & range_max > 24, 1, 0),
                in_bin_26 = ifelse(range_min <= 26 & range_max > 25, 1, 0),
                in_bin_27 = ifelse(range_min <= 27 & range_max > 26, 1, 0),
                in_bin_28 = ifelse(range_min <= 28 & range_max > 27, 1, 0),
                in_bin_29 = ifelse(range_min <= 29 & range_max > 28, 1, 0),
                in_bin_30 = ifelse(range_min <= 30 & range_max > 29, 1, 0),
                in_bin_31 = ifelse(range_min <= 31 & range_max > 30, 1, 0),
                in_bin_32 = ifelse(range_min <= 32 & range_max > 31, 1, 0),
                in_bin_33 = ifelse(range_min <= 33 & range_max > 32, 1, 0),
                in_bin_34 = ifelse(range_min <= 34 & range_max > 33, 1, 0),
                in_bin_35 = ifelse(range_min <= 35 & range_max > 34, 1, 0))

lentijo_vert_rich = as.data.frame(colSums(lentijo_df_rich[,7:ncol(lentijo_df_rich)]))

## Abundance 
lentijo_df_abund <- lentijo_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(n_per_strata = n / range) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, n_per_strata, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, n_per_strata, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, n_per_strata, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, n_per_strata, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, n_per_strata, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, n_per_strata, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, n_per_strata, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, n_per_strata, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, n_per_strata, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, n_per_strata, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, n_per_strata, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, n_per_strata, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, n_per_strata, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, n_per_strata, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, n_per_strata, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, n_per_strata, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, n_per_strata, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, n_per_strata, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, n_per_strata, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, n_per_strata, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, n_per_strata, 0),
                in_bin_21 = ifelse(range_min <= 21 & range_max > 20, n_per_strata, 0),
                in_bin_22 = ifelse(range_min <= 22 & range_max > 21, n_per_strata, 0),
                in_bin_23 = ifelse(range_min <= 23 & range_max > 22, n_per_strata, 0),
                in_bin_24 = ifelse(range_min <= 24 & range_max > 23, n_per_strata, 0),
                in_bin_25 = ifelse(range_min <= 25 & range_max > 24, n_per_strata, 0),
                in_bin_26 = ifelse(range_min <= 26 & range_max > 25, n_per_strata, 0),
                in_bin_27 = ifelse(range_min <= 27 & range_max > 26, n_per_strata, 0),
                in_bin_28 = ifelse(range_min <= 28 & range_max > 27, n_per_strata, 0),
                in_bin_29 = ifelse(range_min <= 29 & range_max > 28, n_per_strata, 0),
                in_bin_30 = ifelse(range_min <= 30 & range_max > 29, n_per_strata, 0),
                in_bin_31 = ifelse(range_min <= 31 & range_max > 30, n_per_strata, 0),
                in_bin_32 = ifelse(range_min <= 32 & range_max > 31, n_per_strata, 0),
                in_bin_33 = ifelse(range_min <= 33 & range_max > 32, n_per_strata, 0),
                in_bin_34 = ifelse(range_min <= 34 & range_max > 33, n_per_strata, 0),
                in_bin_35 = ifelse(range_min <= 35 & range_max > 34, n_per_strata, 0))

lentijo_vert_abund = as.data.frame(colSums(lentijo_df_abund[,7:ncol(lentijo_df_abund)]))


# Latta 1998
latta_df <- read_csv("data/stripped_data/original/latta_1998.csv")

## Richness
latta_df_rich <- latta_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, 1, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, 1, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, 1, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, 1, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, 1, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, 1, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, 1, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, 1, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, 1, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, 1, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, 1, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, 1, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, 1, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, 1, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, 1, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, 1, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, 1, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, 1, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, 1, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, 1, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, 1, 0))

latta_vert_rich = as.data.frame(colSums(latta_df_rich[,7:ncol(latta_df_rich)]))

## Abundance 
latta_df_abund <- latta_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(n_per_strata = n / range) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, n_per_strata, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, n_per_strata, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, n_per_strata, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, n_per_strata, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, n_per_strata, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, n_per_strata, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, n_per_strata, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, n_per_strata, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, n_per_strata, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, n_per_strata, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, n_per_strata, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, n_per_strata, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, n_per_strata, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, n_per_strata, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, n_per_strata, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, n_per_strata, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, n_per_strata, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, n_per_strata, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, n_per_strata, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, n_per_strata, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, n_per_strata, 0))

latta_vert_abund = as.data.frame(colSums(latta_df_abund[,7:ncol(latta_df_abund)]))

# Walter 2002
walther_df <- read_csv("data/stripped_data/original/walther_2002.csv") %>%
  mutate(range_min = range_min*33,
         range_max = range_max*33, 
         mean_height = mean_height*33)

## Richness
walther_df_rich <- walther_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, 1, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, 1, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, 1, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, 1, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, 1, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, 1, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, 1, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, 1, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, 1, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, 1, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, 1, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, 1, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, 1, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, 1, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, 1, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, 1, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, 1, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, 1, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, 1, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, 1, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, 1, 0),
                in_bin_21 = ifelse(range_min <= 21 & range_max > 20, 1, 0),
                in_bin_22 = ifelse(range_min <= 22 & range_max > 21, 1, 0),
                in_bin_23 = ifelse(range_min <= 23 & range_max > 22, 1, 0),
                in_bin_24 = ifelse(range_min <= 24 & range_max > 23, 1, 0),
                in_bin_25 = ifelse(range_min <= 25 & range_max > 24, 1, 0),
                in_bin_26 = ifelse(range_min <= 26 & range_max > 25, 1, 0),
                in_bin_27 = ifelse(range_min <= 27 & range_max > 26, 1, 0),
                in_bin_28 = ifelse(range_min <= 28 & range_max > 27, 1, 0),
                in_bin_29 = ifelse(range_min <= 29 & range_max > 28, 1, 0),
                in_bin_30 = ifelse(range_min <= 30 & range_max > 29, 1, 0),
                in_bin_31 = ifelse(range_min <= 31 & range_max > 30, 1, 0),
                in_bin_32 = ifelse(range_min <= 32 & range_max > 31, 1, 0),
                in_bin_33 = ifelse(range_min <= 33 & range_max > 32, 1, 0),
                in_bin_34 = ifelse(range_min <= 34 & range_max > 33, 1, 0),
                in_bin_35 = ifelse(range_min <= 35 & range_max > 34, 1, 0))

walther_vert_rich <- as.data.frame(colSums(walther_df_rich[,7:ncol(walther_df_rich)]))

## Abundance 
walther_df_abund <- walther_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(n_per_strata = n / range) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, n_per_strata, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, n_per_strata, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, n_per_strata, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, n_per_strata, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, n_per_strata, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, n_per_strata, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, n_per_strata, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, n_per_strata, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, n_per_strata, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, n_per_strata, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, n_per_strata, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, n_per_strata, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, n_per_strata, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, n_per_strata, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, n_per_strata, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, n_per_strata, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, n_per_strata, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, n_per_strata, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, n_per_strata, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, n_per_strata, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, n_per_strata, 0),
                in_bin_21 = ifelse(range_min <= 21 & range_max > 20, n_per_strata, 0),
                in_bin_22 = ifelse(range_min <= 22 & range_max > 21, n_per_strata, 0),
                in_bin_23 = ifelse(range_min <= 23 & range_max > 22, n_per_strata, 0),
                in_bin_24 = ifelse(range_min <= 24 & range_max > 23, n_per_strata, 0),
                in_bin_25 = ifelse(range_min <= 25 & range_max > 24, n_per_strata, 0),
                in_bin_26 = ifelse(range_min <= 26 & range_max > 25, n_per_strata, 0),
                in_bin_27 = ifelse(range_min <= 27 & range_max > 26, n_per_strata, 0),
                in_bin_28 = ifelse(range_min <= 28 & range_max > 27, n_per_strata, 0),
                in_bin_29 = ifelse(range_min <= 29 & range_max > 28, n_per_strata, 0),
                in_bin_30 = ifelse(range_min <= 30 & range_max > 29, n_per_strata, 0),
                in_bin_31 = ifelse(range_min <= 31 & range_max > 30, n_per_strata, 0),
                in_bin_32 = ifelse(range_min <= 32 & range_max > 31, n_per_strata, 0),
                in_bin_33 = ifelse(range_min <= 33 & range_max > 32, n_per_strata, 0),
                in_bin_34 = ifelse(range_min <= 34 & range_max > 33, n_per_strata, 0),
                in_bin_35 = ifelse(range_min <= 35 & range_max > 34, n_per_strata, 0))

walther_vert_abund <- as.data.frame(colSums(walther_df_abund[,7:ncol(walther_df_abund)]))

# Buchanan 2000
buchanan_df <- read_csv("data/stripped_data/original/Buchanan-Smith_2000.csv")

## Richness
buchanan_df_rich <- buchanan_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, 1, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, 1, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, 1, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, 1, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, 1, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, 1, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, 1, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, 1, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, 1, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, 1, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, 1, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, 1, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, 1, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, 1, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, 1, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, 1, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, 1, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, 1, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, 1, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, 1, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, 1, 0))

buchanan_vert_rich <- as.data.frame(colSums(buchanan_df_rich[,7:ncol(buchanan_df_rich)]))

## Abundance 
buchanan_df_abund <- buchanan_df %>% 
  dplyr::mutate(range_min = round(range_min, 0),
                range_max = round(range_max, 0)) %>%
  dplyr::mutate(range = ifelse(range_max - range_min > 1, range_max - range_min, 1)) %>%
  dplyr::mutate(n_per_strata = n / range) %>%
  dplyr::mutate(in_bin_0 = ifelse(range_min < 0.1, n_per_strata, 0),
                in_bin_1 = ifelse(range_min <= 1 & range_max > 1, n_per_strata, 0),
                in_bin_2 = ifelse(range_min <= 2 & range_max > 1, n_per_strata, 0),
                in_bin_3 = ifelse(range_min <= 3 & range_max > 2, n_per_strata, 0),
                in_bin_4 = ifelse(range_min <= 4 & range_max > 3, n_per_strata, 0),
                in_bin_5 = ifelse(range_min <= 5 & range_max > 4, n_per_strata, 0),
                in_bin_6 = ifelse(range_min <= 6 & range_max > 5, n_per_strata, 0),
                in_bin_7 = ifelse(range_min <= 7 & range_max > 6, n_per_strata, 0),
                in_bin_8 = ifelse(range_min <= 8 & range_max > 7, n_per_strata, 0),
                in_bin_9 = ifelse(range_min <= 9 & range_max > 8, n_per_strata, 0),
                in_bin_10 = ifelse(range_min <= 10 & range_max > 9, n_per_strata, 0),
                in_bin_11 = ifelse(range_min <= 11 & range_max > 10, n_per_strata, 0),
                in_bin_12 = ifelse(range_min <= 12 & range_max > 11, n_per_strata, 0),
                in_bin_13 = ifelse(range_min <= 13 & range_max > 12, n_per_strata, 0),
                in_bin_14 = ifelse(range_min <= 14 & range_max > 13, n_per_strata, 0),
                in_bin_15 = ifelse(range_min <= 15 & range_max > 14, n_per_strata, 0),
                in_bin_16 = ifelse(range_min <= 16 & range_max > 15, n_per_strata, 0),
                in_bin_17 = ifelse(range_min <= 17 & range_max > 16, n_per_strata, 0),
                in_bin_18 = ifelse(range_min <= 18 & range_max > 17, n_per_strata, 0),
                in_bin_19 = ifelse(range_min <= 19 & range_max > 18, n_per_strata, 0),
                in_bin_20 = ifelse(range_min <= 20 & range_max > 19, n_per_strata, 0))

buchanan_vert_abund <- as.data.frame(colSums(buchanan_df_abund[,7:ncol(buchanan_df_abund)]))

     