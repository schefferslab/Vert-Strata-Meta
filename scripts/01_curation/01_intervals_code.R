#
#
###########   data digitisation code for 1 m intervals
#
#


library(dplyr)


#data = read.csv("data/stripped_data/original/lentijo_2005.csv")
#data = read.csv("data/stripped_data/original/latta_1998.csv")
#data = read.csv("data/stripped_data/original/walther_2002.csv")
data = read.csv("data/stripped_data/original/Buchanan-Smith_2000.csv")

#<less than
#>more than

data_new <- data %>% 
  mutate(in_bin_0 = ifelse(range_min < 0.1, 1, 0),
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
vert = as.data.frame(colSums(data_new[,5:ncol(data_new)]))
vert

data_newer <- data %>% 
  mutate(in_bin_0 = ifelse(range_min < 0.01, 1, 0),
         in_bin_1 = ifelse(range_min <= 0.1 & range_max > 0.1, 1, 0),
         in_bin_2 = ifelse(range_min <= 0.2 & range_max > 0.1, 1, 0),
         in_bin_3 = ifelse(range_min <= 0.3 & range_max > 0.2, 1, 0),
         in_bin_4 = ifelse(range_min <= 0.4 & range_max > 0.3, 1, 0),
         in_bin_5 = ifelse(range_min <= 0.5 & range_max > 0.4, 1, 0),
         in_bin_6 = ifelse(range_min <= 0.6 & range_max > 0.5, 1, 0),
         in_bin_7 = ifelse(range_min <= 0.7 & range_max > 0.6, 1, 0),
         in_bin_8 = ifelse(range_min <= 0.8 & range_max > 0.7, 1, 0),
         in_bin_9 = ifelse(range_min <= 0.9 & range_max > 0.8, 1, 0),
         in_bin_10 = ifelse(range_min <= 1 & range_max > 0.9, 1, 0))

vert = as.data.frame(colSums(data_newer[,5:ncol(data_newer)]))
vert
