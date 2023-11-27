# Code for sliding window lab
library(tidyverse)

sliding_diff <- function(df, slide_size, column_id) {
  #' The function to calculates the max min difference from a df column in 
  #' a sliding window manner
  #' 
  #' @param df Object of class data.frame
  #' @param slide_size The sliding window size. Default to 10.
  #' @param column_id The column identifier to perform the operation. This can be column name or index.
  
  df['diff'] <- NA
  # slide_start <- 1
  slide_end <- slide_size
  arr_length <- length(row_number(df))
  for (i in c(1:arr_length)) {
    if (i <= arr_length-slide_size+1) {
      min_slide <- min(df[i:slide_end, column_id])
      max_slide <- max(df[i:slide_end, column_id])
      dif_slide <- max_slide - min_slide
      df[i, 'diff'] <- dif_slide
      slide_end <- i + slide_size
    } 
  }
  return(df)
}

sliding_min <- function(df, slide_size, column_id) {
  #' The function to get the minimum value from the window
  #' 
  #' @param df Object of class data.frame
  #' @param slide_size The sliding window size. Default to 10.
  #' @param column_id The column identifier to perform the operation. This can be column name or index.

  df['min'] <- NA
  slide_end <- slide_size
  arr_length <- length(row_number(df))
  for (i in c(1:arr_length)) {
    if (i <= arr_length-slide_size+1) {
      df[i, 'min'] <- min(df[i:slide_end, column_id])
      slide_end <- i + slide_size
    } 
  }
  return(df)
}

thresholding <- function(df, slide_size=10, threshold, column_id) {
  #' A wrapper function to call sliding_diff and sliding_min and then add the 
  #' difference to the selected column at the position before the current window
  #' if the difference at the current window is higher than the threshold.
  #' 
  #' @param df Object of class data.frame
  #' @param slide_size The sliding window size. Default to 10.
  #' @param threshold The threshold for the differences to be added to the selected column.
  #' @param column_id The column identifier to perform the operation. This can be column name or index
  
  df <- sliding_diff(df, slide_size, column_id)
  df <- sliding_min(df, slide_size, column_id)
  df['rule'] <- NA
  arr_length <- length(row_number(df))
  for (i in c(2:arr_length)) {
    if (i <= arr_length-slide_size+1){
      if (df$diff[i] > threshold) {
        df[i-1,'rule'] <- df[i-1,column_id] + df[i-1,'diff']
      } else {
        df[i-1,'rule'] <- df[i-1,column_id]
      }
    }
  }
  return(df)
}

# implement the functions with the test data
df <- readxl::read_xlsx('./W1/D5/Test_data_frame.xlsx')
df <- thresholding(df, slide_size = 30, threshold=12, column_id = 2)
ggplot(data = df, aes(x = T)) +
  geom_line(aes(y=W)) +
  geom_line(aes(y=rule), color='red') +
  geom_line(aes(y=min), color='blue')




