# Code for image filter lab

library(magick)
library(tidyverse)

img <- image_read('./W1/D5/Test image.jpg')
img <- img[[1]]
img <- as.integer(img)

# to_grey <- function(color_vec) {
#   color_vec <- 
# }
