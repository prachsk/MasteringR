selection_sort <- function(arr){
  n <- length(arr) # get the length of the array
  for (i in 1:(n-1)) { # loop through the length of array - 1
    min_index <- i # initialize the min_index to the current index
    for (j in (i+1):n) { # loop through the length of array starting from the next index after i (i+1)
      if (arr[j] < arr[min_index]) { # check if the j (i+1) index is smaller than the min_index
        min_index <- j # assign j as a new min_index
      }
    }
    if (i != min_index) { # check if the min_index and i are not the same
      temp <- arr[i] # assign array at index i to temp
      arr[i] <- arr[min_index] # update the array at index i to the array at min_index
      arr[min_index] <- temp # update the array at min_index to temp value
    }
  }
  return(arr)
}

# call the function to sort the array of letters
sorted_array <- selection_sort(sample(letters))