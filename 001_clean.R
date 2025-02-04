library(tidyverse)

#################Loading data into the environment#################

clean <- function(df){
  
  # Error if one y is missing
  stopifnot(all(!is.na(df$y)))
  
  # Warning if other variables contain NA’s
  if(!is_empty(df[!complete.cases(df),])){
    warning("There are lines having non-values", call. = FALSE)
  }
  
  # change columnames to lower_case letters
  colnames(df) <- colnames(df) %>% tolower()
  
  # remove first column if it is a sequence of ascending numbers
  if (all(df[,1] == sequence(length(df[,1]))))  df <- df[,-1]
  
  # Remove any columns (and report as warning) which contain more than 50% NA’s
  col_na_sum <- function(col){ sum(is.na(col))}
  percentage_of_NA <- apply(df, MARGIN=2, FUN=col_na_sum)/ nrow(df)
  temp <- df %>% select(names(percentage_of_NA[percentage_of_NA<0.5]))
  
}

