#!/usr/bin/env Rscript

## Import of my libraries.
library(tidyverse)
library(dplyr)

##Function to merge CSV and TSV files together.
joins_write_function <- function(clicks_table, impressions_table, advertiser_table, campaigns_table, new_clicks_table, new_impressions_table) {
  print(clicks_table)
  print(new_clicks_table)
  print("\n")
  print(impressions_table)
  print(new_impressions_table)
  
  ## Use of the "left_join" function allowing us to merge CSV files together. 
  ## First I merge my two CSV files together by their IDs, and then I merge the variable (my two CSV files together) with each of my two TSV files.
  advertiser_plus_campaigns_table <- left_join(advertiser_table, campaigns_table, by=c("ID" = "advertiser_id"))
  clicks_processed <- left_join(new_clicks_table, advertiser_plus_campaigns_table, by=c("campaign_id" = "id"))
  impressions_processed <- left_join(new_impressions_table, advertiser_plus_campaigns_table, by=c("campaign_id" = "id"))
  
  print(advertiser_plus_campaigns_table)
  print(clicks_processed)
  print(impressions_processed)
  
  ## Function allowing us to create our two CSV files.
  write.csv(clicks_processed, "clicks_processed.csv", row.names=FALSE)
  write.csv(impressions_processed, "impressions_processed.csv", row.names=FALSE)
}

clicks_function_algo <- function(clicks_time, clicks_timezone, new_clicks_table, my_index, h, index) {
  ## This "If" allows us to see if my index is equal to "Eastern Time" and if it is the case it will enter in this function. 
  ## If not it will go in the other "If" to see if it is the "Pacific time".
  if (clicks_timezone[my_index] == "Eastern time") {
    if (strtoi(h) > 19) {
      ## Convert my time in integer and if it is inferior to 5 and write the new time in my new table with the function paste0 
      ## and I convert in integer with my strtoi for a calculation 
      ## and then I convert it in string for my CSV and finally I convert my index in vector to be able to write it in the good place in the table.
      new_clicks_table$time[my_index] = paste0('0', substr((5 - (24 - strtoi(h))), 1, 2), substr(index, 3, 8))
      if (strtoi(substr(new_clicks_table$date[my_index], 1,1)) == 0) {
        ## Change my date if it goes over 24 or if it goes under 0.
        temp_date = substr(new_clicks_table$date[my_index], 2, 2)
      }
      ## Write in the new table the new date if there has been a change.
      new_clicks_table$date[my_index] = paste0('0', toString(strtoi(temp_date) + 1), substr(new_clicks_table$date[my_index], 3, 10))
    }
    else {
      new_clicks_table$time[my_index] = paste0(substr(toString(strtoi(h) + 5), 1, 2), substr(index, 3, 8))
    }
    ## This function changes all timezones to "UTC".
    new_clicks_table$timezone[my_index] = "UTC"
  }
  ## Second "if" if my index was not equal to "Eastern time", it will be equal to "Pacific time".
  if (clicks_timezone[my_index] == "Pacific time") {
    if (strtoi(h) > 16) {
      ## Convert my time in integer and if it is inferior to 5 and write the new time in my new table with the function paste0 
      ## and I convert in integer with my strtoi for a calculation 
      ## and then I convert it in string for my CSV and finally I convert my index in vector to be able to write it in the good place in the table.
      new_clicks_table$time[my_index] = paste0('0', substr((8 - (24 - strtoi(h))), 1, 2), substr(index, 3, 8))
      if (strtoi(substr(new_clicks_table$date[my_index], 1, 1)) == 0) {
        ## Change my date if it goes over 24 or if it goes under 0.
        temp_date = substr(new_clicks_table$date[my_index], 2, 2)
      }
      ## Write in the new table the new date if there has been a change.
      new_clicks_table$date[my_index] = paste0('0', toString(strtoi(temp_date) + 1), substr(new_clicks_table$date[my_index], 3, 10))
    }
    else {
      new_clicks_table$time[my_index] = paste0(substr(toString(strtoi(h) + 8), 1, 2), substr(index, 3, 8))
    }
    ## This function changes all timezones to "UTC".
    new_clicks_table$timezone[my_index] = "UTC"
  }
  return (new_clicks_table)
}

## First function with my first algorithm to convert times, dates and timezones from my first TSV file -> Clicks.
clicks_function <- function(clicks_time, clicks_timezone, new_clicks_table, my_index){
  for (index in clicks_time) {
    ## Convert my index in integers and in vector.
    if (strtoi(substr(index, 1, 1)) != 0) {
      ## H variables is my new hours in the algorithm.
      h <- substr(index, 1, 2)
    }
    else {
      h <- substr(index, 2, 2)
    }
    ## The main "If" that my index goes into and anything other than "UTC" in the timezone column, it will go into the next "IF".
    if (clicks_timezone[my_index] != "UTC") {
      new_clicks_table <- clicks_function_algo(clicks_time, clicks_timezone, new_clicks_table, my_index, h, index)
    }
    ## Allow to make an iteration so that it repeats the loop until the last line.
    my_index = my_index + 1
  }
  ## Return my variable that contains the new table with the necessary changes, so that it can be used in the "join" functions.
  return(new_clicks_table)
}

impressions_function_algo <- function(impressions_time, impressions_timezone, new_impressions_table, my_index_two, h, index_two) {
  ## This "If" allows us to see if my index is equal to "Eastern Time" and if it is the case it will enter in this function. 
  ## If not it will go in the other "If" to see if it is the "Pacific time".
  if (impressions_timezone[my_index_two] == "Eastern time") {
    if (strtoi(h) > 19) {
      ## Convert my time in integer and if it is inferior to 5 and write the new time in my new table with the function paste0 
      ## and I convert in integer with my strtoi for a calculation 
      ## and then I convert it in string for my CSV and finally I convert my index in vector to be able to write it in the good place in the table.
      new_impressions_table$time[my_index_two] = paste0('0', substr((5 - (24 - strtoi(h))), 1, 2), substr(index_two, 3, 8))
      if (strtoi(substr(new_impressions_table$date[my_index_two], 1,1)) == 0) {
        ## Change my date if it goes over 24 or if it goes under 0.
        temp_date = substr(new_impressions_table$date[my_index_two], 2, 2)
      }
      ## Write in the new table the new date if there has been a change.
      new_impressions_table$date[my_index_two] = paste0('0', toString(strtoi(temp_date) + 1), substr(new_impressions_table$date[my_index_two], 3, 10))
    }
    else {
      ## A little different from just below, it just looks if the new time is less than 10, then it will just add a 0 in front of the new number to have the right values in the table.
      if (strtoi(h) + 5 < 10) {
        new_impressions_table$time[my_index_two] = paste0('0', substr(toString(strtoi(h) + 5), 1, 2), substr(index_two, 3, 8))
      }
      else {
        ## Will write the new value in my new variable (array) using "paste0" 
        ## (which unlike normal "paste", automatically removes the space that normal paste puts).
        ## And contrary to the Else, we add a "0" in the first argument because our time will be lower than 10,
        ## and it is therefore necessary to add the 0 before my new time.
        new_impressions_table$time[my_index_two] = paste0(substr(toString(strtoi(h) + 5), 1, 2), substr(index_two, 3, 8))
      }
    }
    ## This function changes all timezones to "UTC".
    new_impressions_table$timezone[my_index_two] = "UTC"
  }
  ## Second "if" if my index was not equal to "Eastern time", it will be equal to "Pacific time".
  if (impressions_timezone[my_index_two] == "Pacific time") {
    if (strtoi(h) > 16) {
      ## Convert my time in integer and if it is inferior to 5 and write the new time in my new table with the function paste0 
      ## and I convert in integer with my strtoi for a calculation 
      ## and then I convert it in string for my CSV and finally I convert my index in vector to be able to write it in the good place in the table.
      new_impressions_table$time[my_index_two] = paste0('0', substr((8 - (24 - strtoi(h))), 1, 2), substr(index_two, 3, 8))
      if (strtoi(substr(new_impressions_table$date[my_index_two], 1, 1)) == 0) {
        ## Change my date if it goes over 24 or if it goes under 0.
        temp_date = substr(new_impressions_table$date[my_index_two], 2, 2)
      }
      ## Write in the new table the new date if there has been a change.
      new_impressions_table$date[my_index_two] = paste0('0', toString(strtoi(temp_date) + 1), substr(new_impressions_table$date[my_index_two], 3, 10))
    }
    else {
      ## A little different from just below, it just looks if the new time is less than 10, then it will just add a 0 in front of the new number to have the right values in the table.
      if (strtoi(h) + 8 < 10) {
        new_impressions_table$time[my_index_two] = paste0('0', substr(toString(strtoi(h) + 8), 1, 2), substr(index_two, 3, 8))
      }
      else {
        ## Will write the new value in my new variable (array) using "paste0" 
        ## (which unlike normal "paste", automatically removes the space that normal paste puts).
        ## And contrary to the Else, we add a "0" in the first argument because our time will be lower than 10,
        ## and it is therefore necessary to add the 0 before my new time.
        new_impressions_table$time[my_index_two] = paste0(substr(toString(strtoi(h) + 8), 1, 2), substr(index_two, 3, 8))
      }
    }
    ## This function changes all timezones to "UTC".
    new_impressions_table$timezone[my_index_two] = "UTC"
  }
  return (new_impressions_table)
}

## Second function with my other algorithm to convert times, dates and timezones from my second TSV file -> Impressions.
impressions_function <- function(impressions_time, impressions_timezone, new_impressions_table, my_index_two){
  for (index_two in impressions_time) {
    ## Convert my index in integers and in vector.
    if (strtoi(substr(index_two, 1, 1)) != 0) {
      ## H variables is my new hours in the algorithm.
      h <- substr(index_two, 1, 2)
    }
    else {
      h <- substr(index_two, 2, 2)
    }
    ## The main "If" that my index goes into and anything other than "UTC" in the timezone column, it will go into the next "IF".
    if (impressions_timezone[my_index_two] != "UTC") {
      new_impressions_table <- impressions_function_algo(impressions_time, impressions_timezone, new_impressions_table, my_index_two, h, index_two)
    }
    ## Allow to make an iteration so that it repeats the loop until the last line.
    my_index_two = my_index_two + 1
  }
  ## Return my variable that contains the new table with the necessary changes, so that it can be used in the "join" functions.
  return(new_impressions_table)
}

main <- function() {
  ## Import all the CSV and TSV in 4 different variables. (The suppressWarnings function, just allows us to remove
  ## a small warning when we open a CSV file with the command found in this function.)
  advertiser_table <- suppressWarnings(read.csv2("advertiser.csv", sep = ','))
  campaigns_table <- suppressWarnings(read.csv2("campaigns.csv", sep = ','))
  impressions_table <- read.table(file = 'impressions.tsv', sep = '\t', header = TRUE)
  clicks_table <- read.table(file = 'clicks.tsv', sep = '\t', header = TRUE)
  
  ## Recovery of the headers of each column, in this case "Time" and "Timezone" of the two different TSV files,
  ## that we must modify by putting them in variables that I will use later.
  clicks_time <- clicks_table[['time']]
  clicks_timezone <- clicks_table[['timezone']]
  impressions_time <- impressions_table[['time']]
  impressions_timezone <- impressions_table[['timezone']]
  
  ## Setting up my two TSV files in variables with the word "new" at the beginning, 
  ## which will be my two variables that will collect the data that have been modified.
  new_clicks_table <- clicks_table
  new_impressions_table <- impressions_table
  
  ## Creation of two variables that will be my indexes in my two CSV files.
  my_index = 1
  my_index_two = 1
  
  ## Two functions that collect my two algorithms, allowing to change the time according to the timezones for my two TSV files.
  new_clicks_table <- clicks_function(clicks_time, clicks_timezone, new_clicks_table, my_index)
  new_impressions_table <- impressions_function(impressions_time, impressions_timezone, new_impressions_table, my_index_two)
  
  ## After having recovered the two new tables, we enter this function in order to be able to "join" 
  ## the tables between them in order to create our two CSV files. 
  joins_write_function(clicks_table, impressions_table, advertiser_table, campaigns_table, new_clicks_table, new_impressions_table)
}

## "If" to call the "main" function first.
if(!interactive()) {
  main()
}