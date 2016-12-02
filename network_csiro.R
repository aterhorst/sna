library(jsonlite)
library(tidyjson)
library(data.table)
library(dplyr)
library(plyr)
library(purrr)
library(httr)

raw_dat <- fromJSON("http://network.csiro.au:9500/standalone/publications.json?_page=1&_pageSize=5")
pubs <- as.data.frame(subset(raw_dat$result$items, select = c("_about", "yearOfPublication", "author")))

library(jsonlite)
library(dplyr)
library(tidyr)

a <- fromJSON('http://network.csiro.au:9500/standalone/publications.json?_page=1&_pageSize=10')
save(a, file = 'tmp.RData')

items <- a$result$items

# f_keyword <- function(df) {
#   tryCatch({
#     data_frame(keyword = df$keyword[[1]])
#   }, error = function(e) {
#     assign('df', df, .GlobalEnv)
#     stop()
#   })
# }
# 
# keyword <- items %>% 
#   group_by(`_about`) %>% 
#   do(f_keyword(.))

f_author <- function(df) {
  tryCatch({
    a_class <- class(df$author[[1]])
    if (a_class == 'data.frame') {
      n <- df$author[[1]]$hasName
      if (class(n$firstName) == 'list' | class(n$title) == 'list') {
        n <- unnest(n)
      }
      n$`_about` <- NULL
    } else if (a_class == 'list') {
      n <- as.data.frame(df$author[[1]]$hasName)
      n$X_about <- NULL
    } else {
      n <- data.frame(
        firstName = NA_character_,
        lastName = NA_character_,
        title = NA_character_)
    }
    if (class(n$firstName) == 'list') {stop()}
    return (n)
  }, error = function(e) {
    assign('df', df, .GlobalEnv)
    stop()
  })
}


author <- items %>% 
  group_by(`_about`) %>% 
  do(f_author(.))

# 
# f_classification <- function(df) {
#   tryCatch({
#     a_class <- class(df$classification[[1]])
#     if (a_class == 'data.frame') {
#       
#       n <- df$classification[[1]]
#       if (class(n$name) == 'list') {
#         n <- unnest(n)
#       }
#       n$`_about` <- NULL
#     } else if (a_class == 'list') {
#       n <- as.data.frame(df$classification[[1]])
#       n$`_about` <- NULL
#     } else {
#       
#       n <- data.frame(
#         name = NA_character_)
#     }
#     if (class(n$firstName) == 'list') {stop()}
#     return (n)
#   }, error = function(e) {
#     assign('df', df, .GlobalEnv)
#     stop()
#   })
# }
# 
# 
# classification <- items %>% 
#   group_by(`_about`) %>% 
#   do(f_classification(.))


f_year <- function(df) {
  tryCatch({
    data_frame(yearOfPublication = df$yearOfPublication[[1]])
  }, error = function(e) {
    assign('df', df, .GlobalEnv)
    stop()
  })
}

yearOfPublication <- items %>% 
  group_by(`_about`) %>% 
  do(f_year(.))


# Merge all data together


re <- merge(author, yearOfPublication, by="_about")

new_items <- items %>% 
  tbl_df() %>% 
  select(-author, -yearOfPublication) %>% 
  left_join(author) %>% 
  left_join(yearOfPublication) 


library(rjson)
json_file <- "http://network.csiro.au:9500/standalone/publications.json?_page=1&_pageSize=10"
json_data <- fromJSON(file=json_file)

for (item in json_data$result$items) {
  if (is.null(item$author[["hasName"]])) {
    for (auth in item$author) {
      if (is.recursive(auth)) {
        print (auth$hasName$lastName)
      }
    }
  }
  else {
    print (item$author[["hasName"]][["lastName"]])
  }
}
