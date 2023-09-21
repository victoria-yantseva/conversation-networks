



library(stringr) 
library(textclean)
library(dplyr)
library(tidyr)
library(httr)
library (rvest)

#path to save files
setwd ("")


#paste the link to the needed subforum (e.g. integration and immigration, as below)
subforum <- 'https://www.flashback.org/f226p'  

# total number of pages there are in the subforum (check manually)
pages <-  # to be defined here


#################################################################################
                          # 1. CONSTRUCT A LIST OF URLs
#################################################################################

# 1. access each subforum page

raw_pages <- list ()

page_links <- paste0(subforum, 1:pages)

for (i in 1:length (pages)) {
  
  tryCatch(raw_page <- httr::GET (page_links[i], timeout(30)),
           error = function(e) { raw_pages[i] <<- "Timed out!" }) #access article url
  raw_pages[i] <- httr::content(raw_page, "text") #extract content
  
  Sys.sleep(floor(runif(1, min=0, max=10)))
}

#generate links to each thread
flashback_links <- list()
headers <- list()
for (i in 1:length (raw_pages)) {
  flashback_links[[i]] <- str_extract_all (raw_pages[[i]], 'id=\\"thread_title_(?s).*?>')%>%
                          str_extract_all('[0-9]+')
  
  header <- str_extract(raw_pages[[i]], 'id=\\"thread_title_(?s).*?</a>') %>%
    str_extract_all(">(?s).*?<")%>%
    str_remove_all(">|<")
  
  headers[[i]] <- rep (header, times=lengths (flashback_links[[i]]))
  
}

flashback_links <- paste0 ('https://www.flashback.org/t', unlist(flashback_links))



#2. access each thread page

raw_threads <- list ()

for (i in 1:length (flashback_links)) {
  print(i)
  tryCatch(raw_thread <- httr::GET (flashback_links[[i]], timeout(30)),
           error = function(e) { raw_threads[[i]] <<- "Timed out!" }) #access article url
  raw_threads[[i]] <- httr::content(raw_thread, "text") #extract content
  
  Sys.sleep(floor(runif(1, min=0, max=10)))
}


# determine whether the thread has been deleted
thread_deleted <- list()

for (i in 1:length (raw_threads)) {
  thread_deleted[[i]] <- str_extract_all (raw_threads[[i]], '<p class=\\"text-warning\\">')[[1]]
  
    }  

deleted <- unlist(lapply(thread_deleted, function(x) if(identical(x, character(0))) "NA" else "Deleted"))


# determine thread length (number of pages)
thread_length <- list()

for (i in 1:length (raw_threads)) {
  thread_length[[i]] <- str_extract (raw_threads[[i]], '<span class=\\"input-page-jump\\" data-total-pages=\\"(?s).*?\\"')%>%
    str_extract('[0-9]+')%>%
    as.numeric()
} 

tlength <- unlist(lapply(thread_length, function(x) ifelse(is.na(x), 1, x)))


#create a df with thread links and number of pages
flashback_df <- data.frame (url = flashback_links, deleted = deleted, length = tlength)
flashback_df<-flashback_df[flashback_df$deleted !="Deleted",]
flashback_df$deleted <- NULL


#3. generate links to each of the pages

url_list <-  rep (paste0 (flashback_df$url, "p"), times = as.numeric(flashback_df$length))


#create correct numeric sequences depending on the number of pages in each thread
nums <-  unlist (lapply (seq_along (flashback_df$length), function (i)
  seq(from = 1, to = flashback_df$length [i])))

#paste numbers to each link
url_final <- paste0 (url_list, nums)
head(url_final, 25)


#save
subforum_no <- str_extract_all(subforum, "[0-9]+")

saveRDS(url_final, paste0("url_final_", subforum_no, "_", pages, ".rds")) 
saveRDS(flashback_df, paste0("flashback_df_", subforum_no, "_", pages, ".rds")) 


