

#install.packages ("stringr")
#install.packages ("textclean")
#install.packages ("dplyr")
#install.packages("tibble")
#install.packages("tidyr")
#install.packages ("urltools")
#install.packages ("httr")
#install.packages("jsonlite")

library(stringr) 
library(textclean)
library(dplyr)
library(tibble)
library(tidyr)
library(urltools)
library(httr)
library(jsonlite)


#set WD
setwd ("")

#load the RDS file with links
url_final <- readRDS ("") #path to the file with the urls saved at step 1. 



#################################################################################
# 2. SAVE EACH FORUM PAGE; RETRIEVE NECESSARY DATA AND CLEAN IT
#################################################################################


#collect raw html files
raw_files <- list ()
access_dates <- list ()

for (i in 1:length (url_final)) {
  
  if (i%%1000 == 0) 
  { print(i) }
  
  tryCatch(raw_file <- httr::GET (url_final[i], timeout(30)),
           error = function(e) { raw_files[[i]] <<- "Timed out!" }) #access article url
  
  access_dates [[i]] <- raw_file$date
  raw_files[[i]] <- httr::content(raw_file, "text", encoding = "ISO-8859-1") #extract content
  
  Sys.sleep(floor(runif(1, min=0, max=7)))
}


access_dates <- lapply(access_dates, function (x) as.Date(x))

saveRDS (raw_files, paste0("raw_files_", Sys.Date(), ".rds"))
saveRDS(access_dates, paste0("access_dates_", Sys.Date(), ".rds"))


#extract individual posts and headers

raw_posts <- list()
headers <- list()
for (i in 1:length (raw_files)) {
  
  raw_posts[[i]] <- str_extract_all (raw_files[[i]], 'data-postid=\\"(?s).*?<\\!-- / post #')
  header <- str_extract(raw_files[[i]], '<div class=\\"page-title\\">(?s).*?</div>') %>%
    str_remove_all("<.*?>|\\n|\\t")
  
  headers[[i]] <- rep (header, times=lengths (raw_posts[[i]]))
  
}

raw_posts<-unlist(raw_posts)
headers <-unlist(headers)


#create a df (nested data) and fill with the cleaned data


info <- tibble(postid = NA, # each post ids
               post = NA, #post texts (quotes removed)
               raw_post = NA, #raw posts without processing
               header = headers, #thread headers
               date = NA, #the date when post was published
               user = NA, #post's author
               userid = NA, #author's user id
               url = NA, #urls provided in the post text
               quote = NA, #other users quoted
               quoteid = NA, #post id quoted
               quotetext = NA) #text of the quoted post


for (i in 1:length (raw_posts)) {
  
  if (i%%1000 == 0) 
  { print(i) }
  
  #post IDs
  info [i, "postid"]  <- str_extract_all(raw_posts[[i]], '<div class=\\"post_message\\" id=\\"post_message_(?s).*?>')%>%
    str_extract("[0-9]+")
  
  
  #post texts 
  info [i, "post"] <- str_extract_all (raw_posts[[i]], '<div class=\\"post_message\\"(?s).*?<\\!--/\\.post-row-->')%>%
    str_replace_all('Ursprungligen postat av(?s).*?</div>\\n</div>\\n\\r\\n', " ") %>%
    gsub ("\tSenast redigerad av .*?\n\t\t\t", " ", .)%>%
    gsub ("Spoiler", " ", .)%>%
    gsub("[^[:alnum:][:blank:][:punct:]]", " ", .)%>%
    replace_html()%>%
    str_replace_all("[\n|\r]", " ")%>%
    str_trim ()%>%
    str_squish ()
  
  #raw posts
  info [i, "rawpost"] <- str_extract_all (raw_posts[[i]], '<div class=\\"post_message\\"(?s).*?<\\!--/\\.post-row-->')
  
  #headers already provided
  
  
  #post dates
  info [i, "date"] <-str_extract_all (raw_posts[[i]], '<i class=\\"fa fa-file\\">(?s).*?<a href')%>%
    str_extract_all('\\t.*?,.*?\\n', " ") %>%
    str_replace_all("[\n|\r|\t]", " ")%>%
    gsub ("Igår|Ig&aring;r", access_dates[[i]]-1, .)%>%
    gsub ("Idag", access_dates[[i]], .)%>%
    str_squish()
  
  #usernames
  info[i, "user"] <- str_extract_all(raw_posts[[i]], '<a class=\\"post-user-username dropdown-toggle\\"(?s).*?</a>')%>%
    str_extract("\\t\\t(?s).*?\\n")%>%
    str_replace_all("\\t|\\n", "")
  
  #user IDs
  info[i, "userid"]<- str_extract_all(raw_posts[[i]], '<a class=\\"post-user-username dropdown-toggle\\"(?s).*?</a>')%>%
    str_extract("href(?s).*?>")%>%
    str_extract_all('[0-9]+')
  
  #urls
  url  <- str_extract_all(raw_posts[[i]], '<a href=\\"/leave\\.php(?s).*?\\"')[[1]]%>%
    str_replace_all('<a.*?u=', "")%>%
    str_replace_all('\\"', "")%>%
    url_decode()
  
  info [i, "url"] [[1]]<- list(tibble(url = url))
  
  
  #quoted/mentioned users
  quote<- str_extract_all (raw_posts[[i]], 'Ursprungligen postat av (?s).*?</strong>')[[1]]%>%
    str_remove_all('<.*?>|Ursprungligen postat av')
  
  info[i, "quote"][[1]] <-  list(tibble(quote = quote))
  
  
  #quoted post IDs
  quoteid <- str_extract_all (raw_posts[[i]], '<div class=\"alt2 post-bbcode-quote\">(?s).*?</i></a>')[[1]]%>%
    str_extract_all('#p[0-9]+')%>% 
    str_replace_all ('#p', "")
  
  info[i, "quoteid"][[1]] <-  list(tibble(quoteid = quoteid))   
  
  
  #quoted text
  quotetext<- str_extract_all (raw_posts[[i]], '<div class=\\"post_message\\"(?s).*?<\\!--/\\.post-row-->')[[1]]%>%
    str_extract_all('Ursprungligen postat av(?s).*?</div>\\n</div>\\n') %>%
    replace_html()%>%
    str_replace_all("[\n|\r|\t]", " ")%>%
    str_squish()
  
  info[i, "quotetext"][[1]]<- list(tibble(quotetext = quotetext))             
  
}

#save
saveRDS (info, paste0("flashback_df_", Sys.Date(), ".rds"))

exportJSON <- toJSON(info, pretty = TRUE)
write(exportJSON, paste0("flashback_df_",  Sys.Date(), ".json"))







 
