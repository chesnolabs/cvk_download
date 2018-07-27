library(tidyverse)
library(rvest)
library(stringr)
library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here

main_link <- "http://www.cvk.gov.ua/pls/vm2015/PVM005?PT001F01=100&pt00_t001f01=100"
links_by_type <- read_html(main_link) %>% 
  html_nodes("#result .a1small") %>% 
  html_attr("href")
links_by_type <- paste0("http://www.cvk.gov.ua/pls/vm2015/", links_by_type)

councils_list <- vector("list", length(links_by_type))

get_councils <- function(link){
  oblast <- read_html(link) %>% 
    html_nodes("#result .p1") %>% 
    html_text() %>% 
    str_trim()
  
  links_councils <- read_html(link) %>% 
    html_nodes(".a1") %>% 
    html_attr("href")
  
  councils <- read_html(link) %>% 
    html_nodes("tr+ tr td") %>% 
    html_text() %>% 
    matrix(ncol = 3, byrow = T) %>% 
    as_data_frame() %>% 
    rename(council = V1, deputies_total = V2, deputies_elected = V3) %>% 
    mutate(deputies_total = as.integer(deputies_total),
           deputies_elected = as.integer(deputies_elected),
           oblast = rep(oblast, nrow(.)),
           link_council = paste0("http://www.cvk.gov.ua/pls/vm2015/", links_councils))
  Sys.sleep(0.1)
  print(oblast)
  print(cat(councils$council, sep = ", "))
  return(councils)
}

councils <- map(links_by_type, get_councils)
councils_df <- councils %>% 
  bind_rows %>% 
  mutate(council = str_trim(council))



# links0 <- paste0("http://www.cvk.gov.ua/pls/vm2015/PVM002?PT001F01=", 100:500, "&pt00_t001f01=100")
# 
# all_links1 <- character()
# get_links1 <- function(l){
#   html <- read_html(l)
#   
#   links1 <- html %>% 
#     html_nodes("#result .a1small") %>% 
#     html_attr("href")
#   links1 <- paste0("http://www.cvk.gov.ua/pls/vm2015/", links1)
#   all_links1 <- c(all_links1, links1)
#   print(paste("got", length(links1), "links"))
#   Sys.sleep(0.1)
#   return(all_links1)
# }
# 
# all_links1 <- map(links0, possibly(get_links1, otherwise = NA))
# all_links1_clean <- unlist(all_links1)
# all_links1_clean <- all_links1_clean[!is.na(all_links1_clean)&all_links1_clean!="http://www.cvk.gov.ua/pls/vm2015/"]
# 
# all_links2 <- character()
# get_links2 <- function(l){
#   html <- read_html(l)
#   
#   links2 <- html %>% 
#     html_nodes(".a1") %>% 
#     html_attr("href")
#   links2 <- paste0("http://www.cvk.gov.ua/pls/vm2015/", links2)
#   all_links2 <- c(all_links2, links2)
#   print(paste("got", length(links2), "links"))
#   Sys.sleep(0.1)
#   return(all_links2)
# }
# 
# all_links2 <- map(all_links1_clean, possibly(get_links2, otherwise = NA)) 
# all_links2_clean <- unlist(all_links2)
# all_links2_clean <- all_links2_clean[!is.na(all_links1_clean)]

# adapt this to actual deputies!! 

get_local_deputies <- function(link){
  
  print(link)
  
  html3 <- read_html(link)
  
  el <- html3 %>% 
    html_nodes(".t0 b") %>% 
    html_text() %>% 
    as_data_frame() %>% 
    mutate(el_date = str_extract(value, "[:digit:]{2}.[:digit:]{2}.[:digit:]{4}"),
           value = str_replace(value, " [:digit:]{2}.[:digit:]{2}.[:digit:]{4}", ""),
           value = str_to_lower(str_replace(value, "[ ]вибори", ""))) %>% 
    rename(el_type = value)
  
  council <- html3 %>% 
    html_nodes(".p2") %>% 
    html_text() %>% 
    as_data_frame() %>% 
    separate(value, into = c("region", "council"), sep = ", ")
  
  if(grepl("сіль|селищ", council[2])){
    
    results <- html3 %>% 
      html_nodes("br+ .t2") %>% 
      html_table() 
    
    if(length(results)>0){  
      
      results <- results %>% 
        .[[1]] %>% 
        slice(-1) %>% 
        rename(id_in_council = X1,
               fullname = X2,
               nominated_by = X3,
               bio = X4) %>%
        filter(!str_detect(fullname, "повторне голосування|Призначені проміжні")) %>% 
        mutate(el_type = el$el_type,
               el_date = el$el_date,
               region = council$region, 
               council = council$council) %>% 
        separate(bio, into = c("country", "birthdate", "education", 
                               "party_membership", "workplace_residence"),
                 sep = ", ", extra = "merge") %>% 
        separate(workplace_residence, into = c("workplace", "residence"),
                 sep = ", місце проживання: ", extra = "merge") %>% 
        mutate(birthdate = str_extract_all(birthdate, "[:digit:]{2}.[:digit:]{2}.[:digit:]{4}", simplify = T),
               education = str_replace(education, "освіта ", "")) %>% 
        select(-country) %>% 
        select(id_in_council:fullname, el_type:council, everything())
    }
  } else {
    
    # results for proportional
    
    results <- html3 %>% 
      html_nodes(".t2+ .t2") %>% 
      html_table() 
    
    if(length(results)>0){  
      
      results <- results %>% 
        .[[1]] %>% 
        slice(-1) %>% 
        rename(id_in_council = X1,
               fullname = X2,
               bio = X3,
               votes_number = X4,
               votes_percent = X5) %>% 
        mutate(el_type = el$el_type,
               el_date = el$el_date,
               region = council$region, 
               council = council$council,
               nominated_by = ifelse(str_detect(id_in_council, fixed("партія", ignore_case = T)), id_in_council, NA)) %>% 
        fill(nominated_by) %>% 
        filter(!str_detect(str_to_lower(id_in_council), "партія")) %>% 
        separate(bio, into = c("country", "birthdate", "education", 
                               "party_membership", "workplace_residence"),
                 sep = ", ", extra = "merge") %>% 
        separate(workplace_residence, into = c("workplace", "residence"),
                 sep = ", місце проживання: ", extra = "merge") %>% 
        mutate(nominated_by = str_replace(nominated_by, "[Пп]олітична партія ", ""),
               birthdate = str_extract_all(birthdate, "[:digit:]{2}.[:digit:]{2}.[:digit:]{4}", simplify = T),
               education = str_replace(education, "освіта ", ""),
               votes_number = as.integer(votes_number),
               votes_percent = as.numeric(str_replace(votes_percent, ",", "."))) %>% 
        select(-country) %>% 
        select(id_in_council:fullname, el_type:nominated_by, everything())
    }}
  
  print(paste0(results$council[1], ", ", results$el_date[1], " - ", nrow(results), " деп."))
  Sys.sleep(0.05)
  return(results)
}

all_deputies <- map(all_links2_clean, possibly(get_local_deputies, otherwise = NA))

index_empty <- map(all_deputies, ~length(.) == 0) %>% unlist %>% which()
all_deputies_clean <- all_deputies[-index_empty]
all_deputies_df <- map_df(all_deputies_clean, as.data.frame) %>% 
  select(-`.x[[i]]`) %>% 
  filter(!(fullname %in% c("Рада увійшла до складу об’єднаної громади",
                         "Вибори не відбулися") | is.na(fullname)))

all_deputies_df <- all_deputies_df %>% 
  mutate(council_type = ifelse(str_detect(council, "обласна"), "обласна",
                               ifelse(str_detect(council, "міська"), "міська",
                                      ifelse(str_detect(council, "районна рада"), "районна",
                                             ifelse(str_detect(council, "районна у місті"), "районна у місті",
                                                    ifelse(str_detect(council, "селищна"), "селищна",
                                                           ifelse(str_detect(council, "сільська"), "сільська", NA))))))) %>% 
  select(id_in_council:region, council_type, everything())
all_deputies_df$council_type[grepl("міська сільська", all_deputies_df$council)] <- "сільська"

write_csv(all_deputies_df, "output/all_deputies.csv")

deputieswb <- createWorkbook("output/all_deputies.xlsx")
addWorksheet(deputieswb, "Депутати всіх рівнів")
writeDataTable(deputieswb, "Депутати всіх рівнів", all_deputies_df, withFilter = F, rowNames = F)
saveWorkbook(deputieswb, "output/all_deputies.xlsx", overwrite = TRUE)

