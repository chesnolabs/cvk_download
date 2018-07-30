library(tidyverse)
library(rvest)
library(stringr)
library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
mayors <- read_csv("output/all_mayors.csv")

link0 <- "http://www.cvk.gov.ua/pls/vm2015/PVM004?pt001f01=100"

links <- read_html(link0) %>% 
    html_nodes("#result .a1small") %>% 
    html_attr("href") %>%
    paste0("http://www.cvk.gov.ua/pls/vm2015/", .)

get_mayors <- function(link){
  
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
  
  control <- html3 %>% 
    html_nodes(".td0") %>% 
    html_text()
  
  
  if(!grepl("сіль|селищ", control)){  
    
    winner <- html3 %>% 
      html_nodes(".t2") %>% 
      html_table(fill = T)
    
    winner <- winner %>% 
      .[[1]] %>% 
      slice(-1) %>% 
      rename(council = X1,
             fullname = X2,
             nominated_by = X3,
             bio = X4) %>% 
      mutate(el_type = el$el_type,
             el_date = el$el_date) %>% 
      separate(council, into = c("region", "council"),
               sep = ", ", extra = "merge") %>% 
      separate(bio, into = c("country", "birthdate", "education", 
                             "party_membership", "workplace_residence"),
               sep = ", ", extra = "merge") %>% 
      separate(workplace_residence, into = c("workplace", "residence"),
               sep = ", місце проживання: ", extra = "merge") %>% 
      mutate(birthdate = str_extract_all(birthdate, "[:digit:]{2}.[:digit:]{2}.[:digit:]{4}", simplify = T),
             education = str_replace(education, "освіта ", "")) %>% 
      select(-country) %>% 
      select(fullname, el_type:el_date, everything())%>% 
      filter(fullname!=nominated_by)
    
    if("X5" %in% names(winner)){
      winner <- winner %>% select(-X5)
    }
    
    if(nrow(winner) > 0){
    
    link_results <- html3 %>% 
      html_nodes(".a1") %>% 
      html_attr("href")
    
    link_control <- html3 %>%
      html_nodes(".a1") %>%
      html_text %>%
      strsplit(", ") %>%
      map(2) %>%
      unlist()
    
    link_results <- paste0("http://www.cvk.gov.ua/pls/vm2015/", link_results)
    
    control_df <- winner %>% 
      left_join(data.frame(link_control, link_results), by = c("council" = "link_control"))
    link_results <- as.character(with(control_df, link_results[fullname!=nominated_by]))
    
    get_results <- function(l){
      html <- read_html(l)
      
      res <- html %>% 
        html_nodes("tr:nth-child(2) .td2") %>% 
        html_text()
      
      Sys.sleep(0.01)
      return(res)
    }
    
    res_numbers <- map(link_results, get_results) 
    
    results <- res_numbers %>% 
      plyr::ldply() %>% 
      select(-V2) %>% 
      rename(fullname = V1, vote_number = V3, vote_percent = V4) %>% 
      mutate(fullname = str_trim(fullname),
             vote_number = as.integer(as.character(vote_number)),
             vote_percent = as.numeric(str_replace(as.character(vote_percent), ",", "."))) %>% 
      left_join(winner) %>% 
      select(fullname, el_type:residence, vote_number:vote_percent)
    } else {
      results <- winner %>% 
          mutate(vote_number = integer(),
               vote_percent = numeric())
    }
  } else {
    
    # results for countryside
    
    results <- html3 %>% 
      html_nodes(".t2") %>% 
      html_table(fill = T)
    
    obl <- html3 %>% 
      html_nodes(".p2") %>% 
      html_text() %>% 
      str_replace(" область", "")
    
    results <- results %>% 
      .[[1]] %>% 
      slice(-1) %>% 
      rename(council = X1,
             fullname = X2,
             nominated_by = X3,
             bio = X4) %>% 
      mutate(el_type = el$el_type,
             el_date = el$el_date,
             region = obl) %>% 
      separate(bio, into = c("country", "birthdate", "education", 
                             "party_membership", "workplace_residence"),
               sep = ", ", extra = "merge") %>% 
      separate(workplace_residence, into = c("workplace", "residence"),
               sep = ", місце проживання: ", extra = "merge") %>% 
      mutate(birthdate = str_extract_all(birthdate, "[:digit:]{2}.[:digit:]{2}.[:digit:]{4}", simplify = T),
             education = str_replace(education, "освіта ", "")) %>% 
      select(-country) %>% 
      select(fullname, el_type:region, everything())%>%
      filter(fullname!=nominated_by) %>% 
      mutate(vote_number = NA,
             vote_percent = NA)
    
    if("X5" %in% names(results)){
      results <- results %>% select(-X5)
    }
    
  }
  
  results$oblast <- read_html(link) %>% 
    html_nodes(".p2") %>% 
    html_text()
  results$link <- link
  
  print(paste0(results$region[1], ", ", control, ", ", results$el_date[1], " - ", nrow(results), " ос."))
  Sys.sleep(0.05)
  return(results)
}

all_mayors <- map(links, get_mayors)
# map(all_mayors, ~nrow(.))

all_mayors_df <- map_df(all_mayors, as.data.frame) %>% 
  mutate(council_type = ifelse(str_detect(council, "міська"), "міська",
                              ifelse(str_detect(council, "районна рада"), "районна",
                                    ifelse(str_detect(council, "селищна"), "селищна",
                                          ifelse(str_detect(council, "сільська"), "сільська", NA)))),
         oblast = str_remove(oblast, " область")) %>% 
  select(fullname, oblast, region, council:vote_percent, link) %>% 
  mutate(region = case_when(
    !str_detect(region, "обл|р-н") ~ str_extract(council, ".*(?=,)"),
    TRUE ~ region),
         council = str_remove(council, ".*, "))

all_mayors$council_type[grepl("міська сільська", all_mayors$council)] <- "сільська"

write_csv(all_mayors_df, "output/all_mayors.csv")

deputieswb <- createWorkbook("output/all_mayors.xlsx")
addWorksheet(deputieswb, "Голови міст і сіл")
writeDataTable(deputieswb, "Голови міст і сіл", all_mayors_df, withFilter = F, rowNames = F)
saveWorkbook(deputieswb, "output/all_mayors.xlsx", overwrite = TRUE)

