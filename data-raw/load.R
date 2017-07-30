library(tidyverse)
library(stringr)
library(pdftools)

page_lines <- function(page) {
  page %>%
    str_split('\n') %>%
    unlist() %>%
    str_subset('^[:alpha:]')
}

fields_regex <- stringr::regex("
  (^[A-Za-z\\ {1}]+?)           # campus
  [\\ ]+                        # whitespace
  (\\S{3,7}(?=\\ \\ ))          # roster id
  [\\ ]+                        # whitespace
  ([A-Z\\ \\/{1}]+?(?=\\ \\ ))  # job family, with lookaround
  [\\ ]+                        # whitespace
  ([A-Z\\ [:punct:][:digit:]]{1,30}(?=\\ ))     # job title, with lookaround
  [\\ ]+                        # whitespace
  ([A-Z\\ [:punct:]{1}]+?)      # department
  [\\ ]+                        # whitespace
  ([[:digit:]\\.]{1,5})         # percent
  [\\ ]+                        # whitespace
  (\\$[[:digit:],{1,}]+)$       # funding
  ", comments = TRUE)  

line_fields <- function(line) {
  fields <- str_match(line, fields_regex)
 
  funding <- fields[8] %>%
    str_replace("\\$", "") %>%
    str_replace(",", "")
                
  tibble(campus     = str_trim(fields[2]),
         roster.id  = str_trim(fields[3]),
         job.family = str_trim(fields[4]),
         job.title  = str_trim(fields[5]),
         dept       = str_trim(fields[6]),
         percent    = str_trim(fields[7]),
         funding    = str_trim(funding))
}
  
txt <- pdf_text('http://www.cu.edu/doc/fy17-personnel-cu-roster-all-campuses.pdf')

pages <- txt %>% map(page_lines) %>% unlist()

cu_personnel <- pages %>%
  map_df(line_fields) %>%
  mutate(campus     = as.factor(campus),
         job.family = as.factor(job.family),
         job.title  = as.factor(job.title),
         dept       = as.factor(dept),
         percent    = as.numeric(percent),
         funding    = as.numeric(funding)) %>%
  na.omit()
 
# devtools::use_data(cu_personnel)
