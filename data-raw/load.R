library(tidyverse)
library(stringr)
library(pdftools)

page_lines <- function(page) {
  page %>%
    str_split('\n') %>%
    unlist() %>%
    str_subset('^[:alpha:]')
}

  # (R[:digit:]{5})               # roster id
fields_regex <- stringr::regex("
  (^[A-Za-z\\ {1}]+?)           # campus
  [\\ ]+                        # whitespace
  (\\S+)                        # roster id
  [\\ ]+                        # whitespace
  ([A-Z\\ \\/{1}]+?(?=\\ \\ ))  # job family, with lookaround
  [\\ ]+                        # whitespace
  ([A-Z\\ {1}]+?(?=\\ \\ ))     # job title, with lookaround
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
                
  tibble(campus     = fields[2],
         roster.id  = fields[3],
         job.family = fields[4],
         job.title  = fields[5],
         dept       = fields[6],
         percent    = fields[7],
         funding    = funding)
}
  
txt <- pdf_text('http://www.cu.edu/doc/fy17-personnel-cu-roster-all-campuses.pdf')

pages <- txt %>% map(page_lines)

# first line from select pages
page1   <- pages[[1]][[1]]   # CU Boulder
page200 <- pages[[200]][[1]] # CU Denver Anschutz
page295 <- pages[[295]][[1]] # UCCS

page1 %>% line_fields
page200 %>% line_fields
page295 %>% line_fields

# cu_personnel <- txt %>%
#   map(page_lines) %>%
#   map_df(line_fields) %>%
#   filter(campus != '' & campus != 'CAMPUS') %>%
#   mutate(campus = as.factor(campus),
#          perc = as.numeric(perc),
#          funding = as.numeric(funding))
# 
# devtools::use_data(cu_personnel)
