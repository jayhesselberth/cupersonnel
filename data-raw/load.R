library(tidyverse)
library(pdftools)

page_lines <- function(page) {
  page %>%
    str_split('\n') %>%
    unlist() %>%
    str_subset('^[:alpha:]')
}

line_fields <- function(line) {
  fields <- strsplit(line, "\\s{2,}")[[1]]
 
  funding <- fields[8] %>%
    str_replace("\\$", "") %>%
    str_replace(",", "")
                
  tibble(campus     = str_trim(fields[1]),
         roster.id  = str_trim(fields[2]),
         dept.group = str_trim(fields[3]),
         dept       = str_trim(fields[4]),
         job.family = str_trim(fields[5]),
         job.title  = str_trim(fields[6]),
         percent    = str_trim(fields[7]),
         funding    = str_trim(funding))
}
  
txt <- pdf_text('https://www.cu.edu/doc/fy18personnelcurosterallcampusespdf')

pages <- txt %>% map(page_lines) %>% unlist()

cu_personnel <- pages %>%
  map_df(line_fields) %>%
  na.omit() %>%
  # filter out a few (<50) incorrectly parsed rows
  # filter(campus != 'CU') %>%
  # filter(campus != 'System') %>%
  mutate(campus     = as.factor(campus),
         dept.group = as.factor(dept.group),
         dept       = as.factor(dept),
         job.family = as.factor(job.family),
         job.title  = as.factor(job.title),
         percent    = as.numeric(percent),
         funding    = as.numeric(funding)
  )
 
devtools::use_data(cu_personnel, compress = 'xz', overwrite = TRUE)
