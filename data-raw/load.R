library(tidyverse)
library(stringr)
library(pdftools)

page_lines <- function(page) {
  page %>% str_split('\n') %>% flatten()
}

line_fields <- function(line) {
  tibble(
    campus = str_sub(line, 1, 12) %>% str_trim(),
    roster_id = str_sub(line, 13, 22) %>% str_trim(),
    job_family = str_sub(line, 23, 55) %>% str_trim(),
    job_title = str_sub(line, 56, 122) %>% str_trim(),
    perc = str_sub(line, 123, 125) %>% str_trim(),
    funding = str_sub(line, 126, 150) %>%
      str_trim() %>%
      str_replace('\\$', '') %>%
      str_replace(',', '')
  )
}

txt <- pdf_text('http://www.cu.edu/doc/fy17-personnel-cu-roster-all-campuses.pdf')

cu_personnel <- txt %>%
  map(page_lines) %>%
  map_df(line_fields) %>%
  filter(campus != '' & campus != 'CAMPUS') %>%
  mutate(campus = as.factor(campus),
         perc = as.integer(perc),
         funding = as.integer(funding))

devtools::use_data(cu_personnel)
