# 0_extract_journal_data.R
# extract gender data from journals supplied by PLOS Biology authors (huge data set stored in SQL)
# run on lyra
# July 2018
library(dplyr)
library(dbplyr)
library(stringr)
library(lubridate) # for dates

# Load up the database connection - this allows quick access to the whole PubMed dataset without loading the whole thing to memory
pubmed_db <- src_sqlite("pubmed_gender_data.sqlite3", create = F)
pubmed_sqlite <- tbl(pubmed_db, "papers")
journals_sqlite <- tbl(pubmed_db, "journals")

# get the list of journals
journal.frame = collect(journals_sqlite %>% select(title, short.title, ISSN))
head(journal.frame)

# extract date, journal and gender for individual papers ...
# ... then get number of female authors and total author numbers
date.frame = collect(pubmed_sqlite %>% select(date, gender, gender.score, journal)) %>% 
  mutate(females = str_count(gender, "F"),
    n = str_count(gender, "F|M"), # exclude authors of unknown gender
    date = as.Date(date, format='%d_%m_%Y'),
    year = year(date)) %>% 
  filter(n>0) %>% # only papers with one author or more (after excluding unknown authors)
  select(-gender, -gender.score) 

# restrict to journals still active in 2016 with a minimum year of 2012 (at least 5 years of data) ...
# ... then make a random selection of 500 journals
n.select = 500
jselected = date.frame %>%  
  group_by(journal) %>% 
  summarise(miny = min(year), maxy = max(year)) %>%
  filter(miny <= 2012, maxy==2016) %>%
  sample_n(size = n.select) 

# restrict papers to selected journals
selected = filter(date.frame, journal %in% jselected$journal)

# save
save(selected, journal.frame, file='For.Sample.Size.RData')
