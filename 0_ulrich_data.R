# 0_ulrich_data.R
# getting data on journals from Ulrich
# from ulrich FAQ: "Ulrichsweb.com users may print, email or download up to 200 full records per session in either ASCII text or ASCII-delimited formats"
# clients@serialssolutions.com 
# Sep 2018
library(dplyr)

data = read.csv('ulrich.export.csv', stringsAsFactors = F)
data = dplyr::select(data, Title, issn, SubjectCodes)
# is issn in Luke's data?

# save
#save(selected, file='For.Sample.Size.RData')

## getting data from API
# help here https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/
library(httr)
library(jsonlite)
library(xml2) 

url  = "http://ulrichsweb.serialssolutions.com/api/WYYIWQF9EF"
path = "search"
query = 'title:garden'

raw.result <- GET(url = url, path = path, query=query)
this.raw.content <- rawToChar(raw.result$content)
this.content <- fromJSON(this.raw.content)
