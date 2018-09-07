# 2_search_editor_change_pubmed.R
# search for an editorial on an change in editor-in-chief
# using pubmed
library(rentrez)
library(dplyr)

# selected journals
journals = c('BMJ', 'Epidemiology')
journals.search = paste(paste(journals, '[SO]', sep=''), collapse=' OR ', sep='') # 

# article types
articles = c('Editorial', 'News')
articles.search = paste(paste(articles, '[PT]', sep=''), collapse=' OR ', sep='') # 

# search for editorials (document-type) from 2002 to 2016
# remove "guest editor"
query = paste("(editor[TI] NOT guest editor[TI]) AND (", articles.search, ") AND 2002:2016[PDAT] AND (", journals.search, ")", sep='')
s = entrez_search(db = 'pubmed', term = query)

# extract titles, dates, etc
e = entrez_summary(db='pubmed', id=s$ids)
journal = unlist(extract_from_esummary(e, 'fulljournalname'))
date = unlist(extract_from_esummary(e, 'pubdate'))
#edate = unlist(extract_from_esummary(e, 'epubdate'))
title = unlist(extract_from_esummary(e, 'title'))
frame = data.frame(journal)
frame$date = as.Date(date, format='%Y %b %d')
frame$title = title
frame = arrange(frame, journal, date)

# save
write.table(frame, file = "journal.editorials.txt", sep='\t', quote=F, row.names=F)
