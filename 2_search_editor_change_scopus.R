# 2_search_editor_change_scopus.R
# search for an editorial on an change in editor-in-chief
# using scopus; change to pubmed instead
library(rscopus)
source('my.scopus.api.key.R') # get my API key

# search for editorials (document-type) from 2002 
# see here for search tips
query = 'doctype(ed) AND PUBYEAR > 2001 AND EXACTSRCTITLE(International Journal of Epidemiology) AND TITLE-ABS-KEY("editor")'
s = scopus_search(query = query, api_key = my.api.key, verbose = F) # search Scopus
all = NULL
if(s$total_results>0){
  for (k in 1:s$total_results){
    this = s$entries[[k]]
    frame = data.frame(journal = this$`prism:publicationName`,
                       title = this$`dc:title`,
                       type = this$subtypeDescription,
                       scopus = this$`dc:identifier`,
                       date = this$`prism:coverDisplayDate`)
    all = rbind(all, frame)
  }
}
# also get the abstract ... TO DO
a = abstract_retrieval(id='85019764197', identifier = c("scopus_id"), api_key = my.api.key)
