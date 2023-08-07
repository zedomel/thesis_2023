library(bibliometrix)
library(tidytext)
library(tidyr)
library(textstem)
library(SnowballC)
library(udpipe)
library(stringr)
library(parallel)
library(tm)
library(circlize)
library(dplyr)


#'
#' PREPROCESS PUBLICATION CORPUS
#' 
#' @author Jose A. Salim
#' @note used in \code{bib-analysis-wos.R}
#'  

### Read publications from WoD bibtex to data.frame
files <- list.files(path = 'data/dimensions', pattern = '*.csv', full.names = T)
agg_freq <- NULL
df <- NULL
for (file in files) {
  m <- str_match(file, 'data/dimensions/(.*).csv')
  if (!all(is.na(m))) {
    print(file)
    # Read bibtex file into bibliometrix df
    df.aux <- convert2df(file, dbsource = 'dimensions', format = 'csv')
    
    # Assign term/query strings as group 
    df.aux$GROUP = paste(m[2], 'interaction', sep = ' ')
    df <- bind_rows(df, df.aux)
  }
}
rm(files, df.aux, file, m)

# Add unique DOC_ID
df$DOC_ID <- 1:nrow(df)

# Save RDS
saveRDS(df, file='data/dimensions/publications.rds')

# Load RDS
df <- readRDS(file='data/dimensions/publications.rds')

# Number of publications
dim(df)

# Number of publications by group (term)
df %>%
  dplyr::count(GROUP) %>%
  mutate(perc=round(n/sum(n)*100,2)) %>%
  arrange(desc(n))

# Remove duplicate publications returned by different search strings (group by: title, authors, and pub. year)
df.uniq <- df %>%
  group_by(AU,TI,PY) %>%
  add_count() %>%
  filter(n <= 1) %>%
  ungroup()
head(df.uniq)

# Select only "duplicated" publications 
df.docId.dup <- df %>%
  group_by(AU,TI,PY) %>%
  add_count() %>%
  filter(n > 1) %>%
  ungroup() %>%
  mutate(n=1) %>%
  pivot_wider(id_cols=c(AU,TI,PY), names_from = GROUP,values_from = n,  values_fill = list(n=0),
              values_fn =list(n=function(x){head(x,1)})) %>%
  select(-AU,-TI,-PY)
df.docId.dup

# Number of publications removed
dim(df) - dim(df.uniq)

dim(df.uniq)

df.uniq <- read.csv('../../python/data/dimensions/publications-ecology-filtered-13701.csv', header = T)
dim(df.uniq)

library(reticulate)
texts <- py_load_object('../../python/data/m-uniq/texts.pkl')
texts <- sapply(texts, paste, collapse=" ")
df.uniq$text <- texts
df.uniq$doc_id <- df.uniq$X
res.pos.filtered <- df.uniq %>%
  unnest_tokens(lemma, text)


# Count publications per group
df.uniq %>%
  dplyr::count(GROUP) %>%
  mutate(perc=round(n/sum(n)*100,2)) %>%
  arrange(desc(n))

# Cast pub. year to character
df.uniq$PubYear <- as.character(df.uniq$PubYear)

#df.uniq <- df.uniq %>%
#  mutate(AB=stringr::str_replace_all(AB, "\\s+", " "), TI=stringr::str_replace_all(TI, "\\s+", " "))

#write.table(df.uniq, file = 'data/datasets/publications-uniq.tsv', sep = '\t', row.names = F)

# Custom stopwords list
mystopwords <- read.csv('../../python/stopwords.txt', header = F)
head(mystopwords)

# UdPipe model for POS
ud_model <- udpipe_download_model(language = "english-gum")
ud_model <- udpipe_load_model(ud_model$file_model)
df.pos <- df.uniq %>%
  #mutate(text=stringr::str_to_lower(paste(TI, AB, sep = '. '))) %>% # Combine title abstract and author keywords
  mutate(text=gsub("\\.\\.+", ".", text)) %>%
  mutate(text=gsub("<.*/?>","", text, ignore.case = T, perl=T)) %>% # remove html tags
  mutate(text=gsub("[\\-\\‐\\–/:]", " ", text, perl=T)) %>%
  #mutate(text=gsub("[^a-zA-Z\\s\\-\\.!\\?,;:]","", text, ignore.case=T, perl=T)) %>% # remove numbers, special characters and any non-alphabetic chars
  dplyr::rename(doc_id=X) %>% # add unique DOC_ID
  select(doc_id,text)

# POS
res.pos <- udpipe(df.pos, ud_model, parallel.cores = detectCores(logical = T)-1, trace = T)
res.pos %>% sample_n(10)


# Save results
saveRDS(res.pos, file='data/pos-anno.rds')
# Load results from file
#res.pos <- readRDS(file = "data/pos-anno.rds")

# Clean corpus
res.pos.filtered <- res.pos %>%
  dplyr::filter(upos == 'NOUN' | upos == 'ADJ')%>%
  dplyr::mutate(lemma=gsub("[^a-zA-Z]","", lemma, ignore.case=T, perl=T)) %>% # remove numbers, special characters and any non-alphabetic chars
  dplyr::anti_join(stop_words, by = join_by(lemma==word)) %>%
  dplyr::anti_join(stop_words, by = join_by(token==word)) %>%
  dplyr::anti_join(mystopwords, by = join_by(lemma==word)) %>%
  dplyr::anti_join(mystopwords, by = join_by(token==word)) %>%
  dplyr::filter(stringr::str_length(lemma) > 3) %>% #remove words smaller than 3 chars
  dplyr::mutate(stem=wordStem(lemma)) %>% # Stemming 
  dplyr::mutate(bigram=txt_nextgram(lemma, n=2), bigram_stem=txt_nextgram(stem,n=2)) %>% # Bigrams stemming
  dplyr::anti_join(mystopwords, by = join_by(bigram==word)) %>% # remove bigrams in stopwords list
  dplyr::group_by(stem) %>%
  dplyr::mutate(ndocs=n_distinct(doc_id)) %>% #count lemma per document
  dplyr::ungroup()%>%
  dplyr::filter(ndocs>0.001 * length(unique(res.pos$doc_id))) %>% # remove lemma in < 0.1% of docs
  dplyr::filter(ndocs<length(unique(res.pos$doc_id)))

res.pos.filtered %>%
  sample_n(10) %>%
  select(lemma,stem)

# Create a DTM from lemmas
dtm.tokens.tf <- res.pos.filtered %>%
  dplyr::group_by(doc_id,lemma) %>%
  dplyr::count(lemma) %>%
  dplyr::ungroup() %>%
  cast_dtm(doc_id,lemma,n, weighting = tm::weightTf)
head(dtm.tokens.tf)

dtm.tokens.tfidf <- res.pos.filtered %>%
  dplyr::group_by(doc_id,lemma) %>%
  dplyr::count(lemma) %>%
  dplyr::ungroup() %>%
  cast_dtm(doc_id,lemma,n, weighting = tm::weightTfIdf)
inspect(dtm.tokens.tfidf)

# Create a DTM from bigrams 
# TODO: best so far
dtm.bigram.tf <- res.pos.filtered %>%
  dplyr::mutate(bigram=gsub("[^a-zA-Z\\s]", "", bigram, perl=T)) %>%
  dplyr::filter(!is.na(bigram)) %>%
  dplyr::group_by(doc_id,bigram) %>%
  dplyr::count(bigram) %>%
  dplyr::ungroup() %>%
  cast_dtm(doc_id,bigram,n,weighting = tm::weightTf)
head(dtm.bigram.tf)

# Create a DTM with unigrams and bigrams
dtm.combined <- res.pos.filtered %>%
  dplyr::filter(!is.na(bigram)) %>%
  dplyr::group_by(doc_id,bigram) %>%
  dplyr::count(bigram) %>%
  dplyr::rename(token=bigram) %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(
    res.pos.filtered %>% 
      dplyr::group_by(doc_id,stem) %>%
      dplyr::count(stem) %>%
      dplyr::rename(token=stem) %>%
      dplyr::ungroup()
  ) %>%
  cast_dtm(doc_id, token, n, weighting = tm::weightTf)
inspect(dtm.combined)

# Join udpipe results with publications data
res.pos.filtered$doc_id <- as.integer(res.pos.filtered$doc_id)
df.pos.uniq <- res.pos.filtered %>%
  select(-GROUP) %>%
  inner_join(df.uniq, by=join_by(doc_id))
head(df.pos.uniq)
save.image(file = 'tmp/preprocess.RData')

