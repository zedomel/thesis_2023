library(rdatacite)
library(litsearchr)
library(ggraph)

datasets = NULL
cur_page = 1
while (TRUE) {
  result <- dc_dois(query='subjects.subject:("ecological interaction" OR "biotic interaction" OR "species interaction" OR "interspecific interactions") AND types.resourceType:dataset', 
                   resource_type_id = 'dataset',
                   page = cur_page)
  if (length(result$data) == 0) {
    break
  }
  
  datasets <- dplyr::bind_rows(datasets, result$data)
  cur_page <- cur_page + 1
}
datasets

subjects <- datasets$attributes$subjects
features <- unique(unlist(lapply(subjects, function(s) {
  s$subject
})))
features

keywords <- unlist(lapply(subjects, function(kw) {
  kw$subject
}))
keywords

extracted.keywords <- extract_terms(keywords = keywords, method = 'tagged', min_n=1, max_n = 2)
extracted.titles <- extract_terms(text = paste(unlist(datasets$attributes$titles), unlist(datasets$attributes$descriptions)),
                                 method = 'fakerake',
                                 min_freq = 2,
                                 ngrams = T,
                                 min_n = 2,
                                 language = 'English'
)
extracted.keywords
extracted.titles

features <- unique(append(extracted.keywords, extracted.titles))


dfm = create_dfm(
  elements = paste(unlist(datasets$attributes$titles), unlist(datasets$attributes$descriptions)),
  features = features
)

graph <- create_network(
  search_dfm = dfm,
  min_studies = 2,
  min_occ = 2
)
graph

ggraph(graph, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)

cutoff <- find_cutoff(
  graph,
  method = 'cumulative',
  percent = .90,
  imp_method = 'strength'
)
cutoff

reducegraph = reduce_graph(graph, cutoff_strength = cutoff[1])
reducegraph

searchterms <- get_keywords(reducegraph)
searchterms
