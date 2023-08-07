library(dplyr)
library(tm)
library(tidytext)
library(ggplot2)
library(hrbrthemes)
library(stats)
library(factoextra)

load('tmp/preprocess.RData')
#load(file='tmp/clustering.RData')

#' 
#' CLUSTERING CORPUS DOCUMENTS
#' 
#' @author Jose A Salim
#' 

df.pos.uniq %>%
  group_by(GROUP) %>%
  summarise(ndocs=n_distinct(doc_id))
  
# Create a term frequency matrix for groups (interaction terms)
df.groups <- df.pos.uniq %>%
  group_by(GROUP) %>%
  nest() %>%
  mutate(tfidf = purrr::map(
    data,
    \(x) x %>% 
      group_by(doc_id,lemma) %>%
      dplyr::count(lemma) %>%
      bind_tf_idf(term=lemma,document=doc_id,n=n) %>%
      ungroup()
  )) %>%
  unnest(tfidf) %>%
  ungroup() %>%
  dplyr::select(-data) %>%
  group_by(GROUP) %>%
  dplyr::mutate(nterms=n()) %>%
   ungroup() %>%
  group_by(GROUP,lemma) %>%
  dplyr::summarise(n=sum(n), tf=n/first(nterms), idf=first(idf), tf_idf=tf*idf) %>%
  filter(idf>0) %>%
  ungroup() %>%
  filter(n>5)
head(df.groups)


# Create a DTM for groups
df.groups <- df.pos.uniq %>%
  group_by(GROUP, lemma) %>%
  dplyr::count(lemma) %>%
  ungroup()
head(df.groups)

dtm.groups <- df.groups %>% cast_dtm(GROUP,lemma,n, tm::weightTfIdf)
inspect(dtm.groups)

# Calculate distances using cosine similarity 
dtm.matrix <- as.matrix(dtm.groups)
dim(dtm.matrix)
dtm.matrix <- dtm.matrix[,-c(42921:42968)] # remove chinese words
csim <- dtm.matrix / sqrt(rowSums(dtm.matrix * dtm.matrix))
csim <- tcrossprod(csim)
cdist <- as.dist(1 - csim)

# Hierarchical clustering
hcl <- hclust(cdist, method = 'ward.D2')
plot(hcl)
cut_avg <- cutree(hcl, k = 3)
cut_avg
rect.hclust(hcl , k = 3, border = 2:5)
abline(h = 1, col = 'red')

library(factoextra)
sub_grp = cutree(hcl, k=3)
plt <- fviz_cluster(list(data = cdist, cluster = sub_grp), 
                    ggtheme = theme_ipsum(base_family = 'Arial'), ylim=c(-2,2), xlim=c(-2.5,1.5),
                    main="PCA plot of clusters")
plt
ggsave('images/pca-cluster-plot.png', plt, dpi = 300)



library(dendextend)
hcd = as.dendrogram(hcl, hang = 0.2)
avg_col_dend <- color_branches(hcd, k = 3, groupLabels = T)
plot(avg_col_dend, main="Clustering ''interaction terms'' classes", sub="Clustering by overall word frequencies (all documents within a class)")
abline(h = 1, col = 'red')


nodePar <- list(lab.cex = 1, pch = c(NA, 19), cex = 1)
plot(hcd,  yaxt = 'n', nodePar = nodePar, main="Clustering by ''interaction terms'' results")


#library(pheatmap)
#pheatmap(mat=dtm.matrix, cluster_rows = hcl, cluster_cols = hcl)

#hm <- heatmap(dtm.matrix, hclustfun=function(x) {
#  hclust(x, method="ward.D2")
#  },
#  distfun = \(x) {
#    dist(x)
    #csim <- x / sqrt(rowSums(x * x))
    #csim <- tcrossprod(csim)
    #as.dist(1 - csim)
#  },
#  margins = c(10,3))
#hm

#Multimendional Scale
# Multidimensional scaling groups to 2D space
mds <- cmdscale(cdist, k =2, add = T)
#mds <- sammon(dist.m)
#mds$points <- as.data.frame(mds$points)
mds <- as.data.frame(mds$points)
names(mds) <- c("x", 'y')
#names(mds$points) <- c("x", 'y')
mds$groups <- rownames(mds)
head(mds)

plt <- ggplot(mds, aes(x=x, y=y, col=mds$groups)) +
  coord_cartesian(xlim = c(min(mds$x)-0.2,max(mds$x)+0.2)) +
  geom_point(size=5, show.legend = F) +
  geom_label(label=mds$groups, show.legend = F) +
  labs(title = "Multidimensional scalling of the DTM") +
  theme_ipsum(
    base_family = 'Arial'
  )
plt
ggsave(filename = 'images/mds.png', plt, dpi = 300)

# Statically compare groups
groups <- unique(df.groups$GROUP)
groups

ks.pvalues <- matrix(NA,nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
wilcox.pvalues <- matrix(NA,nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))

for (i in 1:length(dtm.groups)) {
  gi <- as.matrix(dtm.groups[i,])
  if (i < length(dtm.groups)) {
    for(j in (i+1):length(dtm.groups)) {
      gj <- as.matrix(dtm.groups[j,])
      
      #ks.res <- ks.test(gi[1,], gj[1,])
      wilcox.res <- wilcox.test(gi[1,], gj[1,], paired = T)
      #ks.pvalues[i,j] <- ks.pvalues[j,i] <- ks.res$p.value
      wilcox.pvalues[i,j] <- wilcox.pvalues[j,i] <- wilcox.res$p.value
      
      print(paste(groups[i], groups[j], sep = ' - '))
      #print(ks.res)
      print(wilcox.res)
    }
  }
}

#ks.pvalues<0.01
wilcox.pvalues<0.01
dtm.groups[1,]
