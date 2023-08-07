library(textmineR)
library(jsonlite)
library(dataone)
library(rdryad)
library(rdatacite)
library(EML)
library(mime)
library(dplyr)
library(tidyr)
library(doMC)
library(XML)

readRenviron('../.Renviron')

#' Calculate topic coherence for given models
#' @param models LDA models
#' @param dtm \code{DocumentTermMatrix} 
#' @param control LDAControl
#' 
#' @return array of coherence of each model in \code{models}
topic_coherence <- function(models, dtm, control) {
  m <-  Matrix::sparseMatrix(i=dtm$i, 
                             j=dtm$j, 
                             x=dtm$v, 
                             dims=c(dtm$nrow, dtm$ncol),
                             dimnames = dtm$dimnames)
  metrics <- sapply(models, \(x, m) {
    phi <-topicmodels::posterior(x)$terms %>% as.matrix
    return(mean(CalcProbCoherence(phi, m)))
  }, m=m)
  return(metrics)
}

#' Plot topic modeling metrics
#' 
#' @param values topic modeling result
#' 
plotTMMetrics <- function (values) 
{
  if ("LDA_model" %in% names(values)) {
    values <- values[!names(values) %in% c("LDA_model")]
  }
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(values["topics"], base::apply(columns, 
                                                           2, function(column) {
                                                             scales::rescale(column, to = c(0, 1), from = range(column))
                                                           }))
  values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
  values$group <- !(values$variable %in% c("CaoJuan2009", 
                                         "Arun2010", "Perplexity"))
  values$group <- base::factor(values$group, levels = c(FALSE, 
                                                        TRUE), labels = c("minimize", "maximize"))
  p <- ggplot(values, aes_string(x = "topics", y = "value", 
                                 group = "variable"))
  p <- p + geom_line()
  p <- p + geom_point(aes_string(shape = "variable"), size = 3)
  p <- p + guides(size = FALSE, shape = guide_legend(title = "metrics:"))
  p <- p + scale_x_continuous(breaks = values$topics)
  p <- p + labs(x = "number of topics", y = NULL)
  p <- p + facet_grid(group ~ .)
  p <- p + theme_bw() %+replace% theme(panel.grid.major.y = element_blank(), 
                                       panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(colour = "grey70"), 
                                       panel.grid.minor.x = element_blank(), legend.key = element_blank(), 
                                       strip.text.y = element_text(angle = 90))
  g <- ggplotGrob(p)
  g$layout[g$layout$name == "strip-right", c("l", "r")] <- 3
  grid::grid.newpage()
  grid::grid.draw(g)
}

#'
#' Fit Jensen-Shannon PCA
#' 
#' @param phi array with LDA phi values
#' 
#' @return data.frame with x and y coordinates
#'
jsPCA <- function (phi) 
{
  jensenShannon <- function(x, y) {
    m <- 0.5 * (x + y)
    0.5 * sum(x * log(x/m)) + 0.5 * sum(y * log(y/m))
  }
  dist.mat <- proxy::dist(x = phi, method = jensenShannon)
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
}



#'
#' Parse HTTP response.
#' Convert JSON response to \code{data.frame}
#' 
#' @param res HTTP response
#' 
#' @return data.frame 
parse_response <- function(res)
{
  if (is.list(res)) {
    tmp <- lapply(res, function(z) z$parse("UTF-8"))
  } else {
    tmp <- res$parse('UTF-8')
  }
  jsonlite::fromJSON(tmp[[1]], flatten = TRUE)
}

#########################
# Repositories/Datasets #
#########################

#' Search a data repository
#' 
#' @param base_url base URL
#' @param path endpoint path
#' @param query query data
#' @param method HTTP method
#' @param headers HTTP headers
#'
#' @return \code{data.frame} with response body
#'
search_repository <- function(base_url, path, query, method='get', headers=list(`Content-Type` = "application/json", Accept = "application/json"), parse=TRUE)
{
  con <- crul::HttpClient$new(
    url = base_url, 
    headers = headers)
  if (method == 'get') {
    res <- con$get(path=path, query = query)
  } else {
    res <- con$post(path=path, body = query, encode = 'json')
  }
  if (parse) {
    return(parse_response(res))
  }
  return(res)
}

#'
#' Get datasets from Dryad
#'
#' @param term term to search
#' @param cols list of fields to return
#' 
#' @return datasets
get_dryad_datasets <- function(term){
  page <- 1
  count <- 0
  data.dryad <- NULL
  query <- list(q=sprintf("\"%s\"", paste(term, "interaction")), per_page=100,page=page)
  d <- search_repository('https://datadryad.org', '/api/v2/search', query, parse = F)
  res <- rdryad:::v2_parse(d$parse('UTF-8'))
  res <- rdryad:::parse_ds(res)
  
  print(paste("Found", res$meta$total, "datasets in Dryad for:", term))
  pb <- txtProgressBar(min=0,max=res$meta$total,style=3)
  res$data$identifier[1]
  while (res$meta$count > 0) {
    files_metadata <- unlist(get_dryad_files(res$data$id))
    datasets <- res$data %>%
      dplyr::select(-id) %>%
      dplyr::rename(id=identifier,pubDate=publicationDate) %>%
      dplyr::mutate(alternateId=NA,datatypes=files_metadata) %>%
      dplyr::mutate(keywords=sapply(keywords, paste, collapse=';'), term=term) %>%
      dplyr::mutate(hasGeoData=is.null(locations), hasTemporalData=NA,hasTaxonData=NA,metadataStd=NA) %>%
      dplyr::select(id,license,datatypes,title,pubDate,keywords,alternateId, term)
    
    metrics <- get_datacite_metrics(datasets$id)
    datasets <- datasets %>% dplyr::inner_join(metrics)
    
    data.dryad <- rbind(data.dryad, datasets)
    count <- count + res$meta$count
    
    setTxtProgressBar(pb,count)  
    
    page <- page + 1
    
    # Avoid rate limit
    Sys.sleep(2)
    
    query <- list(q=sprintf("\"%s\"", paste(term, "interaction")), per_page=100,page=page)
    d <- search_repository('https://datadryad.org', '/api/v2/search', query, parse = F)
    res <- rdryad:::v2_parse(d$parse('UTF-8'))
    res <- rdryad:::parse_ds(res)
  }
  
  close(pb)
  return(data.dryad)
}

#'
#' Get datasets from DataONE
#'
#' @param term search term
#' 
#' @return datasets
#'
get_dataone_datasets <- function(term)
{
  
  #DataOne
  cn <- CNode("PROD")
  mySearchTerms <- list(
    q=paste(paste(
      sprintf("(abstract:\"%1$s\" OR title:\"%1$s\" OR keywords:\"%1$s\"", paste(term,"interaction")),
      sprintf("abstract:\"%1$s\" OR title:\"%1$s\" OR keywords:\"%1$s\")", paste(term,"interactions")),
      sep = " OR "
    ), '-obsoletedBy:*', sep = " AND "),
    fl="id,resourceMap,identifier,title,formatId,keywords,pubDate",
    rows=1000,
    start=0
  )
  result <- dataone::query(cn, solrQuery=mySearchTerms, as="data.frame") 
  
  registerDoMC(detectCores(logical = T) - 1)
  datasets <- NULL
  while (nrow(result) > 0) {
    write.table(result %>% filter(!stringr::str_detect(formatId, 'eml')) %>% select(id,formatId), 
              file = 'data/datasets/dataone-non-eml.tsv', append = T,
              quote = FALSE, sep = '\t',
              col.names = FALSE, row.names = FALSE)
  
    #Filter out Dryad datasets
    #result <- result %>%
    # filter(!stringr::str_detect(formatId, "dryad"))
    
    print(paste("DataONE found", nrow(result), "datasets"))
    additionalData <- plyr::adply(result %>% select(id,formatId, resourceMap), \(x) {
      tryCatch(
        {
          raw <- rawToChar(getObject(cn, x$id))
          if (stringr::str_detect(x$formatId, "schema\\.org|ld+json")) {
            jsonContent <- jsonlite::fromJSON(raw)
            datatypes <- paste(unique(jsonContent$distribution$encodingFormat), collapse = ';')
            doi <- sapply(jsonContent$identifier, '[[', 1)
            license <- paste(jsonContent$license, collapse = ';')
            hasTemporalData <- !is.null(jsonContent$temporalCoverage)
            hasGeoData <- !is.null(jsonContent$spatialCoverage)
            hasTaxonData <- NA
          } else if (stringr::str_detect(x$formatId, "dryad")) {
            dpxml <- read_eml(raw)
            doi <- stringr::str_remove(dpxml$identifier, "https://doi.org/")
            datatypes <- NA
            license = ifelse(!is.null(dpxml$rights), dpxml$rights, NA)
            metadataStd = get_dryad_datapackage_metadata_std(raw)
            hasTemporalData <- !is.null(dpxml$temporal)
            hasTaxonData <- !is.null(dpxml$scientificName)
            hasGeoData <- !is.null(dpxml$spatial)
          } else {
              eml <- read_eml(raw)
              datatypes <- get_dataone_datatypes(eml, x$formatId)
              doi <- unlist(x$resourceMap)
              license = get_dataone_license(eml, x$formatId)
              hasTemporalData <- !is.null(eml$dataset$coverage$temporalCoverage)
              hasGeoData <- !is.null(eml$dataset$coverage$geographicCoverage)
              hasTaxonData <- !is.null(eml$dataset$coverage$taxonomicCoverage)
              metadataStd = stringr::str_split_1(eml$schemaLocation[1], "\\s+")[1]
          }
          
          doi <- ifelse(is.null(doi), NA, doi)
          dn.metrics <- get_dataone_metrics(x$id)
          dc.metrics <- get_datacite_metrics(doi)
          metrics <- dn.metrics %>%
            bind_rows(dc.metrics) %>%
            summarise(downloads=sum(downloads), views=sum(views), citations=sum(citations))
          return(data.frame(
            id=x$id,
            datatypes=datatypes,
            license=license,
            alternateId=doi,
            hasTemporalData=hasTemporalData,
            hasGeoData=hasGeoData,
            hasTaxonData=hasGeoData,
            metadataStd=metadataStd,
            metrics))
        },
        error=\(e) {
          print(paste("Error:", x$id))
          print(e)
          return(data.frame(
            id=x$id,
            datatypes=NA,
            views=0,
            downloads=0,
            citations=0,
            alternateId=NA,
            license=NA,
            hasTemporalData=NA,
            hasGeoData=NA,
            hasTaxonData=NA,
            metadataStd=NA))
      })
    }, .margins=1,
      .parallel = T, 
      .paropts = list(
        .packages=c('dplyr','rdatacite', 'plyr', 'stringr', 'jsonlite', 'EML', 'XML'),
        .export=c('get_dryad_datapackage_metadata_std', 'get_dataone_metrics', 'get_dataone_license', 'get_dataone_datatypes', 'get_datacite_metrics', 'get_eml_datatypes','get_gmd_datatypes')
      )
    )
    
    df <- result %>%
      left_join(additionalData, by = join_by(id)) %>%
      dplyr::mutate(term=term) %>%
      dplyr::mutate(keywords=sapply(keywords, paste, collapse=';')) %>%
      select(id,datatypes,title,pubDate,keywords,alternateId,views,downloads,citations,term)
    
    datasets <- rbind(datasets, df)
    
    mySearchTerms$start <- mySearchTerms$start + mySearchTerms$rows
    result <- dataone::query(cn, solrQuery=mySearchTerms, as="data.frame") 
    
  }
  
  return (datasets)
}

#'
#' Get datasets from Figshare
#'
#' @param search term
#' 
#' @return datasets
#'
get_figshare_datasets <- function(term)
{
  query <- list(search_for=paste(paste(sprintf("(:title:\"%1$s\" OR :description:\"%1$s\" OR :tag:\"%1$s\"", paste(term, "interaction")),
                sprintf(":title:\"%1$s\" OR :description:\"%1$s\" OR :tag:\"%1$s\")", paste(term, "interactions")),
                sep = " OR "),
                "AND :item_type:dataset"))
  page <- 1
  count <- 0
  data <- NULL
  pb <- txtProgressBar(min=0,max=100, style = 3)

  # Register parallel backend for dply (foreach)
  registerDoMC(detectCores(logical = T) - 1)
  
  while (TRUE) {
    path <- sprintf("/v2/articles/search?page_size=1000&page=%s", page)
    d <- search_repository("https://api.figshare.com", path, query, 'post')
    
    if(is.null(nrow(d))) {
      break
    }
    
    
    df <- plyr::ldply(
      d$id, get_figshare_item_details,
      .parallel = T, 
      .paropts = list(
        .packages=c('dplyr','tidyr', 'crul', 'purrr', 'mime','rdatacite', 'plyr', 'stringr'),
        .export=c('parse_response', 'get_figshare_metrics', 'get_datacite_metrics')
        )
    )
    df$term <- term
    data <- data %>% dplyr::bind_rows(df)
      
    Sys.sleep(1)
    page <- page + 1
    setTxtProgressBar(pb, page)
  }
  
  setTxtProgressBar(pb,100)
  close(pb)
  
  print(sprintf("Figshare found %d datasets", nrow(data)))
  data <- data %>%
    as_tibble()
  return(data)
}

#'
#' Get datasets from Zenodo
#' 
#' 
#' @param search term
#'
#' @return datasets
get_zenodo_datasets <- function(term)
{
  query <- list(
    q=paste(paste(sprintf("(title:\"%1$s\" OR description:\"%1$s\" OR keywords:\"%1$s\"", paste(term, "interaction")),
              sprintf("title:\"%1$s\" OR description:\"%1$s\" OR keywords:\"%1$s\")", paste(term, "interactions")),
              sep=" OR "),
              "AND access_right:open"),
    type='dataset',
    status='published',
    page=1,
    size=1000)
  headers <- list(
    `Content-Type` = "application/json", 
    Accept = "application/json",
    Authorization = sprintf('Bearer %s', Sys.getenv('ZENODO_API_TOKEN'))
  )
  data <- NULL
  pb <- txtProgressBar(min=0,max=100, style = 3)
  while (TRUE) {
    d <- search_repository('https://zenodo.org', '/api/records/', query, headers = headers)
    if(is.null(nrow(d))) {
      break
    }
    
    # Get files MIME-type
    datatypes <- paste(sapply(d$files, \(f) {
      unique(guess_type(f$filename))
    }), collapse = ';')
    
    names(datatypes) <- c('datatypes')
    
    d <- d %>% as_tibble() %>%
      mutate(datatypes=datatypes, term=term, keywords=sapply(metadata.keywords, paste, collapse=';'), 
             hasGeoData=is.null(metadata.locations),
             hasTaxonData=NA,
             hasTemporalData=NA,
             metadataStd="http://www.loc.gov/MARC21/slim;http://datacite.org/schema/kernel-3;http://datacite.org/schema/kernel-3;http://www.openarchives.org/OAI/2.0/oai_dc/;https://www.w3.org/ns/dcat;") %>%
      select(-id) %>%
      dplyr::rename(id=doi,license=metadata.license,pubDate=metadata.publication_date,alternateId=metadata.prereserve_doi.doi) %>%
      select(id,license,datatypes,title,pubDate,keywords,alternateId,term)
    
    metrics <- get_datacite_metrics(d$id)
    d <- d %>% 
      inner_join(metrics, by=join_by(id))
    
    data <- data %>% bind_rows(d)
    
    query$page <- query$page + 1
    setTxtProgressBar(pb, query$page)
  }
  setTxtProgressBar(pb,100)
  close(pb)
  
  print(sprintf("Zenodo found %d datasets", nrow(data)))
  data
}

#'
#' Get Dryad files MIME-type
#'
#'
get_dryad_files <- function(version_ids) {
  lapply(version_ids, \(id) {
    f <-dryad_versions_files(id)
    if (any(unlist(f) == "not-found", na.rm = T)) {
      return(NA)
    }
    Sys.sleep(3)
    return(paste(unique((f[[1]]$`_embedded`$`stash:files`$mimeType)), collapse=';'))
  })
}

#' 
#' Get Figshare item metatada
#' 
#' 
get_figshare_item_details <- function(id)
{
  con <- crul::HttpClient$new(
    url = 'https://api.figshare.com', 
    headers = list(
      `Content-Type` = "application/json", 
      Accept = "application/json")
  )
  res <- con$get(path=sprintf('/v2/articles/%s', stringr::str_trim(id)))
  item <- parse_response(res)
  item$license <- purrr::pluck(item, function(x){ x$license$url })
  item$categories <- paste(purrr::pluck(item, function(x){ x$categories$title }), collapse =';')
  datatypes <- paste(sapply(item$files$name, \(f) {
    unique(guess_type(f))
  }), collapse = ';')
  names(datatypes) <- c('datatypes')
  
  dd <- data.frame(
    id=item$doi,
    license=item$license,
    datatypes=NA,
    title=item$title,
    pubDate=item$published_date,
    keywords=item$categories,
    alternateId=item$id,
    term=NA,
    downloads=get_figshare_metrics(item$id, 'downloads'),
    views=get_figshare_metrics(item$id, 'views'),
    citations=0,
    hasTaxonData=NA,
    hasGeoData=NA,
    hasTemporalData=NA,
    metadataStd="http://www.openarchives.org/OAI/2.0/oai_dc/;http://schema.datacite.org/oai/oai-1.0/;urn:xmlns:org:eurocris:cerif-1.6-2;figshare:oai:qdc;http://www.loc.gov/METS/;http://naca.central.cranfield.ac.uk/ethos-oai/2.0/"
  )
  
  
  if(length(datatypes) > 0) {
    dd$datatypes <- datatypes
  }
  
  metrics <- get_datacite_metrics(c(dd$id))
  dd$downloads <- dd$downloads + metrics$downloads
  dd$views <- dd$views + metrics$views
  dd$citations <- metrics$citations  
  
  Sys.sleep(2)
  return(dd)
}

get_figshare_metrics <- function(id, metric='downloads') {
  con <- crul::HttpClient$new(
    url = 'https://stats.figshare.com', 
    headers = list(
      `Content-Type` = "application/json", 
      Accept = "application/json")
  )
  
  res <- con$get(path=sprintf('/total/%s/article/%s', metric, stringr::str_trim(id)))
  res <- parse_response(res)
  return(res$totals)
}

#'
#' Get datatypes for DataONE datasets files
#'
#' @param oe OtherEntity 
#'
#' @return data.frame with datatypes (mimetypes)
get_dataone_datatypes <- function(obj, formatId) {
  datatypes <- NA
  if (stringr::str_detect(formatId, "eml")) {
    datatypes <- get_eml_datatypes(obj)
  } else if (stringr::str_detect(formatId, "gmd")) {
    datatypes <- get_gmd_datatypes(obj)
  } else if (stringr::str_detect(formatId, "FGDC-STD")) {
    datatypes <- NA
  }
  
  return(datatypes)
}

get_dataone_license <- function (obj, formatId) {
  license <- NA
  if (stringr::str_detect(formatId, "eml")) {
    if (is.list(obj$dataset$intellectualRights)) {
      license=paste(obj$dataset$intellectualRights$para,collapse = ';')
    } else if (!is.null(obj$dataset$intellectualRights)) {
      license=obj$dataset$intellectualRights
    }
  } else if (stringr::str_detect(formatId, "gmd")) {
    license <- NA
  } else if (stringr::str_detect(formatId, "FGDC-STD")) {
    license <- NA
  }
  
  return(license)
}

get_eml_datatypes <- function(eml) {
  oe <- eml$dataset$otherEntity
  if (is.null(oe)) {
    return(NA)
  }
  if ("entityName" %in% names(oe) && "entityType" %in% names(oe)) {
    datatypes <- data.frame(name=oe$entityName,type=oe$entityType)
  } else {
    datatypes  <- plyr::ldply(oe, \(x) data.frame(name=x$entityName,type=x$entityType))
  }
  datatypes <- datatypes %>%
    dplyr::mutate(type=guess_type(name)) %>%
    dplyr::select(type) %>%
    dplyr::summarise(types = paste(unique(type), collapse=";"))
  return(datatypes$types)
}

get_gmd_datatypes <- function(obj) {
  if (is.null(obj$distributionInfo$MD_Distribution$distributor$MD_Distributor$distributorFormat$MD_Format$name)) {
    return(NA)
  }
  
  formats <- stringr::str_split(obj$distributionInfo$MD_Distribution$distributor$MD_Distributor$distributorFormat$MD_Format$name, "\\s*,\\s*")
  return (paste(unique(unlist(formats)), collapse = ';'))
}

#'
#' Get dataset metrics from DataCite
#'
#' @param ids dataset' ids
#'
get_datacite_metrics <- function(dois) {
  plyr::ldply(dois, \(doi) {
    doi_ = stringr::str_remove(doi, "doi:")
    e <- rdatacite::dc_events(doi = doi_)
    if (e$meta$total > 0) {
      cols <- c(downloads = 0,views = 0, citations = 0)
      metrics <- e$data$attributes %>%
        dplyr::filter(targetRelationTypeId %in% c('citations', 'views', 'downloads')) %>%
        dplyr::select(targetRelationTypeId, total) %>%
        dplyr::group_by(targetRelationTypeId) %>%
        dplyr::summarise(total=sum(total)) %>%
        pivot_wider(names_from = targetRelationTypeId, values_from = total) %>%
        dplyr::mutate(id=as.character(doi)) %>%
        tibble::add_column(!!!cols[setdiff(names(cols), names(.))]) %>%
        dplyr::select(id,downloads,views,citations)
      
      if (nrow(metrics) > 0) {
        return(metrics)
      }
    }
    
    return(data.frame(id=as.character(doi),downloads=0,views=0,citations=0))
  })
}

get_dataone_metrics <- function(datasetId) {
  res <- tryCatch(
    httr::GET(paste0('http://logproc-stage-ucsb-1.test.dataone.org/metrics?q={%22metricsPage%22:{%22total%22:0,%22start%22:0,%22count%22:0},%22metrics%22:[%22citations%22,%22downloads%22],%22filterBy%22:[{%22filterType%22:%22dataset%22,%22values%22:[%22', datasetId, '%22],%22interpretAs%22:%22list%22},{%22filterType%22:%22month%22,%22values%22:[%2201/01/2012%22,%2206/06/2023%22],%22interpretAs%22:%22range%22}],%22groupBy%22:[%22dataset%22]}')),
    error=\(e) { get_datacite_metrics(datasetId)})
  
  if (is.data.frame(res)) {
    return(res)
  }
  
  if (httr::status_code(res)==200) {
    d <- httr::content(res)
    citations <- length(d$resultDetails$citations)
    views <- sum(sapply(d$resultDetails$metrics_package_counts,'[[', 1))
    downloads <- sum(sapply(d$resultDetails$metrics_package_counts,'[[', 2))
    return(data.frame(id=datasetId, downloads=downloads, views=views,citations=citations))
  } else {
    # try again
    return(get_dataone_metrics(datasetId))
  }
  
  #d <- jsonlite::fromJSON(paste0('https://logproc-stage-ucsb-1.test.dataone.org/metrics?q={%22metricsPage%22:{%22total%22:0,%22start%22:0,%22count%22:0},%22metrics%22:[%22citations%22,%22downloads%22],%22filterBy%22:[{%22filterType%22:%22dataset%22,%22values%22:[%22', datasetId, '%22],%22interpretAs%22:%22list%22},{%22filterType%22:%22month%22,%22values%22:[%2201/01/2012%22,%2205/06/2023%22],%22interpretAs%22:%22range%22}],%22groupBy%22:[%22dataset%22]}'))
  #d
  return(data.frame(id=datasetId, downloads=0, views=0, citations=0))
}

get_dryad_datapackage_metadata_std <- function(raw) {
  doc <- xmlTreeParse(raw)
  root <- xmlRoot(doc)
  ns <- xmlNamespaceDefinitions(root)
  return (paste(lapply(ns, '[[', 'uri'), collapse = ';'))
}

normalize <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
