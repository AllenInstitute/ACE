# convert all values in the input reactive list to a single string
# in URL format: name1=value1,value2&name2=value1
# NOT currently robust to lists stored in input
#
# This is needed because bookmarking isn't working correctly
# with inputs created by reactive functions
build_storage_string <- function(input, keep_empty = T) {
  vals <- reactiveValuesToList(input)
  
  # excluding lists for now.
  vals <- vals[unlist(lapply(vals, is.null)) == FALSE]
  vals <- lapply(vals,as.character)
  
  vals <- lapply(vals, function(x) paste(x,collapse=","))
  
  vals <- vals[order(names(vals))]
  
  store <- ""
  
  if(!keep_empty) {
    
    empty_vals <- unlist(lapply(vals, function(x) identical(x,"")))
    vals[empty_vals] <- NULL
    
  }
  
  for(i in 1:length(vals)) {
    
    store <- paste0(store, names(vals)[i], "=", gsub("[\t ,]+", ",", vals[[i]]))
    
    if(i < length(vals)) {
      store <- paste0(store, "&")
      
    }
    
  }
  
  store
  
}

# parse the stored values string back into a list that can
# be used onRestore.
parse_storage_string <- function(store) {
  split_string <- strsplit(strsplit(store, "&")[[1]], "=")
  
  vals <- lapply(split_string, function(x) x[2])
  
  names(vals) <- lapply(split_string, function(x) x[1])
  
  vals
}

build_url <- function(session, input) {
  
  url <- paste0(session$clientData$url_protocol,"//",
                session$clientData$url_hostname,
                session$clientData$url_pathname,
                "?")
  
  vals <- reactiveValuesToList(input)
  
  vals <- vals[order(names(vals))]
  
  for(i in 1:length(vals)) {
    url <- paste0(url, names(vals)[i], "=", gsub("[\t ,]+", ",", vals[[i]]))
    if(i < length(vals)) {
      url <- paste0(url, "&")
    }
  }
  
  url
}