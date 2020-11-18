DF <- data.frame(x = seq(1,10), y = rep(c('a','b','c','d','e'),2))
DF
OverlapSplit <- function(x,nsplit=1,overlap=2){
  nrows <- NROW(x)
  nperdf <- ceiling( (nrows + overlap*nsplit) / (nsplit+1) )
  start <- seq(1, nsplit*(nperdf-overlap)+1, by= nperdf-overlap )
  
  if( start[nsplit+1] + nperdf != nrows )
    warning("Returning an incomplete dataframe.")
  
  lapply(start, function(i) x[c(i:(i+nperdf-1)),])
}
OverlapSplit(DF,nsplit=3,overlap=2)


OverlapSplit(DF,nsplit=1,overlap=1)

hashim_OverlapSplit <- function(x,chunk_size=4,overlap_ratio=0.5){
  nrows <- NROW(x)
  nperdf <- chunk_size#ceiling( (nrows + overlap*nsplit) / (nsplit+1) )
  #start <- seq(1, nsplit*(nperdf-overlap)+1, by= nperdf-overlap )
  step_by <- ceiling(chunk_size * overlap_ratio)
  start <- seq(1, chunk_size, by= step_by )
  
  #if( start[nsplit+1] + nperdf != nrows )
  #  warning("Returning an incomplete dataframe.")
  
  #lapply(start, function(i) x[c(i:(i+nperdf-1)),])
  lapply(start, function(i) x[c(i:(i+chunk_size-1)),])
}

hashim_OverlapSplit(DF, chunk_size=4,overlap_ratio=0.5)
