DF <- data.frame(x = seq(1,150), y = rep(c('a','b','c','d','e'),30))
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
  #nperdf <- chunk_size#ceiling( (nrows + overlap*nsplit) / (nsplit+1) )
  #start <- seq(1, nsplit*(nperdf-overlap)+1, by= nperdf-overlap )
  step_by <- ceiling(chunk_size * overlap_ratio)
  to_row <- chunk_size * floor((nrows)/(chunk_size+1))
  start <- seq(1, to_row, by= step_by )
  
  #if( start[nsplit+1] + nperdf != nrows )
  #  warning("Returning an incomplete dataframe.")
  
  #lapply(start, function(i) x[c(i:(i+nperdf-1)),])
  return(lapply(start, function(i) x[c(i:(i+chunk_size-1)),]))
}

list_df_splitted <- hashim_OverlapSplit(DF, chunk_size=100,overlap_ratio=0.5)
list_df_splitted
#typeof(list_df_splitted)
#for(df in list_df_splitted){
#  if (sum(is.na(df))>0){
#    print(df)
#  }
#}
#lapply(list_df_splitted, function(df) sum(is.na(df))==0)
seq(1,10, by=2)
15*floor(150/15)
