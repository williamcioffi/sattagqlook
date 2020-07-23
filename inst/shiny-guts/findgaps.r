###
# findgaps.r
# find gaps in the behavior data stream

findgaps2 <- function(behavior, tolerance = 60) {
  MAX_ALLOWED_DIFF <- tolerance # in seconds
  
  # make sure times are nuemric
  timeclass <- lapply(behavior[, c('Start', 'End')], mode)
  if(all(timeclass != "numeric")) stop("use numeric times please")

  # index the messages
  # fail if there are no messages
  msg <- behavior$What == "Message"
  if(all(!msg)) stop("missing message rows")
  
  nmsg <- length(msg)
  msgid <- cumsum(msg)
  
  # calculate the difftimes
  st <- behavior$Start[msg]
  en <- behavior$End[msg]
  n  <- length(st)
  
  diffs <- st[2:n] - en[1:(n - 1)]

  # look for gaps above the tolerance
  desegaps <- which(abs(diffs) > MAX_ALLOWED_DIFF)
  ngaps  <- length(desegaps)
  
  # grab some useful metrics
  if(ngaps == 0) {
    stretchid <- rep(1, nrow(behavior))
    gap_st <- NULL
    gap_en <- NULL
  } else {
    gap_st <- en[desegaps]
    gap_en <- st[desegaps + 1]
    
    stretchid <- rep(FALSE, nrow(behavior))
    stretchid[which(msg)[desegaps + 1]] <- TRUE
    stretchid <- cumsum(stretchid) + 1
  }
  
  # return info on gaps (or lack of gaps)
  list(deploy_id = as.character(behavior$DeployID[1]), ngaps = ngaps, gap_st = gap_st, gap_en = gap_en, gap_diffs = diffs, stretchid = stretchid)
}
