library(sattagutils)
source('findgaps.r')
source('plot_dives.r')


# summary tables
statuscorruptdf <- NULL
locationsdf <- NULL
behaviordf <- NULL
seriesdf <- NULL


# helper functions
calculatestats <- function() {
  ntags <- length(tags)
  deployid <- character(length = ntags)
  ptt <- character(length = ntags)
  # cutoff <- numeric(length = ntags)

  # from status
  nmsg <- numeric(length = ntags)
  ncrc <- numeric(length = ntags)
  transmits <- numeric(length = ntags)
  battvoltrange <- character(length = ntags)
  zerodepthrange <- character(length = ntags)
  last_sta_recieved <- character(length = ntags)

  # from corrupt
  ncorrupt <- numeric(length = ntags)

  # from locations
  nloc <- numeric(length = ntags)
  nz <- numeric(length = ntags)
  na <- numeric(length = ntags)
  nb <- numeric(length = ntags)
  n0 <- numeric(length = ntags)
  n1 <- numeric(length = ntags)
  n2 <- numeric(length = ntags)
  n3 <- numeric(length = ntags)
  last_loc_recieved <- character(length = ntags)

  # from behavior
  n_beh_msgs <- numeric(length = ntags)
  totalbeh_time_hours <- numeric(length = ntags)
  nbehgaps <- numeric(length = ntags)
  behgap_time_hours <- numeric(length = ntags)
  latest_beh_recieved <- character(length = ntags)

  # from series
  n_ser_msgs <- numeric(length = ntags)
  totalser_time_hours <- numeric(length = ntags)
  nsergaps <- numeric(length = ntags)
  nsermsggaps <- numeric(length = ntags)
  sergap_time_hours <- numeric(length = ntags)
  latest_ser_recieved <- character(length = ntags)

  for(i in 1:ntags) {
    tcur <- tags[[i]]
    deployid[i] <- DeployID(tcur)
    ptt[i] <- Ptt(tcur)
    # cutoff[i] <- deploytimes[deploymeta$ptt == ptt[i]]
    
    if("status" %in% streamtype(tcur)) {
      sta <- getstream(tcur, "status", squash = TRUE)
      # sta <- sta[sta$Received > cutoff[i], ]
      sta <- sta[order(sta$Received), ]
      
      nmsg[i] <- nrow(sta)
      ncrc[i] <- length(which(sta$Type == "CRC"))
      
      sta_crc <- sta[sta$Type == "CRC", ]
      trans_tmp <- sta_crc$Transmits
      trans_tmp <- trans_tmp[!is.na(trans_tmp)]
      if(length(trans_tmp) == 0) trans_tmp <- NA
      
      transmits[i] <- trans_tmp[length(trans_tmp)]
      battvoltrange[i] <- paste(range(sta_crc$BattVoltage, na.rm = TRUE), collapse = " to ")
      zerodepthrange[i] <- paste(range(sta_crc$Depth, na.rm = TRUE), collapse = " to ")
      last_sta_recieved[i] <- as.character(num2date(sta$Received[nrow(sta)]))
    }
    
    if("corrupt" %in% streamtype(tcur)) {
      cor <- getstream(tcur, "corrupt", squash = TRUE)
      # cor <- cor[cor$Date > cutoff[i], ]
      ncorrupt[i] <- nrow(cor)
    }
    
    if("locations" %in% streamtype(tcur)) {
      loc <- getstream(tcur, "locations", squash = TRUE)
      # loc <- loc[loc$Date > cutoff[i], ]
      
      nloc[i] <- nrow(loc)
      nz[i] <- length(which(loc$Quality == "Z"))
      na[i] <- length(which(loc$Quality == "A"))
      nb[i] <- length(which(loc$Quality == "B"))
      n0[i] <- length(which(loc$Quality == "0"))
      n1[i] <- length(which(loc$Quality == "1"))
      n2[i] <- length(which(loc$Quality == "2"))
      n3[i] <- length(which(loc$Quality == "3"))
      last_loc_recieved[i] <- as.character(num2date(max(loc$Date)))
    }
    
    if("behavior" %in% streamtype(tcur)) {
      beh <- getstream(tcur, "behavior", squash = TRUE)
      # beh <- beh[beh$Start > cutoff[i], ]
      
      msgs <- beh[beh$What == "Message", ]
      n_beh_msgs[i] <- nrow(msgs)
      totalbeh_time_hours[i] <- sum(msgs$End - msgs$Start) / 60 / 60
     
      gaps <- findgaps2(beh[order(beh$Start), ])
      nbehgaps[i] <- gaps$ngaps
      behgap_time_hours[i] <- sum(gaps$gap_en - gaps$gap_st) / 60 / 60
      
      latest_beh_recieved[i] <- as.character(num2date(msgs$End[nrow(msgs)]))
    }
    
    if("seriesrange" %in% streamtype(tcur)) {
      ser <- getstream(tcur, "seriesrange", squash = TRUE)
      # ser <- ser[ser$Start > cutoff[i], ]
      ser <- ser[order(ser$Start), ]
      
      n_ser_msgs[i] <- nrow(ser)
      
      if(n_ser_msgs[i] > 0) {
        totalser_time_hours[i] <- sum(ser$End - ser$Start) / 60 / 60
        gaps <- abs(ser$Start[2:nrow(ser)] - ser$End[1:(nrow(ser) - 1)]) > 60
        nsergaps[i] <- length(which(gaps))
        sergap_time_hours[i] <- sum(abs(ser$Start[2:nrow(ser)] - ser$End[1:(nrow(ser) - 1)])[gaps]) / 60 / 60
        nsermsggaps[i] <- sergap_time_hours[i]/4
        latest_ser_recieved[i] <- as.character(num2date(max(ser$End)))
      }
    }
   }
  
  # make up the dataframes for soummaries
  statuscorruptdf <<- data.frame(deployid, ptt, nmsg, ncrc, transmits, battvoltrange, zerodepthrange, last_sta_recieved, ncorrupt)
  locationsdf <<- data.frame(deployid, ptt, nloc, nz, na, nb, n0, n1, n2, n3, last_loc_recieved)
  behaviordf <<- data.frame(deployid, ptt, n_beh_msgs, totalbeh_time_hours, nbehgaps, behgap_time_hours, latest_beh_recieved) 
  seriesdf <<- data.frame(deployid, ptt, n_ser_msgs, totalser_time_hours, nsergaps, sergap_time_hours, nsermsggaps, latest_ser_recieved)
}
