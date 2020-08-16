library(shiny)
library(shinyFiles)
library(sattagutils)
library(leaflet)
source('findgaps.r')
source('plot_dives.r')


# summary tables
statuscorruptdf <- NULL
locationsdf <- NULL
behaviordf <- NULL
seriesdf <- NULL

# the tags!
tags <- NULL
behtags <- NULL
sertags <- NULL
taglist <- NULL
btaglist <- NULL
staglist <- NULL

# table of cees to highlight in various plots
ceetable_cur <- NULL

# last used path
lastusedpath <- read.table("lastfile", header = FALSE, sep = ',', stringsAsFactors = FALSE)[1, 1]

plothighlights <- function() {
  if(!is.null(ceetable_cur)) {
    cols <- paste0(ceetable_cur$color, '3f')
    rect(ceetable_cur$st, -10000, ceetable_cur$en, 10000, col = cols, border = NA)
  }
}

plotbeh <- function(tag) {
  if('behavior' %in% sattagutils::streamtype(tag)) {
    beh <- sattagutils::getstream(tag, 'behavior', squash = TRUE)
    plot_dives2(beh, pch = NA, col = 'black')
  }
  
   plothighlights()
}

plotser <- function(tag) {
  if('series' %in% sattagutils::streamtype(tag)) {
    ser <- sattagutils::getstream(tag, 'series', squash = TRUE)
    st1 <- min(ser$Date)
    en1 <- max(ser$Date)
    
    sattagutils::plot_series(ser, xaxt = 'n', las = 1)
    ax <- sattagutils::dateseq(c(st1, en1))
    ax2 <- sattagutils::dateseq(c(st1, en1), hours = TRUE)
    axis(1, at = ax, lab = format(sattagutils::num2date(ax), "%d-%b"), las = 2)
    axis(1, at = ax2, lab = NA, tcl = -0.15)
    
    plothighlights()
  }
}

plothealth <- function(tag) {
  curtag <- tag
  
  # make an empty vector for cutoffs
  tagends <- NA
  
  # get the deployday and the most recent location time as bounds
  loc <- sattagutils::getstream(curtag, "locations", squash = TRUE)
  # st <- deploytimes[deploymeta$ptt == ptt[i]]
  st <- min(loc$Date)
  en <- max(loc$Date)
  
  # check to see if there are any status messages
  if("status" %in% sattagutils::streamtype(curtag)) {
    # get the status messages and filter for the crc'd
    sta <- sattagutils::getstream(curtag, "status", squash = TRUE)
    sta_crc <- sta[sta$Type == "CRC", ]
    
    # sort because my merging routine does not automatically sort in the gonio data
    sta_crc <- sta_crc[order(sta_crc$Received), ]
    
    # calculate a status cutoff
    # first set the cutoff to the last status message
    tagends <- max(sta_crc$Received)
    
    # look for bad pressure readings
    if(length(which(abs(sta_crc$Depth) > 10)) > 1) {
      cutoff_ind <- min(which(abs(sta_crc$Depth) > 10)) - 1
  
      if(cutoff_ind < 1) {
        warning("this whole record appears to have a bad pressure transducer...")
        tagends <- NA
      } else {
        tagends <- sta_crc$Received[cutoff_ind]
      }
    }
    
    # ok now taker a look
    
    # set up a plotting area
    par(mfrow = c(2, 1), mar = c(0, 5.1, 0, 0), oma = c(5.1, 0, 3.1, 0))
    
    # plot the status messages
    if('original' %in% colnames(sta_crc)) {
      cols <- sta_crc$original
      cols[cols == "gonio"] <- "purple"
      cols[cols == "portal"] <- "black"
    } else {
      cols <- 'black'
    }
    
    plot(sta_crc$Received, sta_crc$Depth, pch = 3, axes = FALSE, xlab = "", ylab = "zero depth (meters)", xlim = c(st, en), ylim = c(-100, 100), col = cols)
    axis(2, las = 1)
    legend("topleft", legend = c("portal", "gonio"), col = c("black", "purple"), pch = c(3, 3), bty = 'n')

    # show the good range
    abline(h = c(-10, 10), lty = 2)
    
    # show the cut off
    abline(v = tagends, lty = 2, col = "red")
    # give it a title label
    mtext(paste(sattagutils::Ptt(curtag), sattagutils::DeployID(curtag), sep = ' / '), side = 3)
    
    # plot behavior or series or both whatever it is
    if("series" %in% sattagutils::streamtype(curtag)) {
      ser <- sattagutils::getstream(curtag, "series", squash = TRUE)
      ser <- ser[order(ser$Date), ]
      sattagutils::plot_series(ser, xlim = c(st, en), xaxt = 'n')
      
      ax <- sattagutils::dateseq(c(st, en))
      axis(1, at = ax, lab = format(sattagutils::num2date(ax), "%d-%b"), las = 2)
      
      abline(v = tagends, lty = 2, col = "red")
      # axis(2, las = 1)
    }
    
    if("behavior" %in% sattagutils::streamtype(curtag)) {
      beh <- sattagutils::getstream(curtag, "behavior", squash = TRUE)
      plot_dives2(beh, start_time = st, end_time = en, pch = NA, col = "black")
      
      abline(v = tagends, lty = 2, col = "red")
    }
    
    if(!any(c("behavior", "series") %in% sattagutils::streamtype(curtag))) {
      plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
    }
  }
}

maketablepos <- function(tag, qflags) {
  loc <- sattagutils::getstream(tag, 'locations', squash = TRUE)
  adat <- data.frame(lon = loc$Longitude, lat = loc$Latitude, qflag = loc$Quality, date = loc$originaldate)
  adat <- adat[rev(order(loc$Date)), ]
  adat[adat$qflag %in% qflags, ]
}

plotleaf <- function(tag, qflags) {
  loc <- sattagutils::getstream(tag, 'locations', squash = TRUE)
  adat <- data.frame(lon = loc$Longitude, lat = loc$Latitude, qflag = loc$Quality, date = loc$originaldate)
  datapoints <- adat[adat$qflag %in% qflags, ]
  
  n <- nrow(datapoints)
  dropback <- 10
  if(n < 4) dropback = 0
  radius <- rep(2, n)
  radius[n] <- 5

  pal <- leaflet::colorFactor(c(rgb(.5, .5, .5), rgb(0, .5, .75), rgb(.5, .5, 1),  rgb(0, 0, 0), rgb(1, 1, 1), rgb(1, 1, 1), rgb(1, 1, 1)), domain = c("Z", "A", "B", "0", "1", "2", "3"))
  
  m <- leaflet()
  m <- addTiles(m, urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}")
  m <- addCircleMarkers(m, data = datapoints[, c('lon', 'lat')],
      popup = paste0(
        "qflag = ", 
        datapoints$qflag, "<br>", 
        datapoints$date, "<br>(", 
        round(datapoints$lat, 4), ", ", 
        round(datapoints$lon, 4), ")"
      ),
      radius = radius,
      color = pal(datapoints$qflag)
  )

  m <- addLegend(m, "bottomright", pal = pal, values = datapoints$qflag, opacity = 1)
  m <- addPolylines(m, lng = datapoints$lon[(n-dropback):n], lat = datapoints$lat[(n-dropback):n], weight = 2, color = "white")
  
  m
}

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
