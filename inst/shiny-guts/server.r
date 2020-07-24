
function(input, output, session) {
	
  ### load data
  
  # quit button
  observeEvent(input$quit, {
		stopApp(0)
	})
  
  # load the sattag data dir
  volumes <- c('file system' = '/Users/')
  shinyFiles::shinyDirChoose(input, 'datapath', roots = volumes, session = session)
  output$datadirtext <- renderText(tagdatadir())
  
  tagdatadir <- reactive({
    out <- shinyFiles::parseDirPath(volumes, input$datapath)
    if(length(out) == 0) {
      out <- ""
    } else {
      updateTextInput(session, 'datadirpath', value = out)
      write.table(out, 'lastfile', sep = ',', row.names = FALSE, col.names = FALSE)
    }
    
    out
  })
  
  # upload the tags
  observeEvent(input$upload, {
      tryCatch({
    tags <<- sattagutils::batch_load_tags(input$datadirpath)
      }, error = function(e) {
        warning(e)
      })
    
    tags <<- tags[order(DeployID(tags))]
    taglist <<- c(paste(DeployID(tags), Ptt(tags)))
    # sertags <<- tags[sapply(sattagutils::streamtype(tags), function(x) 'series' %in% x)]
    # behtags <<- tags[sapply(sattagutils::streamtype(tags), function(x) 'behavior' %in% x)]
    
    write.table(input$datadirpath, 'lastfile', sep = ',', row.names = FALSE, col.names = FALSE)
    output$uploadsuccess <- renderText(paste('uploaded', length(tags), 'tags'))
    
    if(!is.null(tags) & length(tags) > 0) {
      calculatestats()
      output$statuscorrupt <- renderTable(statuscorruptdf)
      output$locationsum <- renderTable(locationsdf)
      output$behaviorsum <- renderTable(behaviordf)
      output$seriessum <- renderTable(seriesdf)
      
      output$behtags <- renderUI(selectInput('behtags', 'select a tag:', taglist))
      output$sertags <- renderUI(selectInput('sertags', 'select a tag:', taglist))
      output$healthtags <-  renderUI(selectInput('healthtags', 'select a tag:', taglist))
      output$postags <- renderUI(selectInput('postags', 'select a tag:', taglist))
    }
  })
  
  # add a line to the cee table
  # despite the validate label i don't have any validation at the moment
  # so do it right or suffer the consequences
  observeEvent(input$valcee, {
    label <- input$cee_lab
    start <- input$cee_st
    end <- input$cee_en
    color <- input$cee_col
    st <- sattagutils::date2num(start)
    en <- sattagutils::date2num(end)
    
    if(is.null(ceetable_cur)) {
      ceetable_cur <<- data.frame(label, start, end, color, st, en)
    } else {
      ceetable_cur <<- rbind(ceetable_cur, data.frame(label, start, end, color, st, en))
    }
    
    output$validateresponse <- renderText('i\'m not validating right now so be careful; reload a csv to fix')
    output$ceetab <- renderTable(ceetable_cur)
  })
  
  # load in a csv cee table
  ceetabdf <- observeEvent(input$ceetabinput, {
    ceetabfname <- input$ceetabinput
    ceetabdf_tmp <- NULL

    if(!is.null(ceetabfname)) {
      ceetabdf_tmp <- read.table(ceetabfname$data, header = TRUE, sep = ',', stringsAsFactors = FALSE)
      ceetabdf_tmp[, 'st'] <- sattagutils::date2num(ceetabdf_tmp$start)
      ceetabdf_tmp[, 'en'] <- sattagutils::date2num(ceetabdf_tmp$end)
    }
    
    ceetable_cur <<- ceetabdf_tmp
    output$ceetab <- renderTable(ceetable_cur)
  })
  
  # observe events for the input select menus
  observeEvent(input$behtags, {
    curtag_now <- which(taglist == input$behtags)  
    
    updateSelectInput(session, 'sertags', selected = input$behtags)
    updateSelectInput(session, 'healthtags', selected = input$behtags)
    updateSelectInput(session, 'postags', selected = input$behtags)
  
    output$behplot <- renderPlot(plotbeh(tags[[curtag_now]]))
  })
  
  observeEvent(input$sertags, {
    curtag_now <- which(taglist == input$sertags)  
      
    updateSelectInput(session, 'behtags', selected = input$sertags)
    updateSelectInput(session, 'healthtags', selected = input$sertags)
    updateSelectInput(session, 'postags', selected = input$sertags)
    
    output$serplot <- renderPlot(plotser(tags[[curtag_now]]))
  })
  
  observeEvent(input$healthtags, {
    curtag_now <- which(taglist == input$healthtags)  
      
    updateSelectInput(session, 'behtags', selected = input$healthtags)
    updateSelectInput(session, 'sertags', selected = input$healthtags)
    updateSelectInput(session, 'postags', selected = input$healthtags)
    
    output$healthplot <- renderPlot(plothealth(tags[[curtag_now]]))
  })
  
  observeEvent(input$postags, {
    curtag_now <- which(taglist == input$postags)  
      
    updateSelectInput(session, 'behtags', selected = input$postags)
    updateSelectInput(session, 'sertags', selected = input$postags)
    updateSelectInput(session, 'healthtags', selected = input$postags)
    
    output$leafmap <- renderLeaflet(plotleaf(tags[[curtag_now]], input$qflags))
    output$postable <- renderTable(maketablepos(tags[[curtag_now]], input$qflags))
  })
}
