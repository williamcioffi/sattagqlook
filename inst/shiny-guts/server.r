### globals
# the tags!
tags <- NULL

# table of cees to highlight in various plots
ceetable_cur <- NULL

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
    }
    
    out
  })
  
  # upload the tags
  observeEvent(input$upload, {
      tryCatch({
    tags <- sattagutils::batch_load_tags(input$datadirpath)
      }, error = function(e) {
        warning(e)
      })
    output$uploadsuccess <- renderText(paste('uploaded', length(tags), 'tags'))
    calculatestats()
    output$statuscorrupt <- renderTable(statuscorruptdf)
    output$locationsum <- renderTable(locationsdf)
    output$behaviorsum <- renderTable(behaviordf)
    output$seriessum <- renderTable(seriesdf)
  })
  
  # add a line to the cee table
  # despite the validate label i don't have any validation at the moment
  # so do it right or suffer the consequences
  observeEvent(input$valcee, {
    label <- input$cee_lab
    start <- input$cee_st
    end <- input$cee_en
    color <- input$cee_col
    
    if(is.null(ceetable_cur)) {
      ceetable_cur <<- data.frame(label, start, end, color)
    } else {
      ceetable_cur <<- rbind(ceetable_cur, data.frame(label, start, end, color))
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
    }
    
    ceetable_cur <<- ceetabdf_tmp
    output$ceetab <- renderTable(ceetable_cur)
  })
  
  ### summary
  

}
