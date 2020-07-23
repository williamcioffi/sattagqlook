
navbarPage('sattagqlook',
  tabPanel('load data',
    mainPanel(
      actionButton('quit', 'quit'),
      hr(),
      strong('1. select a path to a data dir consisting of one or more tag dirs:'),
      br(),
      fluidRow(
      column(1, shinyFiles::shinyDirButton('datapath', 'data dir', 'Please select a data dir', FALSE)),
      column(8, textOutput('datadirtext'))
      ),
      textInput('datadirpath', '--or-- enter path here:', value = '/abs/path/to/tags'),
      hr(),
      strong('2. upload tags:'),
      fluidRow(
      column(1, actionButton('upload', 'upload')),
      column(8, textOutput('uploadsuccess'))
      ),
      hr(),
      fileInput('ceetabinput', '3. choose highlight file:',
				accept = c('.csv'),
				width = '255'
      ),
      p('indicate one or more periods to highlight (e.g., CEEs). columns should be label, start, end, and color. start and end should be UTC times in the format yyyy-mm-dd HH:MM:SS and color should be an html color without alpha'),
      br(),
      strong('--or-- enter data here:'),
      br(), 
      fluidRow(column(1, actionButton('valcee', 'validate')), column(8, textOutput('validateresponse'))), 
      br(),
      fluidRow(
        column(2, textInput('cee_lab', 'label', value = 'cee1')),
        column(4, textInput('cee_st', 'start', value = 'yyyy-mm-dd HH:MM:SS UTC')),
        column(4, textInput('cee_en', 'end', value = 'yyyy-mm-dd HH:MM:SS UTC')),
        column(2, textInput('cee_col', 'color', value = '#ff0000'))
      ),
      tableOutput('ceetab')
    )
  ),
  tabPanel('summary', mainPanel(
    p('table 1. status and corrupt messages'),
    tableOutput('statuscorrupt'),
    br(), br(),
    p('table 2. location summary'),
    tableOutput('locationsum'),
    br(), br(),
    p('table 3. behavior data and gap summary'),
    tableOutput('behaviorsum'),
    br(), br(),
    p('table 4. series data and gap summary'),
    tableOutput('seriessum')
  )),
  tabPanel('behavior', mainPanel()),
  tabPanel('series', mainPanel()),
  tabPanel('pressure health', mainPanel()),
  tabPanel('positions', mainPanel())
)
