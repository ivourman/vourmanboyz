library(shinythemes)
library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(zipcode)
library(tidyr)
library(ggplot2)
library(MASS)
library(ggmap)
library(scales)
library(tidyquant)
library(shiny)
library(rsconnect)
library(rmarkdown)
library(Cairo)


ui <- dashboardPage(skin="red",
  dashboardHeader(title="Ruby Tuesday's Dashboard"),
  dashboardSidebar(id="",width=200,
  								 sidebarMenu(width=200,
  	menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard")),
    		fileInput(buttonLabel=icon("upload"),'file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )),
  	menuItem("Plots",tabName = "plots",icon=icon("th")),
  		menuItem("Plots Continued",tabName = "plotscontinued",icon=icon("bar-chart"))
  								 )
  	),
  	
  
  dashboardBody(
  	tabItems(
  	#First tab content#	
  		tabItem(tabName="dashboard",
  						fluidRow(
  					valueBoxOutput("progressBox1"),
  					valueBoxOutput("progressBox2"),
valueBoxOutput("progressBox3"),
    	box(DT::dataTableOutput("table1",width="150%")
    				 
   

   ))),
  
   tabItem(tabName="plots",
    fluidRow(
    	box(width=12,title="Daily Guest Count Performance",
    			plotOutput("Media.Ts.Plot",width=NULL),
    downloadButton('downloadPlot','DownloadPlot'))),
    fluidRow(
    	column(width=8,
    	box(title="Set Date Range for Plot",
    		uiOutput("daterange1"),br(),
    		downloadLink("downloadData","Export Data in CSV",icon=icon("download")))
    	)
  	),
    	
    			fluidRow(
    		column(width=12,
    		
    		box(plotOutput("daily.diff.plot",width=NULL)),
    		box(downloadButton('download.daily.diff','Download Daily Difference Plot',icon=icon("download")))
    		
    		)
    		),
    
   fluidRow(
    		column(width=12,
    		box(title ="YoY Daily Differences by Group",status="warning",
    				selectInput("dailydiffvar",label="Choose Variable to Plot",choices=list("Media Vs. No Media"="Daily.Media.Vs.No.Media.Diff","Corporate Vs. No Media"="Daily.Corporate.Vs.No.Media.Diff","Franchise Vs. No Media"="Daily.Franchise.Vs.No.Media.Diff",selected="Daily.Media.Vs.No.Media.Diff")))
    		)
    		)
   ),
  
  
  
  
  	tabItem(tabName="plotscontinued",
  	
  	 fluidRow(
    	
    	box(plotOutput("MAPlot",width="150%"))),
    	fluidRow(
    		column(width=12,
    	box(plotOutput("Bar",width="150%"))),
    		
    	box(
  title = "Grouping and Date Range Selection for Chart",color="yellow",
  uiOutput("daterange2"),
  selectInput(inputId='groupforbar',label="Choose Location Type for Plot",choices=list("All Location Types"="Location.Type","Media Vs No Media"="Media.Grouping",selected="Location.Type")),
  downloadButton('downloadBarChart','Download Bar Chart'))
    		
 	
  )
  	 )
   )
  	)
)
  