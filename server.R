library(dplyr)
library(zipcode)
library(tidyr)
library(ggplot2)
library(MASS)
library(ggmap)
library(leaderCluster)
library(scales)
library(tidyquant)
library(shiny)
library(rsconnect)
library(rmarkdown)
library(mosaic)
library(tidyquant)
library(shinydashboard)
library(DT)
library(Cairo)

server <- function(input, output,session) {
	
	Store.DMA.match<-readRDS('Store.DMA.match.Rda')
 
 values <- reactiveValues(df_data = NULL)

  observeEvent(input$file1, {
    values$df_data <- read.csv(input$file1$datapath)
  })
    
    	library(dplyr)
    	 merged_data<-reactive({
    	 	new.df=values$df_data 
    	 	new.df$House.Code <- sub(",","", new.df$House.Code)
    	 
    	 	final.df=merge(new.df,Store.DMA.match,by='House.Code',all=TRUE)
    	 })
    
 data_set2<-reactive({
	
	merged.data2<-merged_data()
merged.data2$House.Code<-as.numeric(merged.data2$House.Code)
merged.data2$Grouping<-as.numeric(merged.data2$Grouping)
merged.data2$Guest.Count<-as.numeric(merged.data2$Guest.Count)
merged.data2$X2016.Guest.Count..53Wk.shift.<-as.numeric(merged.data2$X2016.Guest.Count..53Wk.shift.)
data.set2<-merged.data2%>%mutate(ZIP.Code=clean.zipcodes(ZIP.Code))
data.set2<-data.set2%>%mutate(Location.Type=ifelse(Grouping==1,"Corporate","Franchise"))
data.set2<-data.set2%>%mutate(Location.Type=ifelse(Grouping==3,"No Media",Location.Type))


data.set2<-data.set2%>%mutate(Media.Grouping=ifelse(Grouping==1|Grouping==2,'Received Media','No Media'))
data.set2<-data.set2%>%mutate(Business.Date=mdy(Business.Date))
data.set2<-data.set2%>%rename(Guest.Count.LY=X2016.Guest.Count..53Wk.shift.)
data.set2<-data.set2%>%mutate(Raw.Guest.Count.Vs.LY.Change=Guest.Count-Guest.Count.LY)
data.set2<-data.set2%>%mutate(Daily.Perc.Diff.Yoy=Raw.Guest.Count.Vs.LY.Change/Guest.Count.LY)
data.set2<-data.set2%>%mutate(Daily.Perc.Diff.Yoy.no.na=ifelse(is.infinite(Daily.Perc.Diff.Yoy),NA,Daily.Perc.Diff.Yoy))

data.set2
})
 
Daily_Diff_Data<-reactive({
 	Daily.Diff.Data1<-data_set2()
 week1.full.join3.alt.sum.line<-Daily.Diff.Data1%>%filter(!is.na(Daily.Perc.Diff.Yoy.no.na))%>%group_by(Location.Type,Business.Date)%>%summarize(Average.Daily.Perc.Diff.YoY=mean(Daily.Perc.Diff.Yoy.no.na,.keep_all=TRUE))
 	week1.full.join3.alt.sum.line.spread<-week1.full.join3.alt.sum.line%>%spread(Location.Type,Average.Daily.Perc.Diff.YoY)
	week1.full.join3.alt.sum.line.spread<-week1.full.join3.alt.sum.line.spread%>%mutate(Daily.Corporate.Vs.No.Media.Diff=Corporate-`No Media`)
week1.full.join3.alt.sum.line.spread<-week1.full.join3.alt.sum.line.spread%>%mutate(Daily.Franchise.Vs.No.Media.Diff=Franchise-`No Media`)
   week1.full.join3.alt.sum.line.MNM<-week1.full.join3.alt.sum%>%filter(!is.na(Daily.Perc.Diff.Yoy.no.na))%>%group_by(Media.Grouping,Business.Date)%>%summarize(Average.Daily.Perc.Diff.YoY=mean(Daily.Perc.Diff.Yoy.no.na,.keep_all=TRUE))

	week1.full.join3.alt.sum.line.MNM.spread<-week1.full.join3.alt.sum.line.MNM%>%spread(Media.Grouping,Average.Daily.Perc.Diff.YoY)
week1.full.join3.alt.sum.line.MNM.spread<-week1.full.join3.alt.sum.line.MNM.spread%>%mutate(Daily.Media.Vs.No.Media.Diff=`No Media`-`Received Media`)

Daily.Differences.merge<-merge(week1.full.join3.alt.sum.line.MNM.spread,week1.full.join3.alt.sum.line.spread)
Daily.Differences.merge.select<-Daily.Differences.merge%>%dplyr::select(Business.Date,Daily.Media.Vs.No.Media.Diff,Daily.Corporate.Vs.No.Media.Diff,Daily.Franchise.Vs.No.Media.Diff)	
updateSelectInput(session, "dailydiffvar",choices=list("Media Vs. No Media"="Daily.Media.Vs.No.Media.Diff","Corporate Vs. No Media"="Daily.Corporate.Vs.No.Media.Diff","Franchise Vs. No Media"="Daily.Franchise.Vs.No.Media.Diff",selected="Daily.Media.Vs.No.Media.Diff"))

Daily.Differences.merge.select

})

Data.to.Downlad<-reactive({
  	data.to.dl<-data_set2()
  data.to.dl2<-data.to.dl%>%group_by(Business.Date,Location.Type)%>%dplyr::summarize(Average.daily.perc.diff.yoy=mean(Daily.Perc.Diff.Yoy.no.na,na.rm=TRUE),Average.Daily.Guest.Count=mean(Guest.Count),Average.Daily.Guest.Count.LY=mean(Guest.Count.LY),Avg.Raw.Guest.Count.Vs.LY.Change=mean(Raw.Guest.Count.Vs.LY.Change))
  data.to.dl2<-data.to.dl2%>%gather(key=Measurement,value=Group.Value,Average.daily.perc.diff.yoy:Avg.Raw.Guest.Count.Vs.LY.Change)
})

output$downloadData <- downloadHandler(
    filename = paste(Sys.Date(),'data.csv'),
    content = function(file) {
      write.csv((Data.to.Downlad()), file)
    }
  )


 
  
output$daily.diff.plot<-renderPlot({
	daily.diff.data.for.plot<-as.data.frame(Daily_Diff_Data())
	daily.diff.data.for.plot$Business.Date = as.Date(daily.diff.data.for.plot$Business.Date, format = "%m/%d/%Y")
    	daily.diff.data.for.plot2<-as.data.frame(daily.diff.data.for.plot)
	ggplot(daily.diff.data.for.plot2,aes_string(x='Business.Date',y=input$dailydiffvar))+geom_line(lty=2,color='red')+ggtitle(paste(input$dailydiffvar))+scale_y_continuous(labels=percent)+scale_x_date(labels=date_format("%d-%b"),breaks=date_breaks("2 days"))

})		 

  output$table1 <- DT::renderDataTable({
  data_set2<-	 data_set2()
    datatable(data_set2,options = list(Width=TRUE,scrollX = TRUE,searchHighlight = T))
  })
  
  
 
  #need to make daterange1?#
  output$daterange1 <- renderUI({

    dateRangeInput(inputId='daterange1', 
                   label='Select Date Range to Plot', 
                   min = min(data_set2()$Business.Date), 
                   max = max(data_set2()$Business.Date)
                  )

})
  
  
  output$daterange2 <- renderUI({

    dateRangeInput(inputId='daterange2', 
                   label='Select Date Range to Plot', 
                   min = min(data_set2()$Business.Date), 
                   max = max(data_set2()$Business.Date)
                  )

})
  

 
  	
  

Media.Ts.Plot.for.Export.Func <- function(){
  Media.Ts.plot.data<-data_set2()
  Media.Ts.plot.data2<-Media.Ts.plot.data%>%group_by(Business.Date,Location.Type)%>%dplyr::summarize(Average.daily.perc.diff.yoy=mean(Daily.Perc.Diff.Yoy.no.na,na.rm=TRUE),Average.Daily.Guest.Count=mean(Guest.Count),Average.Daily.Guest.Count.LY=mean(Guest.Count.LY),Avg.Raw.Guest.Count.Vs.LY.Change=mean(Raw.Guest.Count.Vs.LY.Change))
  
  ggplot(Media.Ts.plot.data2,aes(x=Business.Date,y=Average.Daily.Guest.Count,color=Location.Type))+geom_line()+scale_y_continuous(labels=comma)+ggtitle('Average Guest Count,Media Vs No Media Locations with Trendlines (dotted)')+scale_x_date(limits = c(input$daterange1[1], input$daterange1[2]),labels=date_format("%d-%b"),breaks=date_breaks("1 day"))+geom_smooth(method='loess',lty=2,se=FALSE)
  }

output$Media.Ts.Plot <- renderPlot({
  Media.Ts.plot.data<-data_set2()
  Media.Ts.plot.data2<-Media.Ts.plot.data%>%group_by(Business.Date,Location.Type)%>%dplyr::summarize(Average.daily.perc.diff.yoy=mean(Daily.Perc.Diff.Yoy.no.na,na.rm=TRUE),Average.Daily.Guest.Count=mean(Guest.Count),Average.Daily.Guest.Count.LY=mean(Guest.Count.LY),Avg.Raw.Guest.Count.Vs.LY.Change=mean(Raw.Guest.Count.Vs.LY.Change))
  
    ggplot(Media.Ts.plot.data2,aes(x=Business.Date,y=Average.Daily.Guest.Count,color=Location.Type))+geom_line()+scale_y_continuous(labels=comma)+ggtitle('Average Guest Count,Media Vs No Media Locations with Trendlines (dotted)')+scale_x_date(limits = c(input$daterange1[1], input$daterange1[2]),labels=date_format("%d-%b"),breaks=date_breaks("1 day"))+geom_smooth(method='loess',lty=2,se=FALSE)
  })


  

output$downloadPlot <- downloadHandler(
  filename = function() { paste("Media.TS.Plot-", Sys.Date(), ".png", sep="") },
  content = function(file1) {
    CairoPNG(file1,width = 680,height=680)
    print(Media.Ts.Plot.for.Export.Func())
    dev.off()
  })      


  
  
  Data.to.Downlad<-reactive({
  	data.to.dl<-data_set2()
  data.to.dl2<-data.to.dl%>%group_by(Business.Date,Location.Type)%>%dplyr::summarize(Average.daily.perc.diff.yoy=mean(Daily.Perc.Diff.Yoy.no.na,na.rm=TRUE),Average.Daily.Guest.Count=mean(Guest.Count),Average.Daily.Guest.Count.LY=mean(Guest.Count.LY),Avg.Raw.Guest.Count.Vs.LY.Change=mean(Raw.Guest.Count.Vs.LY.Change))
  data.to.dl2<-data.to.dl2%>%gather(key=Measurement,value=Group.Value,Average.daily.perc.diff.yoy:Avg.Raw.Guest.Count.Vs.LY.Change)
  
  data.to.dl2.spread<-data.to.dl2%>%spread(Business.Date,Group.Value)
  })
  

    

  
  
  
  output$MAPlot <- renderPlot({
  MaPlotData<-data_set2()
  MaPlotData.sum<-MaPlotData%>%group_by(Business.Date,Location.Type)%>%mutate(Average.Guest.Count=mean(Guest.Count))
MaPlotData.sum<-MaPlotData.sum%>%group_by(Business.Date,Location.Type)%>%distinct(Average.Guest.Count,.keep_all=TRUE)
 ggplot(MaPlotData.sum,aes(x=Business.Date,y=Daily.Perc.Diff.Yoy,color=Location.Type))+geom_point()+geom_ma(ma_fu=SMA,n=3,size=1,lty=2,color="red")+facet_wrap(~Location.Type,scales="free")+scale_y_continuous(labels = percent)+scale_x_date(labels=date_format("%d-%b"),breaks=date_breaks("2 days"))+ggtitle('Average Daily Yoy Perc Diff, with 3 day  Moving Average (dashed lines')+scale_color_tq()+theme_tq()
  })
  
  
  daterange2Input<-reactive({
  		Bar.Plot.Data1<-data_set2()
  	Bar.Plot.Data2<-subset(Bar.Plot.Data1,Business.Date>=input$daterange2[1]& Business.Date<=input$daterange2[2])
  #new#
  		#Bar.Plot.Data3<-Bar.Plot.Data2%>%dplyr::select(Business.Date,Daily.Perc.Diff.Yoy,Location.Type,Media.Grouping)
  		
	#Bar.Plot.Data3<-Bar.Plot.Data3%>%gather(Group.to.Use,Group.Daily.Perc.Diff.Value,Location.Type:Media.Grouping)
  
  	Bar.Plot.Data2a<-Bar.Plot.Data2	%>%dplyr::select(Business.Date,Location.Type,Media.Grouping,Daily.Perc.Diff.Yoy)%>%dplyr::filter(!is.na(Daily.Perc.Diff.Yoy),!is.infinite(Daily.Perc.Diff.Yoy))

Bar.Plot.Data2a.sum<-Bar.Plot.Data2a%>%group_by(Location.Type,Media.Grouping)%>%summarize(Average.Total.Perc.Diff.YoY=mean(Daily.Perc.Diff.Yoy))
#Bar.Plot.Data2a.sum<-as.data.frame(Bar.Plot.Data2a.sum)

updateSelectInput(session,'groupforbar',choices=list("All Location Types"="Location.Type","Media Vs No Media"="Media.Grouping",selected="Location.Type"))
  		return(Bar.Plot.Data2a.sum)
  })
  

 
  
  
  output$Bar <- renderPlot({
 Bar.Chart.Data<-as.data.frame(daterange2Input())
	ggplot(Bar.Chart.Data,aes_string(x=input$groupforbar,y='Average.Total.Perc.Diff.YoY',fill=input$groupforbar))+geom_bar(stat="identity",position="dodge")+scale_y_continuous(labels=percent)+ggtitle('Average YoY Percentage Difference by Location Type')+geom_text(aes(label=sprintf("%0.2f", round(Average.Total.Perc.Diff.YoY, digits = 2))),nudge_y=0.15)
  })
  
  
  Bar.Data.for.Export <- function(){
  
  Bar.Chart.Data<-as.data.frame(daterange2Input())
	ggplot(Bar.Chart.Data,aes_string(x=input$groupforbar,y='Average.Total.Perc.Diff.YoY',fill=input$groupforbar))+geom_bar(stat="identity",position="dodge")+scale_y_continuous(labels=percent)+ggtitle('Average YoY Percentage Difference by Location Type')+geom_text(aes(label=sprintf("%0.2f", round(Average.Total.Perc.Diff.YoY, digits = 2))),nudge_y=0.15)
  }
  
  
output$downloadBarChart <- downloadHandler(
  filename = function() { paste("BarChart-", Sys.Date(), ".png", sep="") },
  content = function(file1) {
    CairoPNG(file1)
    print(Bar.Data.for.Export())
    dev.off()
  }) 

daily.diff.plot.for.export<-function(){
	daily.diff.data.for.plot<-as.data.frame(Daily_Diff_Data())
	daily.diff.data.for.plot$Business.Date = as.Date(daily.diff.data.for.plot$Business.Date, format = "%m/%d/%Y")
    	daily.diff.data.for.plot2<-as.data.frame(daily.diff.data.for.plot)
	ggplot(daily.diff.data.for.plot2,aes_string(x='Business.Date',y=input$dailydiffvar))+geom_line(lty=2,color='red')+ggtitle(paste(input$dailydiffvar))+scale_y_continuous(labels=percent)+scale_x_date(labels=date_format("%d-%b"),breaks=date_breaks("2 days"))
}

output$download.daily.diff <- downloadHandler(
  filename = function() { paste("Daily.Diff-", Sys.Date(), ".png", sep="") },
  content = function(file1) {
    CairoPNG(file1)
    print(daily.diff.plot.for.export())
    dev.off()
  }) 



Value.Box.Data<-reactive({
num.greater.last.year<-data_set2()%>%mutate(Week=week(Business.Date))
num.greater.last.year<-num.greater.last.year%>%mutate(Greater.Than.7.Percent.Increase.Yoy=ifelse(Daily.Perc.Diff.Yoy.no.na>=0.07,'TRUE','FALSE'))
num.greater.last.year.sum<-num.greater.last.year%>%filter(Week==max(Week))%>%group_by(Location.Type,Greater.Than.7.Percent.Increase.Yoy)%>%summarize(Number.of.stores.in.group=n())
num.greater.last.year.sum2<-num.greater.last.year.sum%>%group_by(Location.Type)%>%mutate(Total.Stores=sum(Number.of.stores.in.group),Percent.over.7.Percent.Increase=Number.of.stores.in.group/Total.Stores)
num.greater.last.year.sum3<-num.greater.last.year.sum2%>%filter(Greater.Than.7.Percent.Increase.Yoy=='TRUE')%>%group_by(Location.Type)%>%summarize(Percent.of.Stores.this.Week.Greater.than.7.Perc=Number.of.stores.in.group/Total.Stores)
num.greater.last.year.sum3
})


output$progressBox1 <- renderValueBox({
	col.to.use<-as.data.frame(Value.Box.Data())
   col.to.use<-as.character(round(col.to.use[1,2],2))
	 valueBox(
      paste0(col.to.use, "%"), "Percent of Corporate stores that saw 7% increase that week vs. last year", icon = icon("list"),
      color = "purple")
})
	
	output$progressBox2 <- renderValueBox({
		col.to.use<-as.data.frame(Value.Box.Data())
   col.to.use<-as.character(round(col.to.use[2,2],2))
    valueBox(
      paste0(col.to.use, "%"), "Percent of Franchise stores that saw 7% increase that week vs. last year", icon = icon("bullseye"),
      color = "green"
    )
  })


	output$progressBox3 <- renderValueBox({
		col.to.use<-as.data.frame(Value.Box.Data())
   col.to.use<-as.character(round(col.to.use[3,2],2))
    valueBox(
      paste0(col.to.use, "%"), "Percent of Non-Media stores that saw 7% increase that week vs. last year", icon = icon("bullseye"),
      color = "red"
    )
  })
	
}
