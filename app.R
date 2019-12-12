library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(tigris)
library(zoo)
library(readr)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(stringr)
library(tidyverse)
library(GGally)
library(parcoords)
library(r2d3)
state=states(cb=T)

df_violence_sum <- read_csv("dataset/gun_violence_sum.csv")
df_ABBstate <- read_csv("dataset/ABBstate.csv")
df_population <- read_csv("dataset/clean_population_by_state.csv")


main_map_plotter <- function(input_year,data_new,bind_data,population){
    data_new <- filter(data_new, year == input_year)
    population <- filter(population, year == input_year)
    
    data_new <- merge(data_new,bind_data,by="state")
    data_new <- merge(data_new,population,by="state")
    data_new$rate <- data_new$total_TOLL/data_new$population*100000
    
    states_merged_value <- geo_join(state, data_new, "STUSPS", "ABBstate")
    
    bins <- c(0, 2, 4, 6,8,10,15,Inf)
    pal <- colorBin("YlOrRd", domain=states_merged_value$rate,bins = bins)
    
    state_popup <- paste0("<strong>State: </strong>",
                          states_merged_value$state,
                          "<br><strong>Causualties per 100,000 people </strong>",
                          round(states_merged_value$rate,2)) %>% lapply(htmltools::HTML)
    
    leaflet(states_merged_value)%>%
      addTiles()%>%
      setView( -100, 45, zoom = 3 ) %>%
      addPolygons(
        #data = states_merged_value, Don't use this, can't create legend in this way
        layerId=states_merged_value$state,
        fillColor = ~pal(rate),
        fillOpacity = 0.5,
        weight = 1, 
        color = "white",
        label = state_popup,
        highlight=highlightOptions(weight=10, color="grey",bringToFront=TRUE)
      )%>%
      addLegend(pal = pal, values = ~rate, na.label = "Unknown",
                opacity = 0.5 , title = "Casualties per 100k people",
                position = "bottomleft")
}

selected_state_plotter <- function(proxy,data_new,input_state,input_year,population){
  proxy  %>% clearPopups()
  
 # print(class(input_year))
  state_data <- filter(data_new,state == input_state$state,year == input_year )
  population <- filter(population, year == input_year, state == input_state$state)
  #print(state_data["total_TOLL"]/population["population"]*100000)
  content <- paste0("<strong>State: </strong>",
                        input_state$state,
                        "<br><strong>Casualties per 100,000 people </strong>",
                        round(state_data["total_TOLL"]/population["population"]*100000,2)
                        )
  proxy %>% addPopups(input_state$lng, input_state$lat, content)
}


# heat_map_plotter <- function(input_state,input_year,data){
#   
#   
#   data_new <- data %>%
#     filter(state==input_state) #select state
#   data_new$date <- as.character(data_new$date)
#   data_new$date<-as.Date(data_new$date)
#   data_new["year"]=str_sub( as.character(data_new$date),1,4)
#   data_new <- data_new %>%
#     filter(year==input_year)
#   qmplot(lon,lat,data=data_new,colour=I("red"),zoom=8,size = I(0.8),maptype="toner",alpha=0.3,
#          main= paste0("Gun Violence Spread in ",input_state))+
#     theme(legend.position="none")
# }

###########################
## heatmap transform data
gun_vio_heat=read.csv("dataset/clean gun-violence-data_2013-3-2018-3_new.csv")
gun_vio_heat["year"]=str_sub( as.character(gun_vio_heat$date),1,4)
gun_vio_heat <- gun_vio_heat%>%filter(longitude<0) # remove some inaccurate records

# get us state map
us_states <- map_data("state")


#############################
## function to plot heatmap
heat_map_plotter <- function(input_state,input_year){
  violence_clean <-gun_vio_heat %>%
    filter(state==input_state & year==input_year)
  print(head(violence_clean))
  
  state_clean <- us_states %>%
    filter(region==tolower(input_state))
  
  ggplot()+
    geom_polygon(data=state_clean, aes(x=long,y=lat,group=group),fill="#FFEDA0",color="grey")+
    geom_point(data=violence_clean,aes(x=longitude,y=latitude),color="red",size=0.5)+
    theme_bw()+
    ggtitle(paste("Gun Violence Spread in",input_state, input_year))
}
#heat_map_plotter("Florida",2017)

###########################
## pre data processing for timeseries+pcp
gun_vio=read.csv("dataset/clean gun-violence-data_2013-3-2018-3.csv")
#gun_vio=read.csv("dataset/clean_gun_violence_2013.3-2018.3.csv")
gun_vio["year"]=str_sub( as.character(gun_vio$date),1,4)
gun_vio<-gun_vio[which(as.numeric(as.character(gun_vio$year))>2013)  ,]
back_check=read.csv("dataset/nics-firearm-background-checks copy.csv")
GDP<-read.csv("dataset/GDP_states.csv")
GDP_sub<-GDP %>% select(-c(1,2,4,5,6,7,8,9))

population_tp=read.csv("dataset/clean_population_by_state.csv")
population<-select(population_tp,2,3,4)

gun_vio$date<-as.Date(gun_vio$date)
gun_vio$state<-as.character(gun_vio$state)

back_check$month<-as.character(back_check$month)
back_check["year"]=str_sub(back_check$month,1,4)

gun_vio["year"]<-format(as.Date(gun_vio$date, format="%d/%m/%Y"),"%Y")
gun_vio["month"]<-format(as.Date(gun_vio$date, format="%d/%m/%Y"),"%m")


###########################
## timeseris transform data
gun_vio$date<-as.Date(gun_vio$date)
gun_vio_tp<-subset(gun_vio,select=c("date","state","n_killed","n_injured","month","year"))

# population_sub<-population[which(population$State==state),]
# gun_vio_tp$n_killed<-as.numeric(as.character(gun_vio_tp$n_killed))
# gun_vio_tp$n_injured<-as.numeric(as.character(gun_vio_tp$n_injured))
# gun_vio_tp["n_sum"]<-gun_vio_tp$n_killed+gun_vio_tp$n_injured
gun_vio_month<-gun_vio_tp%>%
  group_by(month,year,state)%>%
  summarize(incident_sum=n())
gun_vio_month<-select(gun_vio_month,1,2,3,4)
gun_vio_month<-as.data.frame(gun_vio_month)
gun_vio_month["date_by_month"]=     str_c(as.character(gun_vio_month$year),"-",as.character(gun_vio_month$month),"-15")
gun_vio_month$date_by_month<-as.Date( gun_vio_month$date_by_month)

#############################
## function to plot time series
TimePlot<-function(state)
{
  gun_vio_month_state<-gun_vio_month[which(gun_vio_month$state==state),]
  # population_sub<-population[which(population$state==state),]
  # 
  # gun_vio_avg=merge(gun_vio_month_state,population_sub,by.x="year",by.y="year")
  # gun_vio_avg["gun_violence_avg"]<-gun_vio_avg$n_sum/(gun_vio_avg$population*1.0)*1000000.0
  # 
  ggplot(gun_vio_month_state)+
    geom_line(aes(x=date_by_month,y=incident_sum),color="#B10026")+
    ggtitle(paste("Number of Gun Violence Incidents \nfrom 01/2014 to 03/2018,",state))+
    labs(x="",y="number of incidents")+
    theme_minimal()
}

###########################
## function to plot ranking of each year
Ranking<-function(year,state)
{
  year<-as.character(year)
  population$year<-as.character(population$year)
  population_sub<-population[which(population$year==year),]
  gun_vio_n_sub<-df_violence_sum[which(df_violence_sum$year==year),]
  
  gun_vio_n_adjust = merge(gun_vio_n_sub,population_sub, by.x="state", by.y="state")
  gun_vio_n_adjust['Toll_avg']<-gun_vio_n_adjust$total_TOLL/gun_vio_n_adjust$population*100000
  gun_vio_n_adjust_sub <-gun_vio_n_adjust[order(-gun_vio_n_adjust$Toll_avg),]
  gun_vio_n_adjust_top10<-gun_vio_n_adjust_sub[1:10,]
  
  colorset=c()
  for (i in 1 : 10){
    if (gun_vio_n_adjust_top10$state[i]==state){
      colorset=append(colorset,"b")
    } else {
      colorset=append(colorset,"a")
    }
    
  }
  gun_vio_n_adjust_top10_2 <- cbind(gun_vio_n_adjust_top10,colorset)
  ggplot(gun_vio_n_adjust_top10_2 ,aes(x=fct_reorder(state,Toll_avg),y=Toll_avg,fill=colorset))+
    geom_bar(position = "dodge", stat = "identity")+
    coord_flip()+
    ggtitle(paste0("Top 10 States with Highest Number \n of Casualties in ",year))+
    scale_fill_manual(values=c("#FEB24C","#E31A1C"))+
    theme(legend.position = "none")+
    labs(x="",y="Casualties per 100k people")
    #theme_minimal()
}


###########################
## function to plot pcp
Parcoord<-function(year)
{
  year<-as.character(year)
  population_sub<-population[which(population$year==year),]
  back_check_state_year<-back_check_state[which(back_check_state$year==year),]
  back_check_state_year_avg<-merge(population_sub,back_check_state_year,by.x="state",by.y="state")
  
  back_check_state_year_avg["checking_avg"]<-back_check_state_year_avg$total_year/back_check_state_year_avg$population*100000.0
  
  gun_vio_state_year<-gun_vio_state[which(gun_vio_state$year==year),]
  gun_vio_state_year_avg<-merge(gun_vio_state_year,population_sub,by.x="state",by.y="state")
  gun_vio_state_year_avg["gun_violence_avg"]<-gun_vio_state_year_avg$n_sum_state/(gun_vio_state_year_avg$population*1.0)*1000000.0
  gun_vio_state_year_avg<-select(gun_vio_state_year_avg,1,6)
  check_vio<-merge(back_check_state_year_avg, gun_vio_state_year_avg, by.x="state", by.y="state")
  GDP_year<-subset(GDP,select=c("GeoName",str_c("X",year)))
  
  colnames(GDP_year)<-c("state","GDP_total")
  
  GDP_avg=merge(GDP_year,population_sub,by.x="state",by.y="state")
  
  GDP_avg["GDP_avg"]=GDP_avg$GDP_total*1000000/GDP_avg$population
  
  check_vio_gdp<-merge(check_vio, GDP_avg, by.x="state", by.y="state")
  check_vio_gdp<-select(check_vio_gdp,1,2,6,7,11)
  
  
  colnames(check_vio_gdp)<-c("state","year","checking_avg","gun_violence_avg","GDP_avg")
  
  parcoords(check_vio_gdp[c("state","checking_avg","gun_violence_avg","GDP_avg")],rownames = FALSE,reorder = TRUE, brushMode="1D",alpha = .3, color = list(colorScale = "scaleOrdinal",colorBy = "state", colorScheme = "schemeCategory10"), withD3 = TRUE)
  # ggpairs(check_vio_uem[c("checking_totals","gun_violence_death","GDP_avg")])
}

###########################
## gun violence type transform data
gun_vio_character<-select(gun_vio,1,2,5,6,7,8,9)


###########################
## function to plot key gun violence type
barCharacter<-function(year,state)
{
  year<-as.character(year)
  gun_vio_year<-gun_vio_character[which(  gun_vio_character$year==year & gun_vio_character$state==state),]
  gun_vio_year<-select(gun_vio_year,-1,-2,-7)
  gun_vio_year_sum<-colSums(gun_vio_year)
  gun_vio_year_sum_df_tp<-as.data.frame(as.list(gun_vio_year_sum))
  
  # col name is too long, add line break in it
  for (i in 1:ncol(gun_vio_year_sum_df_tp)){
    colnames(gun_vio_year_sum_df_tp)[i]= gsub("\\.","\n",colnames(gun_vio_year_sum_df_tp)[i])
    
  }
  
  gun_vio_year_sum_df<-gather(gun_vio_year_sum_df_tp)
  
  gun_vio_year_sum_df%>%
    ggplot(aes(x= fct_reorder(key, value),y=value))+
    geom_bar(position = "dodge", stat = "identity",fill = "#FC4E2A",width = 0.8)+
    geom_text(aes(x= fct_reorder(key, value),y=value+0.1*value,label=value))+
    ggtitle(paste0("Key Types of Gun Violence Incidents\n",state,", ",year))+
    labs(x="",y="Number of incidents")+
    theme_minimal()
  
}
# barCharacter(2016,"New York")

###########################
## shiny begins

ui = fluidPage(
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput("yearSlider",
  #                 "Please select a year",
  #                 min = 2014,
  #                 max = 2018,
  #                 value = 2018,
  #                 step = 1)
  #     
  #    
  #   ),
  #   mainPanel(
  #     leafletOutput("map"),
  #     plotOutput('heatmap'),
  #     plotOutput("timeseries"),
  #     parcoordsOutput("pcp")
  #   )
  # )
  tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  leafletOutput("map", width="100%", height="700"),
  
  absolutePanel(draggable = TRUE, top = 10, left = 20, right = "auto", bottom = "auto",
                width = 330, height = "auto",
                
                h2("Year explorer",align = "center"),
                h5("Please select an year to explore the number of casualties caused by gun violence in each state:"),
                selectInput("yearSlider","",
                            choices=2018:2014),
                plotOutput("ranking",height = 350,width =320),
                style = "opacity: 0.8"
                #parcoordsOutput("pcp",height = 200,width =300)
                
               
  ),
  
  absolutePanel(fixed = TRUE,
                draggable = TRUE, top = 10, left = "auto" , right = 20, bottom = "auto",
                width = 330,
                
                h2("State explorer",align = "center"),
                h5("Please click on the map to select a state. Below are its incidents trend, location and type:"),
                
                wellPanel( width = 300,
                plotOutput("timeseries", height = 200,width =300),
                plotOutput("heatmap", height = 200, width = 300),
                plotOutput("typeBar",height= 300, width = 300),
                style = "overflow-y:scroll; max-height: 550px"
                ),
                style = "opacity: 0.8"
  ),
  
  absolutePanel(fixed=TRUE,
                p("Notes: We only have the first quarter violence data for 2018, so the number of casualties in 2018 is smaller than other years."),
                helpText(a("Back to Reports", href="https://edav-project.github.io/report/interactive-component.html"))
  )
  
  #textOutput("temp"，),
  
)

server = function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    main_map_plotter(input$yearSlider,df_violence_sum,df_ABBstate,df_population)
  })
  
  #heatmap <- reactiveValues(state = "New York",year = 2018)
  heatmap <- reactiveValues(state = "New York",lng = -74, lat = 41 )
  observeEvent(input$map_shape_click,
               {heatmap$state <- input$map_shape_click$id
               heatmap$lng <- input$map_shape_click$lng
               heatmap$lat <- input$map_shape_click$lat
               #heatmap$year <- input$yearSlider
               })
  
  # plot time series
  output$timeseries <- renderPlot({
    TimePlot(heatmap$state)
  })
  
  # plot heatmap
  output$heatmap <- renderPlot({
    heat_map_plotter(heatmap$state,input$yearSlider)
    #input$map_shape_click
  })
  
  proxy = leafletProxy("map")
  observeEvent(heatmap$state,
               selected_state_plotter(proxy,
                                      df_violence_sum,
                                      heatmap,
                                      input$yearSlider,
                                      df_population))
  
  # plot ranking
   output$ranking <- renderPlot({
     Ranking(input$yearSlider,heatmap$state)
   })

  
  output$typeBar <- renderPlot({
    barCharacter(input$yearSlider,heatmap$state)
  })
   # plot pcp
  # output$pcp <- renderParcoords({
  #   Parcoord(input$yearSlider)
  # })

  
}

shinyApp(ui = ui, server = server)
