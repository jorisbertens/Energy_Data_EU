## delete all current variables
rm(list = ls())

# Load the required packages
require(shiny)
require(shinydashboard)
require(plotly)
require(datasets)
library(stringr)
require(leaflet)
require(htmltools)
require(ggplot2)
require(highcharter)
require(maps)
require(billboarder)
require(treemap)
require(dplyr)
require(gridBase)
require(RColorBrewer)
require(readxl)
require(DT)

df <- read.csv("energy_data.csv", header=T, sep = ",")

df$Value_p_c <- (df$Value / df$Population) * 1000
df$Value_p_c <- round(df$Value_p_c, 1)

###############################################################################################

ui <- dashboardPage(
    dashboardHeader(title = "Energy Consumption of European Countries", titleWidth = 500),
    # add a title using the title argument
    # Change the width of the title using the titlewidth argument
    # Header can be disabled using the argument disable = TRUE
    
    dashboardSidebar("",
    # sidebar can be hidden by using the argument collapsed=TRUE
    # add meny items to the sidebar
    # meny items are like tabs when clicked open up a page in tab item
      sidebarMenu(
        menuItem(text = "About", tabName = "about", icon=icon("clipboard")),
        menuItem(text = "Map", tabName = "map", icon=icon("map")),
        menuItem(text = "LinePlots", tabName = "lineplots", icon=icon("signal")),
        menuItem(text = "BarCharts", tabName = "barcharts", icon=icon("bar-chart")),
        menuItem(text = "TimeLine", tabName = "TimeLine", icon=icon("play-circle")),
        menuItem(text = "TreeMap", tabName = "treemap", icon=icon("tree")),
        menuItem(text = "Dataset", tabName = "dataset", icon =icon("database")),
        menuItem("myGithub",  href = "https://github.com/jorisbertens/energy_consumption", icon=icon("code"))
        # https://fontawesome.com/icons?d=gallery
      )
  ),
  
    dashboardBody(
      
      # within tabitems(), define the pages for sidebar menu items
      tabItems(
        tabItem(tabName = "about", p(strong("Introduction")), p(" "),p("Over the last years, due to the fact that climate changes, energy consumption became a topic of high interest (NASA). Countries are setting themselves the goal to reduce the share of non-renewable energy and increase the consumption of renewables, especially after the Paris Agreement in 2016 by the United Nations (United Nations)."), 
p("To explore if this goal of reducing energy usage of non-sustainable energy types is achieved by European countries and how the development of energy consumption has been in the past decades, information about the energy consumption of several types of energy sources is visualized in different ways.
The objective is to present the user useful visualizations to better understand the development of the energy consumption in Europe over the last decades. So that the user creates a higher perception of what has to change in order to reduce climate change.
  "), p(" "),
                fluidRow(valueBoxOutput("minconsumption_", width = 4), valueBoxOutput("maxconsumption_", width = 4), valueBoxOutput("meanconsumption_", width = 4)),
                fluidRow(valueBoxOutput("minconsumption_p_c", width = 4), valueBoxOutput("maxconsumption_p_c", width = 4), valueBoxOutput("meanconsumption_p_c", width = 4)),
                fluidRow(valueBoxOutput("mostrenewable_", width = 4), valueBoxOutput("leastrenewable_", width = 4), valueBoxOutput("alleurope1990_", width = 4)),
                fluidRow(valueBoxOutput("leastsolid_", width = 4), valueBoxOutput("mostsolid_", width = 4), valueBoxOutput("alleurope2016_", width = 4))),
        tabItem(tabName = "map",
                #position of the widgets
                fluidRow(box(title = "Product",width = 4,height = 150, uiOutput("inputproduct")),
                box(title = "Year",width = 4,height = 150, uiOutput("inputyear")),
                box(title = "Value", width = 4,height = 150,uiOutput("inputpercapita4"))),
                
                # position of the map
                fluidRow(box(title = strong("Energy consumption in Europe"),tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"), leafletOutput("firstMap", height = 700), width = 12, height = 770))),
            
        tabItem(tabName = "lineplots",
                # position of the widget
                fluidRow(box(title = "Countries", uiOutput("inputcountry"), width = 3, height = 250,
                             checkboxInput("all_europe", "All Europe", FALSE),
                             checkboxInput("per_capita", "Per Capita", FALSE)),
                         box(title = "Products", uiOutput("inputproductLine"), width = 3, height = 250),
                         box(title = "Time Line", uiOutput("inputtime7"), width = 6, height = 250)
                ),
                
                # position of the plot
                fluidRow(box(title = strong("Consumption Development over time"), highchartOutput("graph1"), width = 12, height = 500))),
        
        tabItem(tabName = "barcharts",
                # position of the widget
                fluidRow(box(title = "Countries", uiOutput("inputcountry2"), width = 6, height = 250),
                         box(title ="Products", uiOutput("inputproduct2"), width = 4, height = 250),
                box(title ="Year", uiOutput("inputyear2"), width = 2, height = 250)),
                
                # position of the plot
                fluidRow(box(title = strong("Composition of Energy Consumption by Country"), plotOutput("mplot", height = 550), width = 12, height = "100%"))),
         
        
        tabItem(tabName = "TimeLine",
                # position of the widget
                fluidRow(box(title = "Value", width = 4,height = 150,uiOutput("inputpercapita1")),
                         box(title = "Year",collapsed=TRUE, width = 4 ,height = 150,uiOutput("inputtime6")),
                         box(title = "Product", width = 4,height = 150,uiOutput("inputenergy6"))),
                
                
                
                # position of the plot
                fluidRow(box(title = "", plotOutput("mplottime",height = 550), width = 12, height = 600))),
        
        
        tabItem(tabName = "treemap",
                fluidRow(box(title = "Country", uiOutput("inputcountry3")),
                         box(title = "Year", uiOutput("inputyear3"))),
                
                fluidRow(box(title = strong("Energy consumption per Country in KTOE"), plotOutput("treemap_energy_country"), height = 700, width = 12))),
                
        tabItem(tabName = "dataset",
                #fluidRow(box(title = "Select Countries", uiOutput("inputcountry4", width = 6, height = 250))),
                
                fluidRow(box(title = "Dataset output", dataTableOutput("mydatatable"), width = 12, height = 800)))
        )
      )  
)
  
###############################################################################################################################
        
server <- function(input, output){
  
  ## input product (for Map)
  output$inputproduct <- renderUI({
    selectInput(inputId = "product", label = "Select Product:", choices = df$PRODUCT, selected = "")
  })
  
  ## input product (for Map)
  output$inputpercapita4 <- renderUI({
    radioButtons(inputId = "percapita8", # name of this input to access it later in the server part
                 label= "Select Value: ", # title of input menue with html style
                 choices= c("Total consumption","Consumption per capita"), # adopt the choices to be displayed from our ordered "pollutantsorder"- vector
                 selected = "Consumption per capita") # make all air pollutants to the default selected 
  })
  
  
  
  
  ## input year (for Map)
  output$inputyear <- renderUI({
    selectInput(inputId = "year", label = "Select Year:", choices = df$TIME, selected = "")
  })
  
  ## input productLine (for lineplot)
  output$inputproductLine <- renderUI({
    checkboxGroupInput(inputId = "productLine", # name of this input to access it later in the server part
                       label= "Select Products: ", # title of input menue with html style
                       choices= unique(df$PRODUCT), # adopt the choices to be displayed from our ordered "pollutantsorder"- vector
                       selected = c("Total petroleum products", "Gas", "Solid fuels")) # make all air pollutants to the default selected  
  })
  
  ## input country (for lineplot)
  output$inputcountry <- renderUI({
    selectInput(inputId = "country", label = "Select Country:", choices = df$GEO, selected = "")
  })
  
  ## input year7 (for lineplot)
  output$inputtime7 <- renderUI({
    sliderInput(
      "Year7", "Select Year:", min= min(df$TIME), max =max(df$TIME), 
      value=c(1990,2016),sep="",step=1,round = TRUE, animate = animationOptions(interval = 2000, loop = TRUE, playButton = NULL,
                                                                                pauseButton = NULL), dragRange= TRUE)
  })
  
  ## input year2 (for Barchart)
  output$inputyear2 <- renderUI({
    selectInput(inputId = "year2", label = "Select Year:", choices = df$TIME, selected = "")
  })
  
  ## input country2 (for Barchart)
  output$inputcountry2 <- renderUI({
    selectInput(inputId = "country2", label = "Select Countries: ", choices= unique(df$GEO), 
                multiple = TRUE,
                selected = c("Netherlands", "Belgium", "Denmark","Ireland","Czech Republic","Portugal")
  )              
  })
  
  ## input Year (for TimeLine)
  output$inputtime6 <- renderUI({
    sliderInput(
      "Year66", "Select Year:", min= min(df$TIME), max =max(df$TIME),
      value=c(2016),sep="",step=1,round = TRUE, animate = animationOptions(interval = 2500, loop = TRUE, playButton = NULL,
                                                                           pauseButton = NULL), dragRange= TRUE)
    
  })
  
  #playbotton 
  ## input Type energy (for TimeLine)
  output$inputenergy6 <- renderUI({
    selectInput(inputId = "product7", label = "Select Product:", choices = df$PRODUCT, selected = "")
  })
  
  ## input percapita (for Timeline)
  output$inputpercapita1 <- renderUI({
    radioButtons(inputId = "percapita7", # name of this input to access it later in the server part
                 label= "Select Value: ", # title of input menue with html style
                 choices= c("Total consumption","Consumption per capita"), # adopt the choices to be displayed from our ordered "pollutantsorder"- vector
                 selected = "Consumption per capita") # make all air pollutants to the default selected  
  })
  
  ## input product2 (for Barchart)
  output$inputproduct2 <- renderUI({
    checkboxGroupInput(inputId = "product2", # name of this input to access it later in the server part
                       label= "Select Products: ", # title of input menue with html style
                       choices= unique(df$PRODUCT), # adopt the choices to be displayed from our ordered "pollutantsorder"- vector
                       selected = c("Total petroleum products", "Gas", "Solid fuels")) # make all air pollutants to the default selected  
  })
  
  ## input country3 (for Treemap)
  output$inputcountry3 <- renderUI({
    selectInput(inputId = "country3", label = "Select Country:", choices= unique(df$GEO), 
                selected = "Portugal")
  })
  
  ## input year3 (for Treemap)
  output$inputyear3 <- renderUI({
    selectInput(inputId = "year3", label = "Select Year:", choices = unique(df$TIME), selected = 2016)
  })

  ## input country4 (for Dataset)
  output$inputcountry4 <- renderUI({
    selectInput(inputId = "country4", label = "Select Countries: ", choices= unique(df$GEO), 
                multiple = TRUE,
                selected = c("Netherlands", "Belgium", "Denmark","Ireland","Czech Republic","Portugal"))
  })
  
  ###############################################################################################
  
  # Output for Map
  output$firstMap <- renderLeaflet({
    
    df_tmp1<-reactive({
      subset(df, df$PRODUCT==input$product & df$TIME==input$year)
    })
    
    df_colors1<-reactive({
      subset(df, df$PRODUCT==input$product)
    })
    
    df_tmp <-df_tmp1()
    df_colors <- df_colors1()
    
    bounds <- map("world", unique(df$GEO), fill = TRUE, plot = FALSE)
    
    d<-strsplit(bounds$names, ":")
    a<-do.call(rbind.data.frame, d)
    colnames(a) <- c("GEO", "rest")
    df1 <- str_split_fixed(a$GEO, ":", 2)
    colnames(df1) <- c("GEO", "rest")
    df1 <- cbind(df1, "observation"=1:nrow(df1)) 
    df2 <- merge(x = df1, y = df_tmp, by = "GEO", all.x = TRUE)
    df2 <- df2[order(as.numeric(as.character(df2$observation))),]
    
    #bounds$value <- df2$Value_p_c
    
    if(input$percapita8=="Consumption per capita"){
      bounds$value <- df2$Value_p_c
      if(input$product=="Renewable energies"){
        pal <- colorNumeric("Greens", df_colors$Value_p_c)
      } else {
        pal <- colorNumeric("OrRd", df_colors$Value_p_c)
      }
      
      leaflet(bounds)%>%
        addTiles()%>%
        setView(29, 57, 3) %>%
        addPolygons(stroke = TRUE,
                    smoothFactor = 0.5,
                    fillOpacity=0.6,
                    fillColor = ~pal(bounds$value),
                    color = "black",
                    opacity = 0.7,
                    weight = 1,
                    popup = paste("<b>",bounds$names,"</b>", "<br>", bounds$value, "TOE"
                    )
        )%>%
        addLegend("topright", pal = pal, values = df_colors$Value_p_c,
                  title = "in Tons Oil Equivalent",
                  #labels = c(min(df$Value_p_c), max(df$Value_p_c)),
                  opacity = 0.6
        )
      
    } else {
      bounds$value <- df2$Value
      if(input$product=="Renewable energies"){
        pal <- colorNumeric("Greens", df_colors$Value)
      } else {
        pal <- colorNumeric("OrRd", df_colors$Value)
      }
      
      leaflet(bounds)%>%
        addTiles()%>%
        setView(29, 57, 3) %>%
        addPolygons(stroke = TRUE,
                    smoothFactor = 0.5,
                    fillOpacity=0.6,
                    fillColor = ~pal(bounds$value),
                    color = "black",
                    opacity = 0.7,
                    weight = 1,
                    popup = paste("<b>",bounds$names,"</b>", "<br>", bounds$value, "KTOE"
                    )
        )%>%
        addLegend("topright", pal = pal, values = df_colors$Value,
                  title = "in Kilo Tons Oil Equivalent",
                  #labels = c(min(df$Value_p_c), max(df$Value)),
                  opacity = 0.6
        )
      
    }
    
    
    
    
    
  })
  
  ###############################################################################################
  
  # Output for Lineplot
  output$graph1 <- renderHighchart({
    
    #df_color <- read.csv("energy_color.csv", header=T, sep = ",")
    
    #df_grouped <- read.csv("energy_data_grouped.csv", header=T, sep = ",")
    #aggregate(b$Value, by=list(GEO=b$GEO,PRODUCT=b$PRODUCT), FUN=sum)
    
    df_grouped <- aggregate(df$Value, by=list(df$TIME, df$PRODUCT), FUN=sum)
    colnames(df_grouped) <- c("TIME", "PRODUCT","Value")
    
    
    
    df$Value_p_c <- (df$Value / df$Population) * 1000
    df$Value_p_c <- round(df$Value_p_c, 1)
    
    
    
    df2 <- df %>%
      as.data.frame() %>%
      # the content of "input$product2" changes whener the user changes the input
      filter(PRODUCT %in% input$productLine & GEO %in% input$country) # %>%
    #color = c("#00FF00", "#FF0000", "#00FF00", "#ffa500", "#FF0000")
    
    df_tmp4<-reactive({
      b=subset(df2, df2$TIME>=input$Year7[1]& df2$TIME<=input$Year7[2])
    })
    
    df2_ <- df_tmp4()
    
    df_grouped2 <- df_grouped %>%
      as.data.frame() %>%
      # the content of "input$product2" changes whener the user changes the input
      filter(PRODUCT %in% input$productLine)
    
    
    
    
    if(input$all_europe)
    {
      hchart(df_grouped2, "line", hcaes(x = "TIME", y = "Value", group = "PRODUCT"))
    }
    else if (input$per_capita)
    {
      hchart(df2_, "line", hcaes(x = "TIME", y = "Value_p_c", group = "PRODUCT"))
    }
    else
    {
      hchart(df2_, "line", hcaes(x = "TIME", y = "Value", group = "PRODUCT"))
    }
    
  })
  
  #################################################################################################
  
  # Output for Barchart
  data <- reactive({
    
    validate(
      need(input$country2 != "", "Please select at least one country"),
      need(input$product2 != "", "Please select at least one product")
      
    )
    
    # filter the data for plot depending on reactive data input with the filter()-function of dplyr
    plotdata <- df %>%
      as.data.frame() %>%
      # the content of "input$product2" changes whener the user changes the input
      filter(PRODUCT %in% input$product2 & GEO %in% input$country2 & TIME %in% input$year2)
    
    #create dataset that contains "optimized" range of y-axis scales dependent on reactive data input
    scalecalc <- plotdata %>%
      group_by(GEO) %>%
      summarize(value = sum(plotdata$Value))
    
    scalemax <- max(scalecalc$value)
    scalesteps <- round(scalemax/10, digits = -3)
    
    list(plotdata = plotdata,
         scalemax = scalemax,
         scalesteps = scalesteps
    )
  })

  my_cols = c("#FF0000", "#008000", "#00FFFF", "#000080", "#800080", "#FFFF00")
  
  output$mplot <- renderPlot({
    #data =data()$plotdata
    myplot <- ggplot(data = data()$plotdata, aes(as.factor(GEO), y = Value,
                                                 fill = factor(PRODUCT), order = PRODUCT)) +
      geom_bar(stat = "identity") + 
      xlab("Country") +
      ylab("Consumption in KTOE") +
      theme_minimal()+
      ggtitle("") +
      guides(fill=guide_legend(title="products", reverse = T))+
      
      scale_y_continuous(breaks=seq(0,data()$scalemax, data()$scalesteps),
                         labels=abs(seq(0,data()$scalemax, data()$scalesteps))) + # !
      theme(plot.title=element_text(family="Arial", face="bold", size=18),
            axis.text.x = element_text(angle = 0, family="Arial", size=13), 
            axis.text.y = element_text(angle = 0, family="Arial", size=13),
            axis.title.x = element_text(size=14, face="bold", vjust = -1),
            axis.title.y = element_text(size=14, face="bold", vjust = 2))
    
    print(myplot)
    
    
   
    
  })
  
#################################################################################################################
  
  
  
  
  output$mplottime <- renderPlot({
    df_tmp4<-reactive({
      b=subset(df, df$PRODUCT==input$product7 & df$TIME>=1990& df$TIME<=input$Year66[1])
      if(input$percapita7=="Consumption per capita"){
        c= aggregate(b$Value_p_c, by=list(GEO=b$GEO,PRODUCT=b$PRODUCT), FUN=sum)
      } else {
        c= aggregate(b$Value, by=list(GEO=b$GEO,PRODUCT=b$PRODUCT), FUN=sum)
      }
      #c= aggregate(b$Value, by=list(GEO=b$GEO,PRODUCT=b$PRODUCT), FUN=sum)
      colnames(c) <- c("GEO", "PRODUCT","Value")
      return(c)
    })
    
    
    xd2 <-  if (input$product7 == "Total petroleum products") { 
      paste0("Petroleum")
    } else if (input$product7 == "Waste (non-renewable)") {
      paste0("Waste")
    } else paste0(input$product7)
    
    xd3 <- paste0(toString("Barchart of Consumption of "),toString(xd2))
    
    
    xd<-paste0(toString(input$percapita7),toString(" in KTOE"))
    
    df_tmp_4 <-df_tmp4()
    
    if(input$product7=="Renewable energies"){
      pal <- colorNumeric("Greens", df_tmp_4$Value)
    } else {
      pal <- colorNumeric("OrRd", df_tmp_4$Value)
    }
    
    
    myplottime <-  ggplot(data = df_tmp_4, aes(reorder(GEO, Value, sum), y = Value,
                                               fill = pal(df_tmp_4$Value), order = Value),las=1) +
      geom_bar(stat = "identity") + 
      xlab("Country") +
      ylab(xd) +
      theme_minimal()+
      ggtitle(xd3) +
      coord_flip()+
      theme(legend.position="none")+
      #guides(fill=guide_legend(title="products", reverse = T))+
      #scale_y_continuous(breaks=seq(0,scalemax, scalesteps),
      #labels=abs(seq(0,scalemax, scalesteps))) + # !
      theme(plot.title=element_text(family="Arial", face="bold", size=18),
            axis.text.x = element_text(angle = 0, family="Arial", size=13), 
            axis.text.y = element_text(angle = 0, family="Arial", size=13),
            axis.title.x = element_text(size=14, face="bold", vjust = -1),
            axis.title.y = element_text(size=14, face="bold", vjust = 2))
    
    
    print(myplottime)
  })
  
  
  
######################################################################################  
  # Output for Treemap
  data_selected_country<-reactive({
    subset(df, df$GEO==input$country3 & df$TIME==input$year3)
  })
  
  output$treemap_energy_country <- renderPlot({
    
    par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
    plot(c(0,1), c(0,1), axes=F, col="white")
    vps <- baseViewports()
    
    temp=data_selected_country()
    
    temp$percentage <- paste(round((temp$Value/sum(temp$Value))*100, digits = 0), "%", sep="")
    
    temp$label <- paste(temp$PRODUCT, temp$percentage, sep = "\n ")
    
    .tm <<- treemap(temp,
                    index="label",
                    vSize="Value",
                    vColor="Value",
                    type="value",
                    title="",
                    palette="Blues",
                    border.col="white",
                    position.legend = "right",
                    fontsize.labels = 16,
                    algorithm = "pivotSize",
                    title.legend = ""
                    
    )
  })

####################################################################################################################

  # Output for Datatable
  output$mydatatable <- renderDataTable({
    df
  })
  
  ####################################################################################################################
  
  # 1st fluid Row & 1st value box
  
  df_current = subset(df, df$TIME==2016)
  df_old = subset(df, df$TIME==1990)
  
  df_agg_total = aggregate(df_current$Value, by=list(GEO=df_current$GEO), FUN=sum)
  colnames(df_agg_total) <- c("GEO", "Value")
  df_agg_capita = aggregate(df_current$Value_p_c, by=list(GEO=df_current$GEO), FUN=sum)
  colnames(df_agg_capita) <- c("GEO", "Value_p_c")
  
  df_total_min = subset(df_agg_total,  df_agg_total$Value==min(df_agg_total$Value))
  df_total_max = subset(df_agg_total,  df_agg_total$Value==max(df_agg_total$Value))
  
  df_capita_min = subset(df_agg_capita,  df_agg_capita$Value_p_c==min(df_agg_capita$Value_p_c))
  df_capita_max = subset(df_agg_capita,  df_agg_capita$Value_p_c==max(df_agg_capita$Value_p_c))
  
  
  df_renew = subset(df_current, df_current$PRODUCT=="Renewable energies")
  df_renew_min = subset(df_renew,  df_renew$Value_p_c==min(df_renew$Value_p_c))
  df_renew_max = subset(df_renew,  df_renew$Value_p_c==max(df_renew$Value_p_c))
  
  df_renew_per = select(df_renew, GEO, Value)
  df_agg_per = select(df_agg_total, GEO, Value)
  names(df_renew_per) <- c("GEO", "Value_renew")
  
  df_per <- merge(df_renew_per, df_agg_per, by="GEO")
  df_per$Percentage <- (df_per$Value_renew/df_per$Value)*100
  
  df_per_min = subset(df_per,  df_per$Percentage==min(df_per$Percentage))
  df_per_max = subset(df_per,  df_per$Percentage==max(df_per$Percentage))
  
  
  output$minconsumption_ <- renderValueBox({
    valueBox(
      value = df_total_min$GEO,
      subtitle = "Country with Lowest Total consumption (2016)",
      color = "green"
    )
  })
  
  output$maxconsumption_ <- renderValueBox({
    valueBox(
      value = df_total_max$GEO,
      subtitle = "Country with Highest Total Consumption (2016)",
      color = "red"
    )
  })
  
  output$meanconsumption_ <- renderValueBox({
    valueBox(
      value = paste(format(round(mean(df_agg_total$Value),0), nsmall=0, big.mark=","), " KTOE"), 
      subtitle = "Mean Total Consumption (2016)",
      color = "blue"
    )
  })
  
  output$minconsumption_p_c <- renderValueBox({
    valueBox(
      value = df_capita_min$GEO,
      subtitle = "Country with Lowest Consumption per Capita (2016)",
      color = "green"
    )
  })
  
  output$maxconsumption_p_c <- renderValueBox({
    valueBox(
      value = df_capita_max$GEO,
      subtitle = "Country with Highest Consumption per Capita (2016)",
      color = "red"
    )
  })
  
  output$meanconsumption_p_c <- renderValueBox({
    valueBox(
      value = paste(round(mean(df_agg_capita$Value_p_c),1), " TOE"), 
      subtitle = "Mean Consumption per Capita (2016)",
      color = "blue"
    )
  })
  
  output$mostrenewable_ <- renderValueBox({
    valueBox(
      value = df_renew_max$GEO,
      subtitle = "Country Consuming Most Renewable energies per Capita (2016)",
      color = "green"
    )
  })
  
  output$leastrenewable_ <- renderValueBox({
    valueBox(
      value = df_renew_min$GEO,
      subtitle = "Country Consuming Least Renewable energies per Capita (2016)",
      color = "red"
    )
  })
  
  output$alleurope1990_ <- renderValueBox({
    valueBox(
      value = paste(format(round(sum(df_old$Value),0), nsmall=0, big.mark=","), " KTOE"), 
      subtitle = "Total Energy Consumption of Europe (1990)",
      color = "blue"
    )
  })
  
  output$leastsolid_ <- renderValueBox({
    valueBox(
      value = paste(df_per_max$GEO, " (", round(df_per_max$Percentage,1),"%)"),
      subtitle = "Country with Highest % of Renewables (2016)",
      color = "green"
    )
  })
  
  output$mostsolid_ <- renderValueBox({
    valueBox(
      value = paste(df_per_min$GEO, " (", round(df_per_min$Percentage,1),"%)"),
      subtitle = "Country with Lowest % of Renewables (2016)",
      color = "red"
    )
  })
  
  output$alleurope2016_ <- renderValueBox({
    valueBox(
      value = paste(format(round(sum(df_current$Value),0), nsmall=0, big.mark=",")," KTOE"), 
      subtitle = "Total Energy Consumption in Europe (2016)",
      color = "blue"
    )
  })
}

shinyApp(ui, server)

