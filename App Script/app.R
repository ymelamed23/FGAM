#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(DT)
library(shinythemes)
library(hoopR)
library(ggplot2)
library(tidyverse)
library(geomtextpath)
library(ggimage)
library(rvest)
library(RcppParallel)
library(jsonlite)
library(RSQLite)
library(cli)
#load data
player_wide = read.csv("nba_FGAM.csv")

############################################################ app ##################################################################


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
  tabsetPanel(
  #  tabPanel("Input Players",
 #         selectInput(inputId = "player1", 
  #                    label   = "select player 1", 
   #                   choices = unique(player_wide$ShotLocations.PLAYER_NAME),
    #                  selected = "LeBron James"),
     #     selectInput(inputId = "player2", 
      #                label   = "select player 2", 
       #               choices = unique(player_wide$ShotLocations.PLAYER_NAME),
        #              selected = "Luka Doncic")),
    tabPanel("About",
             fluidRow(h1("Yonathan Melamed's NBA Field Goals Above the Mean (Updated 2/16/2024)")),
             fluidRow(h3("Welcome to my site! I've developed a very simple metric that aims to estimate each player's field goals added above the average shooter at each shot location. For each player, I take their attempted field goals for each shot location and multiply it by the league-wide season average FG% of the shot location (I strip out each player's own shot attempts from this average). This gives me the number of field goals the player would make had they been shooting like the average shooter of each shot location. I then subtract this from the amount of field goals the player actually has made, giving me my metric, Field Goals Above the Mean. I find this simple calculation both intuitive and more contextual than simple field goals made, FG%, or even TS%. It's a combination of both volume and efficiency. Still, it has it's limitations, and should not be thought of as a pure 'value added' statistic, since we are penalizing/rewarding players differently for 2/3 point shots that have the same value. In the above tabs you can find radar graphs to compare 2 players' statistics (thanks to @tanya_shapiro for the inspo!), along with a table that displays the top 10 at each measure, but can be filtered and sorted to find different combinations of players. Thanks for checking it out!"))),
    tabPanel("Radar Graph",
             fluidRow(selectInput(inputId = "player1", 
                                  label   = "Select Player 1 (Note: Giannis currently breaks the graph.)", 
                                  choices = unique(player_wide$ShotLocations.PLAYER_NAME),
                                  selected = "LeBron James")),
             fluidRow(selectInput(inputId = "player2", 
                                  label   = "Select Player 2", 
                                  choices = unique(player_wide$ShotLocations.PLAYER_NAME),
                                  selected = "LeBron James")),
             
             fluidRow(
           plotOutput("distPlot", width = '100%')
        )),
  tabPanel( "Leaders Table",
            sidebarPanel(
      selectInput(inputId = "leader_type",
                  label = "Select FGAM Measure",
                  choices = unique(player_wide$Shot_Type))
    ),
    mainPanel(titlePanel("FGAM Leaders"),
              DTOutput("leaders"))
    
  )
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #filter players
  app_plot_data = reactive({player_wide %>% filter(Shot_Type!="Total FGAM", Shot_Type!="Total 3", 
                                                   ShotLocations.PLAYER_NAME == input$player1| ShotLocations.PLAYER_NAME== input$player2)})  
  
  

    output$distPlot <- renderPlot({
      
      #set color aesthetics as variables
      #palettes
      pal_font<-"white"
      pal_bg <-"#131314"
      pal_line <-"#D0D0D0"
      pal<-c("#ffe850","#FF903B","#E1341A","#27f880","#4bd8ff")
      
      
      #custom dataframe for line segments
      segments<- data.frame(
        x1=rep(0,4),
        x2=rep(4.5,4),
        y1=c(-10,0,10, 20 ),
        y2=c(-10,0,10, 20 )
      )
      
      #custom dataframe for labels
      labels<-data.frame(
        y = c(-10, 0, 10, 20),
        x = rep(0.25,4)
      )
      
      
      ggplot(app_plot_data(), aes(x=Shot_Type, y=FGAM, fill=Shot_Type))+
        #create y axis text
        geom_textpath(inherit.aes=FALSE,
                      mapping=aes(x=Shot_Type, label=Shot_Type, y=30),
                      fontface="bold", upright=TRUE, text_only=TRUE, size=5, color=pal_font
        )+
        #image with cahracter icon in the center
        geom_image(mapping=aes(y=-30,x=1,image=head), size=0.1)+
        #text for actual name below superhero name
        # geom_text(inherit.aes=FALSE,
        #          mapping=aes(label=ShotLocations.PLAYER_NAME, x=1, y=-30),
        #         vjust=-13.6, color="white", size=3.25)+
        #create curved coordinate system, curvedpolar accomodates text
        coord_curvedpolar()+
        #create line segments to represent panel gridlines
        geom_segment(inherit.aes=FALSE,
                     data = segments,
                     mapping=aes(x=x1,xend=x2,y=y1,yend=y2),
                     size=0.45, color=pal_line)+
        #bars
        geom_col(show.legend = FALSE, width=1)+
        #text for panel gridlines
        geom_textsegment(inherit.aes=FALSE,
                         data=labels,
                         mapping=aes(x=4.5, xend=5.5, y= y, yend=y, label=y),
                         color = pal_line, textcolour= pal_font, linewidth=0.45, size=4)+
        #adjist scales for fill (custom palette) & create y scale limits (create blank circle at center to store image)
        scale_fill_manual(values=pal)+
        scale_y_continuous(limits=c(-30,60))+
        #iterate per character
        facet_wrap(~toupper(ShotLocations.PLAYER_NAME))+
        #add labels & theme
        labs(#title="FGAM",
             subtitle="NBA Field Goals Above Mean by Shot Type")+
            # caption="Data from hoopR | Graphic @yonathan_melamed")+
        theme_minimal()+
        theme(text=element_text(color=pal_font),
              plot.background = element_rect(fill=pal_bg),
              plot.title=element_text(face="bold", hjust=0.5, size=18, margin=margin(t=5)),
              plot.subtitle=element_text(hjust=0.5, margin=margin(t=5, b=20), size = 15),
              plot.caption = element_text(margin=margin(t=15), size = 4),
              axis.title=element_blank(),
              panel.grid = element_blank(),
              plot.margin=margin(t=10,b=5,l=10,r=10),
              axis.text=element_blank(),
              axis.ticks = element_blank(),
              strip.text=element_text(face="bold", color=pal_font, size=12, vjust=-0.5))
      
    }, height = 1300, width = 1800)
    
    app_tbl_data = reactive({player_wide %>% filter(Shot_Type==input$leader_type) %>% 
        arrange(-FGAM) %>% 
       # mutate(rank=row_number()) %>% 
        select(ShotLocations.PLAYER_NAME, ShotLocations.TEAM_ABBREVIATION, FGAM) %>% 
        rename("Player"=ShotLocations.PLAYER_NAME, "Team"=ShotLocations.TEAM_ABBREVIATION) %>% 
        mutate(across(is.numeric, round, digits = 3))})
    output$leaders = renderDT(app_tbl_data(), filter="top", options = list(columnDefs = list(list(className = 'dt-center', targets = 0:3)))) 
}

# Run the application 
shinyApp(ui = ui, server = server)
