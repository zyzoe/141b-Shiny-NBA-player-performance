library(nycflights13)
library(shiny)
library(tidyverse)


library(rvest)
library(RSelenium)
library(wdman)
library(plotly)
library(tidyverse)
library(fmsb)

server <- phantomjs(port = 4567L, verbose = FALSE)
rd <- remoteDriver(browserName = "phantomjs", port = 4567L)
rd$open(silent = TRUE)
rd$navigate("https://stats.nba.com/leaders/?SeasonType=Regular%20Season")

leader <- NULL
for (i in 1:6) {
  leader <- rd$getPageSource() %>% 
    str_flatten() %>%
    read_html() %>% 
    html_node("div.nba-stat-table__overflow table") %>% 
    html_table() %>% 
    bind_rows(leader, .)
  nextbutton <- rd$findElement("css", "a.stats-table-pagination__next")
  nextbutton$clickElement()
}

leader1<-as.matrix(leader)%>%as_tibble()%>%select(Player,PTS,REB,AST,STL,BLK,EFF)
ui <- fluidPage(
  #Application title
  titlePanel("NBA Players Comparision"),
  #Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Player1",
                  label = "Select the First Player:",
                  choices = unique(leader$Player),
                  selected = "James Harden"),
      selectInput(inputId = "Player2",
                  label = "Select the Second Player:",
                  choices = unique(leader$Player),
                  selected = "Bradley Beal")),
    mainPanel(
      plotOutput("output0"),
      plotOutput("output1"),
      plotOutput("output2"),
      plotOutput("output3"),
      plotOutput("output4"),
      plotOutput("output5"),
      plotOutput("output6")
      )
  )
)
server <- function(input, output) {
  
  leader2 <-reactive({
    filter(leader1,Player == input$Player1 | Player == input$Player2)
    
  })
  data.plot<-reactive({
    data <- leader %>% select(Player,PTS,REB,AST,STL,BLK,EFF)
    colMax <- function(data)sapply(data, max, na.rm = TRUE)
    bd.max <- colMax(data[,-1])
    bd.min <- c(rep(0,6))
    data.plot<-filter(leader,Player == input$Player1 | Player == input$Player2)%>%
      select(PTS,REB,AST,STL,BLK,EFF)
    p1<-input$Player1
    p2<-input$Player2
    data.plot <- rbind(bd.max,bd.min,data.plot)
    rownames(data.plot) <- c("max","min",p1,p2)
    colnames(data.plot) <- c("Points", "Rebounds", "Assists", "Steals", "Blocks", "Efficiency")
    data.plot
  })
  output$dtab <- renderDataTable(leader2())
  output$output0 <- renderPlot({
    library(RColorBrewer)
    coul <- brewer.pal(3, "BuPu")
    colors_border <- coul
    library(scales)
    colors_in <- alpha(coul,0.5)
    radarchart(data.plot(),
               #custom polygon
               pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
               #custom labels
               vlcex=0.8)
    })
  output$output1 <- renderPlot({
    ggplot(data = leader2(), aes(Player,PTS,label = PTS,color=Player,fill=Player)) +
      geom_col() +
      geom_label(vjust = -0.1,color="Black")
  })
  output$output2 <- renderPlot({
    ggplot(data = leader2(), aes(Player,REB,label = REB,color=Player,fill=Player)) +
      geom_col() +
      geom_label(vjust = -0.1,color="Black")
  })
  output$output3 <- renderPlot({
    ggplot(data = leader2(), aes(Player,AST,label = AST,color=Player,fill=Player)) +
      geom_col() +
      geom_label(vjust = -0.1,color="Black")
  })
  output$output4 <- renderPlot({
    ggplot(data = leader2(), aes(Player,STL,label = STL,color=Player,fill=Player)) +
      geom_col() +
      geom_label(vjust = -0.1,color="Black")
  })
  output$output5 <- renderPlot({
    ggplot(data = leader2(), aes(Player,BLK,label = BLK,color=Player,fill=Player)) +
      geom_col() +
      geom_label(vjust = -0.1,color="Black")
  })
  output$output6 <- renderPlot({
    ggplot(data = leader2(), aes(Player,EFF,label = EFF,color=Player,fill=Player)) +
      geom_col() +
      geom_label(vjust = -0.1,color="Black")
  })
}



shinyApp(ui = ui, server = server)
