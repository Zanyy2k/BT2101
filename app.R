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
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(DT)
library(googleVis)
library(tm)
library(wordcloud)
library(stringr)


setwd("/Users/benjaminong/Desktop/Shiny/SIA_dashboard")

most.countries = setDT(read.csv("most_countries.csv", as.is = T))
overall.top.500 = setDT(read.csv("overall_top_500.csv", as.is = T))
country.top.100 = setDT(read.csv("country_top_100.csv", as.is = T))
followers.country.count = setDT(read.csv("followers_country_count.csv", as.is = T))
reviews = read.csv("CollatedReviews.csv", as.is = T)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(theme="bootstrap.css", 
  "SIA Dashboard",
  tabPanel("Followers", 
           sidebarLayout(
             sidebarPanel(
               h1("Geographic Distribution of Twitter Followers"), 
               radioButtons("geogOption", "View:", 
                            c("% of Total" = "percent.total", "% of Country Population" = "percent.popn"))
             ),
             mainPanel(
               htmlOutput("followers"),
               br(),
               DT::dataTableOutput("followers.table")
             )
           )
  ), 
  navbarMenu("Top Influencers", 
             tabPanel("Overall",
                      plotOutput("all.influencers.point", height=800),
                      br(),
                      DT::dataTableOutput("all.influencers")
             ), 
             tabPanel("By Country", 
                      sidebarLayout(
                        sidebarPanel(
                          h1("Top Influencers by Country"),
                          p("Search for the top influencers on Twitter for each country."), 
                          p(
                            strong("count:"),
                            "number of Twitter followers from the country that is following the account"), 
                          p(
                            strong("percent:"),
                            "percentage of Twitter followers from that country who is following the account"),
                          selectInput("country", "Country:", sort(unique(country.top.100$country))), 
                          br()
                        ), 
                        mainPanel(
                          plotOutput("by.countries.point", height=700), 
                          br(),
                          DT::dataTableOutput("by.countries")
                        )
                      )
             ),
             tabPanel("By Category", 
                      sidebarLayout(
                        sidebarPanel(
                          h1("Top Influencers by Category"),
                          p("Search for the top influencers on Twitter for each country, by category."), 
                          p(
                            strong("count:"),
                            "number of Twitter followers from the country that is following the account"), 
                          p(
                            strong("percent:"),
                            "percentage of Twitter followers from that country who is following the account"),
                          selectInput("category", "Category:", sort(unique(country.top.100$category))), 
                          br()
                        ), 
                        mainPanel(
                          plotOutput("by.categories.point", height=700),
                          br(),
                          DT::dataTableOutput("by.categories")
                        )
                      )
             ), 
             tabPanel("Most Countries", 
                      sidebarLayout(
                        sidebarPanel(
                          h1("Top Influencers in the Most Number of Countries"),
                          p("We collated the top 100 most influential Twitter accounts per country 
                            (i.e., many people within the country are following those Twitter accounts), 
                            and counted how many countries have these accounts in the top 100. "),
                          selectInput("most.countries.cat", "Category:", sort(unique(most.countries$category)))
                        ), 
                        mainPanel(
                          DT::dataTableOutput("most.countries")
                        )
                      )
             )
        ), 
        tabPanel("Reviews", 
                 sidebarLayout(
                   sidebarPanel(
                     h1("Reviews Analysis"),
                     p("We collated reviews from a range of websites. Here, you can filter by words to analyse 
                       what other words occur with certain reviews."),
                     textInput("filterText", label = h3("Filter reviews that contain")),
                     sliderInput("reviewSlider", label=h3("Filter reviews that contain ratings between"), 
                                 min=1, max=10, value=c(1, 10)),
                     actionButton("filterReviews", label = "Filter")
                   ), 
                   mainPanel(
                     plotOutput("reviewsWordcloud"),
                     br(),
                     DT::dataTableOutput("reviewsWordTable"),
                     br(),
                     DT::dataTableOutput("reviewsTable")
                   )
                 ))
  )
)

plot.theme = function(plot) {
  plot + scale_colour_brewer(palette="Paired") + theme(panel.background = element_rect(fill = '#333333'), 
                                                       panel.grid.major = element_line(colour="#555555"),
                                                       panel.grid.minor = element_line(colour="#444444"))
}

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$all.influencers <- DT::renderDataTable(
    DT::datatable(
      overall.top.500,
      options=list(
        pageLength=10
      )
    )
  )
  
  output$all.influencers.point <- renderPlot({
    plot.theme(ggplot(overall.top.500[which(overall.top.500$count >= quantile(overall.top.500$count, c(0.5)))], 
           aes(x = category, y = count, color=category)) + geom_point() + geom_text_repel(aes(label=name))) + 
      scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x), labels = scales::comma)
  })

  output$by.countries <- DT::renderDataTable(
    DT::datatable(
      country.top.100[which(country.top.100$country == input$country),c("screen_name", "name", "description", "category", "count", "percent"), with=FALSE],
      options=list(
        pageLength=10
        )
    )
  )
  
  output$by.countries.point <- renderPlot({
    plot.theme(ggplot(country.top.100[which(country.top.100$country == input$country),], aes(x = category, color = category, y = count)) + 
      geom_point() + geom_text_repel(aes(label=screen_name)) + scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x),
                                                                      labels = scales::comma))
  })
  
  output$most.countries <- DT::renderDataTable(
    DT::datatable(most.countries[which(most.countries$category == input$most.countries.cat),c("screen_name", "name", "description", "count"), with = FALSE],
                  options=list(
                    pageLength=10
                  )
    )
  )
  
  output$by.categories.point <- renderPlot({
    plot.theme(ggplot(country.top.100[which(country.top.100$category == input$category),] %>% top_n(100, percent), aes(x = category, color = category, y = percent)) + 
                 geom_point() + geom_text_repel(aes(label=screen_name)) + scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x),
                                                                                 labels = scales::comma))
  })
  
  output$by.categories <- DT::renderDataTable(
    DT::datatable(country.top.100[which(country.top.100$category == input$category),c("screen_name", "name", "description", "country", "count", "percent", "rank"), with=FALSE][order(-percent)], 
                  options = list(
                    pageLength = 10
                  )
    )
  )
  
  output$followers <- renderGvis({
    gvisGeoChart(followers.country.count, 
                 locationvar="country",
                 colorvar=input$geogOption, 
                 options=list(
                   width="100%",
                   height="80vh",
                   colorAxis=paste0("{colors:['#CCEECC', '#55CC55','#003300'], 
                   values:[",min(unlist(followers.country.count[, c("percent.total"), with = FALSE])),", ",
                     quantile(unlist(followers.country.count[, c("percent.total"), with = FALSE]),c(0.2))[[1]],", ",
                     quantile(unlist(followers.country.count[, c("percent.total"), with = FALSE]),c(0.99))[[1]],"]}"),
                   vAxis="{format:'#%'}"
                 )
    )
  })
  
  output$followers.table <- DT::renderDataTable((
    DT::datatable(followers.country.count, 
                  options = list(
                    pageLength=10
                  ))
  ))
  
  fetchReviews <- eventReactive(input$filterReviews, {
    # Do stuff here
    filteredReviews = reviews %>% filter(Rating >= input$reviewSlider[1], Rating <= input$reviewSlider[2]) %>% 
      filter(str_detect(Content, coll(input$filterText, TRUE)))
  })
  
  buildMatrix <- function(tbl) {
    tbl$Source = NULL
    tbl$Date = NULL
    tbl$Rating = NULL
    tbl = paste(tbl, collapse = "")
    myCorpus = Corpus(VectorSource(tbl))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
    myCorpus = tm_map(myCorpus, removeWords, c("singapore", "flight", "airline", "airlines", "one", "back", "fly", "sq", strsplit(sub(",", " ", input$filterText), " ")[[1]]))
    tdm = TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
    m <- as.matrix(tdm)
  }
  
  buildCloud <- function(tbl) {
    m = buildMatrix(tbl)
    # calculate the frequency of words and sort it by frequency
    word.freq <- sort(rowSums(m), decreasing = T)
    wordcloud(words = names(word.freq),
              freq = word.freq, min.freq = 25,
              random.order = F,
              # to add in more colours
            ordered.colors=T)
  }
  
  output$reviewsWordcloud <- renderPlot({
    #fetchReviews()
    buildCloud(fetchReviews())
  })
  
  output$reviewsTable <- DT::renderDataTable(
    DT::datatable(fetchReviews())
  )
  
  output$reviewsWordTable <- DT::renderDataTable(
    DT::datatable(buildMatrix(fetchReviews()),
                  options = list(
                    order = list(list(1, 'desc'))
                  ))
  )
  
})

# Run the application 
shinyApp(ui = ui, server = server)

