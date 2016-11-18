#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#to dos, leyend and tilte and text with the overall performance

library(shiny)
library(maps)
library(ggplot2)
library(xlsx)

actual <- read.xlsx('data.xlsx',sheetIndex=1)
actual_target <- read.xlsx('data.xlsx',sheetIndex=2)
targets_values<- read.xlsx('data.xlsx',sheetIndex=3)
#MEDICARE_targets <- read.xlsx('data.xlsx',sheetIndex=4)
#RENT_targets <- read.xlsx('data.xlsx',sheetIndex=5)

all_states <- map_data("state")
#names(all_states)[5] <- 'state'

BDT_states <- c(31,7,8,6,37,29,45,32,47,28,18,19,20,38,44)
BDT_states <- unique(all_states$region)[sort(BDT_states)]
n_states <- length(BDT_states)

#### 02. filter the data and ggplot ####
all_states <- all_states[all_states$region %in% BDT_states,]


programs <- c('SNAP','MEDICARE','RENT')
time_frames <- c('YTD','MTD')
targets <- c('out-mail','applications')


#convert actual into barplot form
db_barplot <- NULL

for (month in colnames(actual)[4:15]) {

 db_barplot <- rbind(db_barplot, data.frame(actual[,c('state','program','target')],value=actual[,month],month=month))
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("DEMO Scorecard Benefits Data Trust"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(

        img(src='bdtlogo.jpeg', align = "center",width=200),
         selectInput('program', 'Program',programs),
         selectInput('time_frame', 'Montly / Yearly', time_frames),
         selectInput('target', 'Target', targets),
         selectInput('state', 'State', c('all',BDT_states)),
        downloadButton('downloadData', 'Download'),
         width = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel( 
        fluidRow( 
          splitLayout(cellWidths = c("35%", "60%"),       
                      plotOutput("distPlot"), 
                      plotOutput("distPlot2"))
        ),
        div(tableOutput("view"), style = "font-size:80%")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
 
   output$distPlot <- renderPlot({
     ids2filter <- actual_target$program==input$program & actual_target$target==input$target
     if(input$state=='all') {
       ids2filterState <- ids2filter
       metric <- ''
     } else {
       ids2filterState <- ids2filter&actual_target$state==input$state
       metric <- paste0(round(actual_target[ids2filterState,input$time_frame]*100,0),"%",collapse="")
     }
     
     df <- data.frame(value=actual_target[ids2filterState,input$time_frame],
                      region  = actual_target[ids2filterState,'state'])

     
     ggplot() + 
       geom_map(data=all_states, map=all_states,
                aes(x=long, y=lat, map_id=region),
                fill="#ffffff", color="grey75", size=0.15) + 
       geom_map(data=df, map=all_states,
                aes(fill=value, map_id=region),
                color="grey75", 
                size=0.15) +
       scale_fill_gradient2(low="red", mid="yellow",high="#2ca25f", limits=c(0.75,1.5),midpoint = 0.99) +
       theme(legend.position="none",
             axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             plot.title = element_text(size = 14, face = "bold")) +
            ggtitle(paste0(input$time_frame," achievement of ",input$program,' ',input$target," plan",collapse="")) +
       annotate("text",y=35.326, x=-70.33,label = metric ,size=22)
   }, width=360, height=360)
   
   
   output$view <- renderTable({
     ids2filter <- actual_target$program==input$program & actual_target$target==input$target
     if(input$state=='all') {
       ids2filterState <- ids2filter
       
     } else {
       ids2filterState <- ids2filter&actual_target$state==input$state
     }
     
     df <- actual[ids2filterState,]

    df$tot <- rowSums(df[,4:13])
    df
   },digits=0,na="-",spacing = 's')
   
   output$distPlot2 <- renderPlot({
     ids2filter2 <- db_barplot$program==input$program & db_barplot$target==input$target
     ids2filtertargaests <- targets_values$program==input$program &targets_values$target==input$target
     if(input$state=='all') {
       ids2filterState2 <- ids2filter2
       ids2hline <- ids2filtertargaests
       
     } else {
       ids2filterState2 <- ids2filter2&db_barplot$state==input$state
       ids2hline <- ids2filtertargaests&targets_values$state==input$state
     }
     objective_MTD <-sum(targets_values$MTD[ids2hline])
     db_barplot <- db_barplot[ids2filterState2,c('state','value','month')]
     
     #ggplot
     ggplot(db_barplot,aes(x = month, y =value,fill=state)) + 
       geom_bar(stat="identity",width=.5) +
       geom_hline(yintercept=objective_MTD) +
       ggtitle(paste0("Monthly evolution of ",input$program,' ',input$target,collapse="")) +
       theme(legend.position="bottom",axis.title.x=element_blank(),
             plot.title = element_text(size = 14, face = "bold")) +
       ylab("quantity")
   }, width=540, height=360)
   
   output$downloadData <- downloadHandler(
     
     filename = function() { 
       'BDT_Scorecard_Data.csv'
     },
     content = function(file) {
       out_df <- cbind(actual,targets_values[,4:5])
       
       write.csv( out_df , file)
     }
   )
   
})

# Run the application 
shinyApp(ui = ui, server = server)

