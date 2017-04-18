setwd("~/Documents/msan622_data_visualization/hw/hw2/untitled folder")
cat('\014')
rm(list = ls())
#dev.off()
library(reshape2)
library(dplyr)
library(plotly)
library(ggplot2)
life_exp <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv',skip = 3)
population <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv')
fertility <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv',skip = 3)
metadata <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv',na.strings=c("", "NA"))

population$X2015 <- NULL
population$X2016 <- NULL
population$X <- NULL
population$Indicator.Code <- NULL
population$Indicator.Name <- NULL
life_exp$X2015 <- NULL
life_exp$X2016 <- NULL
life_exp$X <- NULL
life_exp$Indicator.Code <- NULL
life_exp$Indicator.Name <- NULL
fertility$X2015 <- NULL
fertility$X2016 <- NULL
fertility$X <- NULL
fertility$Indicator.Code <- NULL
fertility$Indicator.Name <- NULL
metadata$IncomeGroup <- NULL
metadata$SpecialNotes <- NULL
metadata$X <- NULL
metadata$TableName<- NULL
metadata <- na.omit(metadata)

population <- melt(population,value.name = 'Population')
life_exp <- melt(life_exp,value.name = 'Life_Expectancy')
fertility <- melt(fertility,value.name = 'Fertility')


final_df <- merge(population,life_exp)
final_df <- merge(final_df,fertility)
final_df <- merge(final_df,fertility)
final_df <- merge(final_df,metadata)
final_df$Country.Code <- NULL
final_df$variable <- substring(final_df$variable,first=2)
colnames(final_df)[2] <- "Year"
final_df$Year <- as.numeric(final_df$Year)
# final_df <- final_df[final_df$Country.Name!='World', ]
final_df <- arrange(final_df,Country.Name, Year)
final_df <- final_df[!(is.na(final_df$Region) | final_df$Region==""),]
rm(fertility,life_exp,metadata,population)

ui <- fluidPage(
  titlePanel("Data Visualization Homework 2"),
  mainPanel(
    plotlyOutput("scatter"),
    sliderInput("Year", "Year", min=1960, max=2014,
                step=1, value = 1960, width=550,
                animate = animationOptions(interval=300), sep="")
  ),
  sidebarPanel(
    sliderInput("Population", "Population", value=5, min=1, max=10, step=1)
  )
)


server <- function(input, output){
  subset_final_df <- reactive(final_df[final_df$Year == input$Year,])
  output$scatter <- renderPlotly({
    year_plot <- ggplot(subset_final_df(), aes(x=Life_Expectancy, y=Fertility))
    year_plot <- year_plot+ geom_point(aes(colour=Region, text = paste("Region: ", Region, "</br> Country Name: ", Country.Name, 
                                                                       "</br> Population: ", Population, "</br> Life Expectancy: ", 
                                                                       round(Life_Expectancy,3), "</br> Fertility Rate: ", 
                                                                       round(Fertility,3)),size=Population))
    year_plot <- year_plot+ xlab("Life Expectancy") + ylab("Fertility")
    year_plot <- year_plot + scale_x_continuous(breaks = seq(10,90,10), limits = c(10,90))+ scale_y_continuous(breaks = seq(0,9,1), limits = c(0,9))
    year_plot <- year_plot+ggtitle('Life Expectancy vs Fertility')
    year_plot <- year_plot + scale_color_manual(values = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494'), name="")
    year_plot <- year_plot + scale_size(range = c(1,input$Population), guide = FALSE)
    ggplotly(year_plot, tooltip = c('text'))
  })
}

shinyApp(ui = ui, server = server)