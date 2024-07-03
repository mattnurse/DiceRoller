
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries of packages
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(shiny)
library(psych)
library(tidydice)
library(xtable)
library(gridExtra)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Dice roller"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      p("This app shows how larger sample sizes are more likely to give you representative data about your population. Play with the sliders below. This will randomly produce the outcomes of dice throws, just like in real life."), p("The graph to the right is a tally of all of the results of each of the throws in your sample."), p("Pay attention to the title of this graph. When it reports that your data doesn't have a normal distribution (that classic bell shape we often see in data), that's probably because your sample size is too small - although random chance also plays a role. However, when you increase your sample size, the title of the graph may change to indicate that your data now has a normal distribution, which may more accurately represent reality."), p("Remember, if you graphed all the possible outcomes of any combination of dice throws it would have a normal distribution, so your sample should too."), p("For example, when throwing three six-sided dice, you would expect a normal distribution, with a mean around 10.5, a median of 11, a standard deviation of 3, with close to zero skew and a fairly flat distribution with a kurtosis of around -0.4. However that is much more likely when the sample is large"), 
      
      
      
      sliderInput(inputId = "Number_dice", label = "How many dice do you want to roll at a time?",
                  min = 1,
                  max = 5,
                  value = 3),
      
      sliderInput(inputId = "Sides_dice", label = "How many sides do these dice have?",
                  min = 4,
                  max = 20,
                  value = 6,
                  step = 2),
      
      sliderInput(inputId = "Number_rolls", label = "How many times do you want to roll these dice?",
                  min = 5,
                  max = 1000,
                  value = 5,
                  step = 5),
      
      p("The statistics in the table below the graph help to describe the data and its normality. You can find out more about these statistics and how to interpret them here:"), tags$a(href="https://watermark.silverchair.com/mkm020.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAnUwggJxBgkqhkiG9w0BBwagggJiMIICXgIBADCCAlcGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMxH3HKzlkmQqzN4_jAgEQgIICKH6VVUnPHaAEJ6cFcEtiY9kDxBB8rHE7nS7IIF8g33rZiCHM-udz4KAeWddGAjocJI6NAPhI_HqVZ5tA_UbBClcU4uTEvriPRXaBEYZhipOMVkO_QdnStaz_dzmVzifwdJ5T7TfpUjfNJlO6kaOKdyZ4MjpSfkeANzv3kO_PwR5eLig0iHN8c912wQrjoIbond3mt8KhOOWvFtelIbeTLOEAaut_G3mWPtgkTGL0u1RetQwpF3FsjunQL1HO4rBVx4PONyDeKoUEhxxj10ZQS9ky2rqDa_wUH1qJlAMsOCt5BouveABti3l4lOISZu0ddC0BB26tXMIeVCPDSwQnRv_mZq5P7wUnXNCdYHR7D79VelYf6GAVaesmFcGPsIIjR-Hw_WnAogAf89Rm058RYz9VTUnKEJePLZGQQEnYQNDR_uiPmL0AmYcvIItwLE6WBShLvmVWvnfpAvoYLqWan2wwAFGc-nWjwENjjZLdonjcoPMY1DOw8IaVuTjdCagmKtj8uJykxHe2iJDmAT4mErAUu4-h4utS24QERIsJCvyCXO59jNbpjB14bOEgbvSML-xF61FWLdxF7seu9Q6VmvxjvtzGYPzh0y8NLZJgqbt0tbTVatMuW5adCWb1dDPqG4R1vhXrr6jyjAG7t-XH5NDB7bTKPvfBFI13j7i-EOAbAEggUrJDcy5B8XAoUSNMvM-XM-3RaVY6nhPcAX0NywRzD2bpj1hq9g", "Central tendency and the spread of data")
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      
      
      
      
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({ 
    
    #turn off scientitic notation
    options(scipen=999)
    
    #set up colour palette with large number of colours
    nb.cols <- 115
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
    
    #Code to roll dice and simulate sampling 
    data <- roll_dice(times = input$Number_dice, sides = input$Sides_dice, rounds = input$Number_rolls)
    
    #create a subset of the totals to use for normality test
    Subset_data_sum <- data %>% group_by(round) %>% summarise(result = sum(result))
    
    #convert to dataframe
    Subset_data_sum <- as.data.frame(Subset_data_sum)
    
    #select only result column and overwrite Subset_data_sub
    Subset_data_sum <-Subset_data_sum %>% select(result)
    
    #put psych summary stats into an object
    Summary_stats<-describe(Subset_data_sum)
    
    #Shapiro-wilk normality 
    s_normal <- shapiro.test(Subset_data_sum$result)
    
    #create table headings for summary table
    Statistic <- c("Mean", "Median", "Standard deviation", "Skew", "Kurtosis", "Shapiro-Wilk test (p<0.05 = normal)")
    
    #create statistcs values for summary table
    Value <-c(Summary_stats$mean, Summary_stats$median,Summary_stats$sd, Summary_stats$skew, Summary_stats$kurtosis, s_normal$p.value)
    
    #create dataframe of summary statistics
    Summary_statistics_df<- data.frame(Statistic, Value)
    
    
    #create summary table "Grob" for display beneath plot
    Summary_table <- tableGrob(xtable(Summary_statistics_df, digits=1),rows = NULL)
    
    #count the number of observations for normality test
    observations <- tally(Subset_data_sum)
    
    
    
    #create a bar graph to display the totals in a simliar format to a histagram (note we cannot use colurs for each bar of a histagram in ggplot2), and render a graph title indicating whether it passed a normality test
    s_bargraph<-ggplot(Subset_data_sum) + geom_bar(aes(factor(result), fill=factor(result))) + scale_fill_manual(values = mycolors) + xlab("total of each dice roll") +  theme_classic()+ theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), legend.position = "none", axis.title=element_text(size=12)) + if(s_normal$p.value<0.05){ggtitle("This data has a normal distribution")} else {ggtitle("This data does not have a normal distribution")} 
    
    grid.arrange(s_bargraph, Summary_table, nrow=2, as.table=TRUE, heights=c(3,2))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
