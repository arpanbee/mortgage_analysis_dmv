#####################################################################
# Mortgage Market Analysis

# By Arpan Bhattacharya
# 05/17/2016
#####################################################################

# Download data from 
# http://data.arpan.info/2012_to_2014_loans_data.csv
# http://data.arpan.info/2012_to_2014_loans_data.csv
# Set working directory to the location of the data.
# Data: '2012_to_2014_loans_data.csv and '2012_to_2014_institutions_data.csv'
# setwd("/Path/to/data")

library(ggplot2)
library(rjson)
library(shiny)
library(dplyr)

hmda_init <- function() {
  
  
  # load loans and institution csv files into data frames
  loans <- read.csv("2012_to_2014_loans_data.csv", 
                    head=TRUE, sep=",", stringsAsFactors = F)
  inst <- read.csv("2012_to_2014_institutions_data.csv", 
                   head=TRUE, sep=",", stringsAsFactors = F)
  
  # join loans on institution data frame on Respondent_ID and Agency_Code
  loans_inst <- merge(loans, inst, c("Respondent_ID","Agency_Code"))
  
  # convert Conventional_Conforming_Flag from char to logical data type
  loans_inst$Conventional_Conforming_Flag <- 
    loans_inst$Conventional_Conforming_Flag == 'Y'
  
  # return expanded data set
  return(loans_inst)
}

# This function writes out a file to the working directory: 
# output.json, which is a subset of data based on the  
# arguments passed in.  

# First Argument: 'data' argument is a dataframe representing
# the full dataset; this argument is required.  

# Second Argument: 'states' argument is a vector of characters 
# representing the states, e.g. c("MD","VA"); it is optional.  

# Third Argument: 'conventional_conforming' argument is a 
# logical representing if conventional conforming flag is TRUE 
# or FALSE; it is optional.

# Fourth Argument: 'writeToFile' argument is a logical 
# representing if the subset of the data should be stored to 
# disk in output.JSON file in JSON structure 

hmda_to_json <- function(data, states=NA, conventional_conforming=NA, 
                         writeToFile=FALSE){
  if(is.na(conventional_conforming)){
    if(is.na(states)){
      # if both 'states' and 'conventional_conforming' argument
      # aren't passed in, return original dataset
      output_data <- data
    } else {
      data_by_state <- data[data$State %in% states,]
      # if 'conventional_conforming' argument is missing but
      # 'states' argument is passed in, return subset of data
      # which matches the state
      output_data <- data_by_state
    }
  } else {
    data_subset <- subset(data, Conventional_Conforming_Flag == 
                            conventional_conforming)
    if(missing(states)){
      # if 'conventional_conforming' flag is passed in but
      # 'states' is not passed in, return subset of data
      # which matches the 'conventional_conforming' flag value
      output_data <- data_subset
    } else {
      data_subset_by_state <- data_subset[data_subset$State %in% states,]
      # if both 'conventional_conforming' flag and 'states'
      # arguments are passed in, return the subset of data
      # which matches the 'conventional_conforming' flag and
      # the 'states' values
      output_data <- data_subset_by_state
    }    
  }
  if(writeToFile == TRUE) {
    output_data_json <- toJSON(unname(split(output_data,1:
                                              nrow(output_data))))
    write(output_data_json, file="output.JSON")
  }
  return(output_data)
}

# load data
data <- hmda_init()

# get conventional conforming subset of data
data_cc <- hmda_to_json(data,,TRUE,)
# get total loan amount from conventional conforming subset 
total_loans <- data_cc %>% group_by(As_of_Year.x,State) %>% 
  summarise(sumy = sum(Loan_Amount_000))

# get conventional conforming subset of data for state MD
data_cc_MD <- hmda_to_json(data,c("MD"),TRUE,)
# get total loan amount from conventional conforming subset
# by Region description (MSA_MD_Description) for state of MD
total_loans_md <- data_cc_MD %>% 
  group_by(As_of_Year.x,MSA_MD_Description) %>% 
  summarise(loans = sum(Loan_Amount_000))

# get conventional conforming subset of data for state VA
data_cc_VA <- hmda_to_json(data,c("VA"),TRUE,)
# get total loan amount from conventional conforming subset
# by Region description (MSA_MD_Description) for state VA
total_loans_va <- data_cc_VA %>% 
  group_by(As_of_Year.x,MSA_MD_Description) %>% 
  summarise(loans = sum(Loan_Amount_000))

# get conventional conforming subset of data for state WV
data_cc_WV <- hmda_to_json(data,c("WV"),TRUE,)
# get total loan amount from conventional conforming subset
# by Region description (MSA_MD_Description) for state WV
total_loans_wv <- data_cc_WV %>% 
  group_by(As_of_Year.x,MSA_MD_Description) %>% 
  summarise(loans = sum(Loan_Amount_000))

# get conventional conforming subset of data for state DC
data_cc_DC <- hmda_to_json(data,c("DC"),TRUE,)
# get total loan amount from conventional conforming subset
# by Region description (MSA_MD_Description) for state DC
total_loans_dc <- data_cc_DC %>% 
  group_by(As_of_Year.x,MSA_MD_Description) %>% 
  summarise(loans = sum(Loan_Amount_000))

# get conventional conforming subset of data for state DE
data_cc_DE <- hmda_to_json(data,c("DE"),TRUE,)
# get total loan amount from conventional conforming subset
# by Region description (MSA_MD_Description) for state DC
total_loans_de <- data_cc_DE %>% 
  group_by(As_of_Year.x,MSA_MD_Description) %>% 
  summarise(loans = sum(Loan_Amount_000))

ui <- fluidPage(
  titlePanel("Mortgage Analysis in DMV"),
  
  mainPanel(
    column(12,
           tabsetPanel(
             tabPanel("Aggregate Analysis", 
                      fluidRow(
                        plotOutput("plot1",click = "plot_click"),
                        verbatimTextOutput("info"),
                        helpText("The barchart above displays the total 
                                 Conventional Conforming Mortgage loan 
                                 amount for the states of Maryland, 
                                 Washington DC, Virginia, West Virginia, 
                                 and Delaware over years 2012, 2013, 2014.  
                                 Clicking on the chart will display the 
                                 specific year and aggregate loan amount 
                                 in $1000s."),
                        helpText("The chart shows a downward trend in 
                                 the Conventional Conforming Mortgage 
                                 market from 2012 to 2014.  The chart 
                                 also shows us that Maryland and Virginia 
                                 are the biggest aggregate markets, with 
                                 Delaware, West Virginia, and DC all being 
                                 equally smaller.  Even though the market 
                                 is trending downwards, we want to examine 
                                 specific regions within each state to 
                                 determine if there are any 
                                 niche regional growth opportunities"))
                        ),
             tabPanel("MD", 
                      fluidRow(
                        plotOutput("plotmd",click = "plotmd"),
                        verbatimTextOutput("infomd"),
                        helpText("From the chart above, we can see
                                 that there is Silver Spring region
                                 of MD has a growing loan market. 
                                 In addition, we see there has been
                                 a drop off in Bethesda and Baltimore
                                 regions but this could be explained
                                 by the way the region is being 
                                 categorized in 2014.  There is a 
                                 sharp spike in NULL regions for
                                 2014, and these could be represented
                                 by Baltimore and Bethesda."),
                        helpText(""))
             ),
             tabPanel("VA", 
                      fluidRow(
                        plotOutput("plotva",click = "plotva"),
                        verbatimTextOutput("infova"),
                        helpText("From the chart above, we can see
                                 that Arlington and Alexandria are 
                                 the biggest mortgage markets in VA.
                                  Although they have had a decrease
                                  in total mortgage sum over 2012-2014
                                  these two regions still represent
                                  the largest share within the second
                                  largest mortgage market state."),
                        helpText(""))
             ),
             tabPanel("WV", 
                      fluidRow(
                        plotOutput("plotwv",click = "plotwv"),
                        verbatimTextOutput("infowv"),
                        helpText("From the chart above, we can see
                                 that there are a lot of NULL regions
                                 within WV.  If these data points were
                                 identified we would have better information
                                 to identify best regional markets within 
                                 the state. However, based on the chart, 
                                 Steubenville-Weirton region has 
                                 experienced growth over 2013-2014 and
                                 may be a regional market to consider."),
                        helpText(""))
             ),
             tabPanel("DE", 
                      fluidRow(
                        plotOutput("plotde",click = "plotde"),
                        verbatimTextOutput("infode"),
                        helpText("Based on the chart above, we 
                                 can see that Wilmington is the largest
                                 regional mortgage market, although 
                                 it has been decreasing since 2012.  
                                 The chart also shows that 2012-2013
                                 data had many NULL regions for Delaware,
                                 whereas 2014 data has close to zero
                                 unidentified regions in the dataset."),
                        helpText(""))
             ),
             tabPanel("DC", 
                      fluidRow(
                        plotOutput("plotdc",click = "plotdc"),
                        verbatimTextOutput("infodc"),
                        helpText("The chart above simply shows the
                                 total loans in DC by year.  Since
                                 DC is all in one region, this chart
                                 isn't particularly helpful in 
                                 identifying specific regions within DC"),
                        helpText(""))
                  ),
             tabPanel("Data Quality", 
                      fluidRow(
                        h3(textOutput("caption")),
                        plotOutput("loanAmt",click = "info2"),
                        verbatimTextOutput("info2"),
                        selectInput("variable", "Variable:",
                                    list("Year" = "As_of_Year.x", 
                                         "State" = "State", 
                                         "Agency" = "Agency_Code")),
                        
                        checkboxInput("outliers", "Show outliers", TRUE),
                        helpText("The following interactive boxchart
           illustrates outliers in the 'Loan_Amount_000'
                                 column by different groups.  The boxchart presents
                                 the min, max, mean, and variance of the 'Loan_Amount_000'
                                 values.  This chart helps identify outlier 
                                 'Loan_Amounts_000' values and also tries to find which 
                                 variables are most correlated with these outliers.  
                                 Use the drop down menu on the left to view 
                                 'Loan_Amount_000' outliers by Year, State, and Agency Code.  
                                 Clicking on the boxplot will show the specific 
                                 'Loan_Amount_000' outlier values which can be
                                 investigated further to see if they are valid."),
                        helpText("Year Correlated Loan Amount Outliers: 
                                 2012 and  2013 had some outliers, whereas 2014 had 
                                 none.  For 2012, loan amounts above ~$609,000 should 
                                 be investigated.  For 2013, loan amounts above 
                                 ~$587,000 should be investigated."),
                        helpText("State Correlated Loan Amount Outliers: 
                                 Maryland and West Virginia had the most outliers, 
                                 whereas the other states had none.  Maryland loan 
                                 amounts greater than ~$584,000 should be investigated.
                                 West Virginia loan amounts greater than 
                                 ~$337,000 should be investigated"),
                        helpText("Agency Correlated Loan Amount Outliers: 
                                 Agencies belonging to Agency Code 5 and 9 contained
                                 all the outliers.  These agencies could have 
                                 misreported data and their values should be scrutinized"),
                        helpText("Columns to Validate: Along with the 
                                 'Loan_Amount_000' and 'Respondent_Name_TS' column, the
                                 'MSA_MD_Description' description column should be 
                                 cleaned.  This column had many Null values and the 
                                 metadata indicates it should be 1:1 with 'MSA_MD' code.
                                 This column is being used for a deep dive into the data, 
                                 specifically to identify regional opportunities.  The
                                 regional analysis will show many NA values.  If these 
                                 values were cleaned up it would provide better information
                                 for regional analysis.  The 'County_Code' and 
                                 'Census_Tract_Number' are also incredibly important 
                                 because they would allow more granular regional analysis
                                 and allow us to pair the data with census information.")
                        )
             )             
             
             
             )),
    width = 12))

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    ggplot(total_loans,aes(As_of_Year.x,sumy,fill=State))+
      geom_bar(stat="identity",position="stack")+
      scale_x_continuous("Year") +
      scale_y_continuous("Total Loan Amount ($1000)") +
      ggtitle("Conventional Conforming Mortgage Size by State")
  })
  
  output$plot3 <- renderPlot({
    ggplot(total_loans,aes(As_of_Year.x,sumy,fill=State))+
      geom_bar(stat="identity",position="stack")+
      scale_x_continuous("Year") +
      scale_y_continuous("Total Loan Amount ($1000)") +
      ggtitle("Conventional Conforming Mortgage Size by State")
  })
  
  output$info <- renderText({
    paste("Year = ", input$plot_click$x, 
          "\nTotal Loan Amount (in $1000s) = ", 
          input$plot_click$y)
  })
  
  # Data Quality Section
  
  # Get the variable to plot against from the user input
  formulaText <- reactive({
    paste("Loan_Amount_000 ~", input$variable)
  })
  
  # Return the appropriate label
  output$caption <- renderText({
    if(input$variable == "As_of_Year.x"){
      return("Loan Amount Outliers by Year")
    }
    if(input$variable == "State"){
      return("Loan Amount Outliers by State")
    }
    if(input$variable == "Agency_Code"){
      return("Loan Amount Outliers by Agency")
    }
  })
  
  # Generate a box plot against the requested variable
  output$loanAmt <- renderPlot({
    ylab_val <- "Loan Amount (in $1000)"
    if(input$variable == "As_of_Year.x") xlab_val <- "Year"
    if(input$variable == "State") xlab_val <- "State"
    if(input$variable == "Agency_Code") xlab_val <- "Agency Code"
    
    boxplot(as.formula(formulaText()), 
            data = data_cc,
            xlab = xlab_val,
            ylab = ylab_val,
            outline = input$outliers)
  })
  
  output$info2 <- renderText({
    paste("Loan Amount (in $1000s) = ", 
          input$info2$y)
  })
  
  # Maryland Specific Charts
  
  output$plotmd <- renderPlot({
    ggplot(total_loans_md,aes(As_of_Year.x,loans,fill=MSA_MD_Description))+
      geom_bar(stat="identity",position="dodge")+
      scale_x_continuous("Year") +
      scale_y_continuous("Total Loan Amount ($1000)") +
      ggtitle("Conventional Conforming Mortgage Size by Maryland Regions")
  })
  
  output$infomd <- renderText({
    paste("Year = ", input$plotmd$x, 
          "\nTotal Loan Amount (in $1000s) = ", 
          input$plotmd$y)
  })
  
  # Virginia Specific Charts
  
  output$plotva <- renderPlot({
    ggplot(total_loans_va,aes(As_of_Year.x,loans,fill=MSA_MD_Description))+
      geom_bar(stat="identity",position="dodge")+
      scale_x_continuous("Year") +
      scale_y_continuous("Total Loan Amount ($1000)") +
      ggtitle("Conventional Conforming Mortgage Size by Virginia Regions")
  })
  
  output$infova <- renderText({
    paste("Year = ", input$plotva$x, 
          "\nTotal Loan Amount (in $1000s) = ", 
          input$plotva$y)
  })
  
  # West Virginia Specific Charts
  
  output$plotwv <- renderPlot({
    ggplot(total_loans_wv,aes(As_of_Year.x,loans,fill=MSA_MD_Description))+
      geom_bar(stat="identity",position="dodge")+
      scale_x_continuous("Year") +
      scale_y_continuous("Total Loan Amount ($1000)") +
      ggtitle("Conventional Conforming Mortgage Size by West Virginia Regions")
  })
  
  output$infowv <- renderText({
    paste("Year = ", input$plotwv$x, 
          "\nTotal Loan Amount (in $1000s) = ", 
          input$plotwv$y)
  })
  
  # Washington DC Specific Charts
  
  output$plotdc <- renderPlot({
    ggplot(total_loans_dc,aes(As_of_Year.x,loans,fill=MSA_MD_Description))+
      geom_bar(stat="identity",position="dodge")+
      scale_x_continuous("Year") +
      scale_y_continuous("Total Loan Amount ($1000)") +
      ggtitle("Conventional Conforming Mortgage Size by Washington, DC Regions")
  })
  
  output$infodc <- renderText({
    paste("Year = ", input$plotdc$x, 
          "\nTotal Loan Amount (in $1000s) = ", 
          input$plotdc$y)
  })
  
  # Delaware Specific Charts
  
  output$plotde <- renderPlot({
    ggplot(total_loans_de,aes(As_of_Year.x,loans,fill=MSA_MD_Description))+
      geom_bar(stat="identity",position="dodge")+
      scale_x_continuous("Year") +
      scale_y_continuous("Total Loan Amount ($1000)") +
      ggtitle("Conventional Conforming Mortgage Size by Delaware Regions")
  })
  
  output$infode <- renderText({
    paste("Year = ", input$plotde$x, 
          "\nTotal Loan Amount (in $1000s) = ", 
          input$plotde$y)
  })
  
}

shinyApp(ui, server)



