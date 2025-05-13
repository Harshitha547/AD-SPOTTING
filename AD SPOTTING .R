# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Read dataset correctly
df <- read.csv("D:/AD SPOTTING/AD SPOTTING DATA SET.csv", stringsAsFactors = FALSE)
df
colnames(df)
# Ensure numeric columns are correctly formatted
df$Sales <- as.numeric(df$Sales)
df$Sales
df$Ad_Cost_Per_Click <- as.numeric(df$Ad_Cost_Per_Click)
df$Ad_Cost_Per_Click
# Check for structure and NA values
str(df)
summary(df)

# Outlier removal using IQR
Q1 <- quantile(df$Ad_Cost_Per_Click, 0.25, na.rm = TRUE)
Q1
Q3 <- quantile(df$Ad_Cost_Per_Click, 0.75, na.rm = TRUE)
Q3
IQR <- Q3 - Q1
IQR

df <- df %>%
  filter(Ad_Cost_Per_Click >= (Q1 - 1.5 * IQR) & Ad_Cost_Per_Click <= (Q3 + 1.5 * IQR))
df
# Summary statistics
df %>% summarise(
  Variance_Sales = var(Sales, na.rm = TRUE),
  SD_Sales = sd(Sales, na.rm = TRUE),
  Variance_Ad_Cost_Per_Click = var(Ad_Cost_Per_Click, na.rm = TRUE),
  SD_Ad_Cost_Per_Click = sd(Ad_Cost_Per_Click, na.rm = TRUE)
)

# Barplot: Sales by Platform
colors <- rainbow(length(unique(df$Platform)))
barplot(
  df$Sales,
  names.arg = df$Platform,
  horiz = FALSE,
  col = colors,
  main = "Sales by Advertising Platform",
  xlab = "Platform",
  ylab = "Total Sales",
  las = 2
)

# K-Means Clustering
k_clusters <- kmeans(df[, c("Ad_Cost_Per_Click", "Sales")], centers = 4)
df$Cluster <- as.factor(k_clusters$cluster)

ggplot(df, aes(x = Ad_Cost_Per_Click, y = Sales, color = Cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ Market_Type) +
  labs(title = "Market Segmentation (K-Means Clustering)", x = "Ad Cost Per Click", y = "Sales") +
  theme_minimal()

# Sales by Market Type
market_sales <- aggregate(Sales ~ Market_Type, data = df, sum)
barplot(market_sales$Sales, names.arg = market_sales$Market_Type, col = "coral",
        main = "Sales by Market Type", xlab = "Market Type", ylab = "Total Sales", las = 2)

# Scatter Plot: Ad Cost vs. Sales
plot(df$Ad_Cost_Per_Click, df$Sales, col = "blue", pch = 19,
     xlab = "Ad Cost Per Click", ylab = "Sales", main = "Ad Cost Per Click vs Sales")
abline(lm(Sales ~ Ad_Cost_Per_Click, data = df), col = "red", lwd = 2)
abline
# Boxplot: Ad Cost Per Click by Market Type
boxplot(df$Ad_Cost_Per_Click ~ df$Market_Type, col = "lightblue",
        main = "Ad Cost Per Click by Market Type", xlab = "Market Type", ylab = "Ad Cost Per Click")

# Histogram: Ad Cost Per Click Distribution
hist(df$Ad_Cost_Per_Click, col = "green", main = "Distribution of Ad Cost Per Click",
     xlab = "Ad Cost Per Click", breaks = 8)

# Pie Chart: Market Share by Platform
platform_sales <- aggregate(Sales ~ Platform, data = df, sum)
pie(platform_sales$Sales, labels = paste(platform_sales$Platform,
                                         round(100 * platform_sales$Sales / sum(platform_sales$Sales), 1), "%"),
    col = colors, main = "Market Share by Platform")

# Correlation Heatmap
cor_matrix <- cor(df[, sapply(df, is.numeric)], use = "complete.obs")
heatmap(cor_matrix, col = colorRampPalette(c("blue", "white", "red"))(20),
        scale = "none", main = "Correlation Heatmap",
        xlab = "Variables", ylab = "Variables")

# Barplot: Best Cities for Ads
city_ad_spend <- aggregate(Ad_Cost_Per_Click ~ City, data = df, sum, na.rm = TRUE)
city_ad_spend <- city_ad_spend[order(-city_ad_spend$Ad_Cost_Per_Click), ]
bar_positions <- barplot(city_ad_spend$Ad_Cost_Per_Click, names.arg = city_ad_spend$City, col = heat.colors(nrow(city_ad_spend)),
                         main = "Best Cities for Ads", las = 2, border = NA,
                         xlab = "Cities", ylab = "Total Ad Cost")
text(bar_positions, city_ad_spend$Ad_Cost_Per_Click,
     labels = round(city_ad_spend$Ad_Cost_Per_Click, 2), pos = 3, cex = 0.8, col = "black")

# Identify the city with the highest ad spend
city_ad_spend <- aggregate(Ad_Cost_Per_Click ~ City, data = df, sum)
city_ad_spend
best_city <- city_ad_spend[which.max(city_ad_spend$Ad_Cost_Per_Click), "City"]
best_city
# Identify the city benefiting the most (highest Sales)
city_sales <- aggregate(Sales ~ City, data = df, sum)
city_sales
best_benefitting_city <- city_sales[which.max(city_sales$Sales), "City"]
best_benefitting_city

# Improved Shiny App for Ad Spotting Dashboard
ui <- fluidPage(
  titlePanel("Ad Spotting Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("marketSegment", "Select Market Segment:",
                  choices = unique(df$Market_Type), selected = unique(df$Market_Type)[1]),
      hr(),
      h4("Insights:"),
      textOutput("topAdCity"),
      textOutput("topSalesCity"),
      textOutput("cityClassification")
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Server part
server <- function(input, output) {
  
  # Filter data based on selected Market Type
  filteredData <- reactive({
    df %>% filter(Market_Type == input$marketSegment)
  })
  
  # Find the city with the highest ad spending
  output$topAdCity <- renderText({
    data <- filteredData()
    if(nrow(data) == 0) return("No data available for this market segment")
    
    top_city <- data %>% group_by(City) %>% summarise(Total_Ad_Spend = sum(Ad_Cost_Per_Click)) %>% 
      arrange(desc(Total_Ad_Spend)) %>% slice(1)
    
    paste("📢 City with Highest Ad Spending: ", top_city$City, "($", round(top_city$Total_Ad_Spend, 2), ")")
  })
  
  # Find the city with the highest sales
  output$topSalesCity <- renderText({
    data <- filteredData()
    if(nrow(data) == 0) return("No data available for this market segment")
    
    top_sales_city <- data %>% group_by(City) %>% summarise(Total_Sales = sum(Sales)) %>% 
      arrange(desc(Total_Sales)) %>% slice(1)
    
    paste("💰 Most Beneficial City (Highest Sales): ", top_sales_city$City, "($", round(top_sales_city$Total_Sales, 2), ")")
  })
  
  # Show city classification (Rural, Urban, Semi-Urban)
  output$cityClassification <- renderText({
    data <- filteredData()
    if(nrow(data) == 0) return("No data available for this market segment")
    
    city_classification <- data %>% select(City, Market_Type) %>% unique()
    city_list <- paste(city_classification$City, "(", city_classification$Market_Type, ")", collapse = ", ")
    
    paste("🏙️ City Classification: ", city_list)
  })
  
  # Render scatter plot
  output$scatterPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = Ad_Cost_Per_Click, y = Sales)) +
      geom_point(color = "blue", size = 3) +
      theme_minimal() +
      labs(title = paste("Sales vs Ad Cost Per Click (Market:", input$marketSegment, ")"),
           x = "Ad Cost Per Click", y = "Sales")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)