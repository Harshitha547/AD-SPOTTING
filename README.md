# AD SPOTTING 📊  
**A Data-Driven Shiny Dashboard to Explore Advertisement Performance**

## 🔍 About the Project

**Ad Spotting** is an interactive web application built using R and Shiny that helps analyze and visualize the performance of advertisements across different cities and market types. By leveraging real-world ad data, this dashboard enables marketers, analysts, and business teams to extract insights such as top-performing regions, ad cost efficiency, and sales patterns.

---

## 📂 Contents

- `AD SPOTTING .R` – The main R script that launches the Shiny dashboard and performs data analysis.
- `AD SPOTTING DATA SET.csv` – The dataset used, containing ad cost, clicks, sales, and market-related fields.

---

## 🧠 What This Dashboard Offers

- ✅ **Interactive Filtering** – Filter data by market type (Urban, Rural, Semi-Urban).
- 📊 **Dynamic Visualizations** – Histogram, bar charts, scatter plots, and boxplots for easy analysis.
- 💰 **Top City Insights** – Displays cities with the highest ad spend and maximum sales.
- 🔍 **Clustering (K-Means)** – Segments cities based on advertisement performance.
- 🔥 **Heatmap Analysis** – Visual correlation between sales, costs, and clicks.

---

## ⚙️ How to Use

### 1. Requirements

Ensure you have R installed along with these packages:
```r
install.packages(c("shiny", "ggplot2", "dplyr", "readr"))
