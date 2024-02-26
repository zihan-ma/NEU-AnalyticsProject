# Load required libraries
library(sparklyr)
library(readr)
library(readxl)
library(ggplot2)
# library(DBI)

# Connect to Spark
sc <- spark_connect(master = "local", version = "3.5")

# Load datasets
HPI_AT_BDL_ZIP3_csv <- read_csv("DataSets/HPI_AT_BDL_ZIP3.csv")
HPI_AT_BDL_ZIP3_excel <- read_excel("DataSets/HPI_AT_BDL_ZIP3.xlsx")
HPI_AT_BDL_ZIP5_csv <- read_csv("DataSets/HPI_AT_BDL_ZIP5.csv")
HPI_AT_BDL_ZIP5_excel <- read_excel("DataSets/HPI_AT_BDL_ZIP5.xlsx")
Zip_Zhvi_Summary_AllHomes <- read_csv("DataSets/Zip_Zhvi_Summary_AllHomes.csv")

# Transfer datasets to Spark
HPI_AT_BDL_ZIP3_csv <- copy_to(sc, HPI_AT_BDL_ZIP3_csv)
HPI_AT_BDL_ZIP3_excel <- copy_to(sc, HPI_AT_BDL_ZIP3_excel)
HPI_AT_BDL_ZIP5_csv <- copy_to(sc, HPI_AT_BDL_ZIP5_csv)
HPI_AT_BDL_ZIP5_excel <- copy_to(sc, HPI_AT_BDL_ZIP5_excel)
Zip_Zhvi_Summary_AllHomes <- copy_to(sc, Zip_Zhvi_Summary_AllHomes)

# Sample queries for verification
# dbGetQuery(sc, "SELECT count(*) FROM Zip_Zhvi_Summary_AllHomes")
# dbGetQuery(sc, "SELECT count(*) FROM HPI_AT_BDL_ZIP5_csv")

# Exploratory Data Analysis using SparklyR

# -----1. Summary Statistics-----

# Copy datasets to Spark with explicit table names
HPI_AT_BDL_ZIP3_csv_spark <- copy_to(sc, HPI_AT_BDL_ZIP3_csv, "HPI_AT_BDL_ZIP3_csv", overwrite = TRUE)
HPI_AT_BDL_ZIP5_csv_spark <- copy_to(sc, HPI_AT_BDL_ZIP5_csv, "HPI_AT_BDL_ZIP5_csv", overwrite = TRUE)
Zip_Zhvi_Summary_AllHomes_spark <- copy_to(sc, Zip_Zhvi_Summary_AllHomes, "Zip_Zhvi_Summary_AllHomes", overwrite = TRUE)

# Register DataFrames as temporary SQL views
sdf_register(HPI_AT_BDL_ZIP3_csv, "hpi_at_bdl_zip3_csv_view")
sdf_register(HPI_AT_BDL_ZIP5_csv, "hpi_at_bdl_zip5_csv_view")
sdf_register(Zip_Zhvi_Summary_AllHomes, "zip_zhvi_summary_allhomes_view")

# Detailed Summary Statistics
summary_stats_zip3 <- dbGetQuery(sc, "SELECT AVG(HPI) as AvgHPI_ZIP3, STDDEV(HPI) as StdHPI_ZIP3 FROM hpi_at_bdl_zip3_csv_view")
summary_stats_zip5 <- dbGetQuery(sc, "SELECT AVG(HPI) as AvgHPI_ZIP5, STDDEV(HPI) as StdHPI_ZIP5 FROM hpi_at_bdl_zip5_csv_view")
summary_stats_zhvi <- dbGetQuery(sc, "SELECT AVG(Zhvi) as AvgZhvi, STDDEV(Zhvi) as StdZhvi FROM zip_zhvi_summary_allhomes_view")

# Trend Analysis
trend_analysis_zip3 <- dbGetQuery(sc, "SELECT Year, AVG(HPI) as AvgHPI FROM hpi_at_bdl_zip3_csv_view GROUP BY Year ORDER BY Year")
trend_analysis_zip5 <- dbGetQuery(sc, "SELECT Year, AVG(HPI) as AvgHPI FROM hpi_at_bdl_zip5_csv_view GROUP BY Year ORDER BY Year")

# Comparison Between ZIP Code Datasets
comparison_zip3_zip5 <- dbGetQuery(sc, "SELECT zip3.Year, AVG(zip3.HPI) as AvgHPI_ZIP3, AVG(zip5.HPI) as AvgHPI_ZIP5 FROM hpi_at_bdl_zip3_csv_view zip3 JOIN hpi_at_bdl_zip5_csv_view zip5 ON zip3.Year = zip5.Year GROUP BY zip3.Year ORDER BY zip3.Year")

# Print the summary statistics and trend analysis results
print(summary_stats_zip3)
print(summary_stats_zip5)
print(summary_stats_zhvi)
print(trend_analysis_zip3)
print(trend_analysis_zip5)

# Combining both datasets into one plot
ggplot() +
  # Add line for three-digit ZIP codes
  geom_line(data = trend_analysis_zip3, aes(x = Year, y = AvgHPI), color = "blue") +
  geom_point(data = trend_analysis_zip3, aes(x = Year, y = AvgHPI), color = "blue") +
  # Add line for five-digit ZIP codes
  geom_line(data = trend_analysis_zip5, aes(x = Year, y = AvgHPI), color = "red") +
  geom_point(data = trend_analysis_zip5, aes(x = Year, y = AvgHPI), color = "red") +
  theme_minimal() +
  labs(title = "Trend Analysis of Average HPI for Three-Digit vs. Five-Digit ZIP Codes",
       x = "Year",
       y = "Average HPI",
       caption = "Data Source: Annual House Price Indexes") +
  scale_color_manual(values = c("Three-Digit ZIP Codes" = "blue", "Five-Digit ZIP Codes" = "red")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "ZIP Code Type"))

print(comparison_zip3_zip5)

# Plotting the comparison between ZIP code datasets
ggplot(comparison_zip3_zip5, aes(x = Year)) +
  geom_line(aes(y = AvgHPI_ZIP3, color = "Three-Digit ZIP Codes")) +
  geom_point(aes(y = AvgHPI_ZIP3, color = "Three-Digit ZIP Codes")) +
  geom_line(aes(y = AvgHPI_ZIP5, color = "Five-Digit ZIP Codes")) +
  geom_point(aes(y = AvgHPI_ZIP5, color = "Five-Digit ZIP Codes")) +
  theme_minimal() +
  labs(title = "Comparison of Average HPI: Three-Digit vs. Five-Digit ZIP Codes",
       x = "Year",
       y = "Average HPI",
       caption = "Data Source: Annual House Price Indexes") +
  scale_color_manual(values = c("Three-Digit ZIP Codes" = "blue", "Five-Digit ZIP Codes" = "red")) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "ZIP Code Type"))

# -----2. Visualization-----

# 1. Visualize Detailed Summary Statistics for HPI
summary_detailed_hpi <- dbGetQuery(sc, "SELECT 'Average' as Statistic, AVG(HPI) as Value FROM hpi_at_bdl_zip3_csv_view
                                      UNION ALL
                                      SELECT 'Minimum', MIN(HPI) FROM hpi_at_bdl_zip3_csv_view
                                      UNION ALL
                                      SELECT 'Maximum', MAX(HPI) FROM hpi_at_bdl_zip3_csv_view
                                      UNION ALL
                                      SELECT 'Standard Deviation', STDDEV(HPI) FROM hpi_at_bdl_zip3_csv_view")
ggplot(summary_detailed_hpi, aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = "Summary Statistics for HPI", x = "Statistic", y = "Value")

summary_detailed_hpi

# 2. Enhanced Trend Analysis Visualization
hpi_trend <- dbGetQuery(sc, "SELECT Year, AVG(HPI) as AvgHPI FROM hpi_at_bdl_zip3_csv_view GROUP BY Year ORDER BY Year")
ggplot(hpi_trend, aes(x = Year, y = AvgHPI)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Average HPI Over Years", x = "Year", y = "Average HPI")

hpi_trend

# 3. Correlation Visualization
correlation_data <- dbGetQuery(sc, "SELECT Year, HPI FROM hpi_at_bdl_zip3_csv_view WHERE HPI IS NOT NULL AND Year IS NOT NULL")
correlation_data$Year <- as.numeric(correlation_data$Year)
correlation_data$HPI <- as.numeric(correlation_data$HPI)
correlation_result <- cor(correlation_data$Year, correlation_data$HPI, use = "complete.obs")

summary(correlation_data)
correlation_result

# Scatter Plot for Correlation
ggplot(correlation_data, aes(x = Year, y = HPI)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'blue') +
  theme_minimal() +
  labs(title = paste("Correlation between Year and HPI: ", round(correlation_result, 2)), x = "Year", y = "HPI")









