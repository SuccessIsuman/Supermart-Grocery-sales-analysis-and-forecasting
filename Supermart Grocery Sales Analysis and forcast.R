
# installing the needed libraries 
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("cowplot", quietly = TRUE)) {
  install.packages("cowplot")
}
if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
if (!requireNamespace("tsibble", quietly = TRUE)) {
  install.packages("tsibble")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tseries", quietly = TRUE)) {
  install.packages("tseries")
}
if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}
if (!requireNamespace("Metrics", quietly = TRUE)) {
  install.packages("Metrics")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}


# Loading in the dataset

# using the read_csv to read in the dataset file

df <- read.csv("Supermart Grocery Sales - Retail Analytics Dataset.csv",header = T, stringsAsFactors=T)


#Checking for the structure of the dataset


# checking for the dimension of the dataset

dim(df)



# 1. Data cleaning



# Checking for rows with missing values

# Identify rows with missing values
rows_with_missing_values <- df[which(rowSums(is.na(df)) > 0), , drop = FALSE]

# Display rows with missing values
print(rows_with_missing_values)



# 2. Identifying duplicated rows


# Identify duplicated rows
duplicated_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), , drop = FALSE]

# Display duplicated rows
print(duplicated_rows)




# Function to calculate cost price
calculate_cost_price <- function(selling_price, profit, discount_percentage) {
  cost_price <- (selling_price - profit) / (1 - discount_percentage / 100)
  return(cost_price)
}

# Apply the cost price calculation to the entire DataFrame
df$cost_price <- calculate_cost_price(df$Sales, df$Profit, df$Discount)


# Extracting month from the order data so as to know sale per months


# checking for unique values of the order date column

# Checking unique values in a vector
unique_values <- unique(df$Order.Date)


#Looking through the order date columns it is observed that there different data format the "%m/%d/%Y" and the "%m-%d-%Y" hence a single conversion won't surfice.

library(lubridate)

# Specify the date formats
date_formats <- c("%m/%d/%Y", "%m-%d-%Y")

# Try to parse 'Order.Date' to Date class using parse_date_time
df$Order_date <- try(parse_date_time(df$Order.Date, orders = date_formats), silent = TRUE)

# Print the entries that failed to parse
failed_to_parse <- df[is.na(df$Order.Date), "Order.Date"]
print(failed_to_parse)



# Extract month and create a new 'month'

df$month <- months(df$Order_date)


# Extracting the Order year

# Extract year and create a new 'year'
df$year <- format(df$Order_date, "%Y")


# Extracting the Order year and month together

# Extract year and month together and create a new 'year_month'
df$year_month <- format(df$Order_date, "%Y-%m")



# Extracting the Order day for sales analysis

# Extract day and create a new 'day'
df$day <- format(df$Order_date, "%d")

# Extract the day names
df$day_names <- weekdays(df$Order_date)




# checking for missing rows

# Identify rows with missing values
rows_with_missing_values <- df[which(rowSums(is.na(df)) > 0), , drop = FALSE]

# Display rows with missing values
print(rows_with_missing_values)

          
 #        3. Exploratory Data Analysis
         
         
  #       Demand Analysis 
          
#Days with the highest orders 

library(dplyr)
library(ggplot2)

# Grouping data by 'day_name' and counting orders
grouped_data <- df %>%
  group_by(day_names) %>%
  summarise(total_orders = n())

# Sorting values before plotting
grouped_data <- arrange(grouped_data, total_orders)

# Plotting the results
ggplot(grouped_data, aes(x = reorder(day_names, total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Total Demand by Days", x = "Days", y = "Demand") +
  theme_minimal() +
  coord_flip()

# Generally orders are poor on Thursdays and Fridays while orders are highest on Tuesdays and good on Saturday, Sunday and Mondays.


# Orders per month of the year


# Grouping data by 'month' and counting orders
grouped_data <- df %>%
  group_by(month) %>%
  summarise(total_orders = n())

# Sorting values before plotting
grouped_data <- arrange(grouped_data, total_orders)

# Plotting the results
ggplot(grouped_data, aes(x = reorder(month, total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Total Demand by Month", x = "Months", y = "Demand") +
  theme_minimal() +
  coord_flip()



# Orders per year


# Grouping data by 'year' and counting orders
grouped_data <- df %>%
  group_by(year) %>%
  summarise(total_orders = n())

# Sorting values before plotting
grouped_data <- arrange(grouped_data, total_orders)

# Plotting the results
ggplot(grouped_data, aes(x = reorder(year, total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Total Demand by Year", x = "Years", y = "Demand") +
  theme_minimal() 


# Orders per month of the year

# Create a horizontal bar plot of orders per month of the year in descending order
library(ggplot2)
library(dplyr)

# Assuming 'df' is your data frame
df %>%
  ggplot(aes(x = reorder(factor(year_month), -table(year_month)))) +
  geom_bar(stat = "count", fill = "brown") +
  labs(title = "Orders per Month of the year", x = "Month of the year", y = "Number of Orders") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




#  Value counting the year_month
value_counts <- table(df$year_month)

# Convert the result to a data frame for better visualization 
value_counts_df <- as.data.frame(value_counts)

## Create the plot
plot <- ggplot(value_counts_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Orders per Month of the year", x = "Month of the year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  theme_minimal()

print(plot)
# Save the plot with the desired figure size
#ggsave("output_plot.png", plot, width = 8, height = 10)
 



# Total sales by region


# Grouping data by 'month' and Summing Sales
grouped_data <- df %>%
  group_by(Region) %>%
  summarise(total_sales = sum(Sales))

# Sorting values before plotting
grouped_data <- arrange(grouped_data, total_sales)

# Plotting the results
ggplot(grouped_data, aes(x = reorder(Region, total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Sales by Region", x = "Region", y = "Total Sales") +
  theme_minimal() 


## Group by 'Region' and 'Year' columns and sum the 'Sales' column
grouped_data <- aggregate(Sales ~ Region + year, data = df, sum)

# Plot a bar chart of the grouped data
bar_chart <- ggplot(grouped_data, aes(x = Region, y = Sales, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales by Region and Year", x = "Region", y = "Total Sales", fill = "Year") +
  theme_minimal()



# In what city is the sales highest


# Group by 'City' column and sum the 'Sales' column
grouped_data2 <- aggregate(Sales ~ City, data = df, sum)

# Plot a bar chart of the grouped data
bar_chart <- ggplot(grouped_data2, aes(x = City, y = Sales)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Total Sales by City", x = "City", y = "Total Sales") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Group by 'Region' and 'City' columns and sum the 'Sales' column
grouped_data3 <- aggregate(Sales ~ Region + City, data = df, sum)

# Plot a bar chart of the grouped data
bar_chart <- ggplot(grouped_data3, aes(x = City, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales by City and Region", x = "City", y = "Total Sales", fill = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter the dataset where 'Region' equals 'North'
north_data <- subset(df, Region == "North")

## Group by date and summarize sales and profit
summarized_data <- df %>%
  group_by(Order_date) %>%
  summarize(total_sales = sum(Sales), total_profit = sum(Profit))

# Plot a line graph of total sales and profit over time
line_plot <- ggplot(summarized_data, aes(x = Order_date)) +
  geom_line(aes(y = total_sales, color = "Total Sales"), size = 1) +
  geom_line(aes(y = total_profit, color = "Total Profit"), size = 1) +
  labs(title = "Total Sales and Profit Over Time", x = "Date", y = "Amount") +
  scale_color_manual(values = c("Total Sales" = "blue", "Total Profit" = "red")) +
  theme_minimal()


# Convert 'year' column to numeric
df$year <- as.numeric(as.character(df$year))

# Group by date and summarize sales and profit
summarized_year_data <- df %>%
  group_by(year) %>%
  summarize(total_sales = sum(Sales), total_profit = sum(Profit))

# Sort the dataframe by year in descending order
summarized_year_data <- summarized_year_data[order(-summarized_year_data$year), ]

# Line plot with sales and profit
ggplot(summarized_year_data, aes(x = year)) +
  geom_line(aes(y = total_sales, color = "Sales"), size = 1) +
  geom_line(aes(y = total_profit, color = "Profit"), linetype = "dashed", size = 1) +
  labs(title = "Yearly Sales and Profit Trend",
       x = "Year", y = "Value") +
  scale_color_manual(values = c("Sales" = "blue", "Profit" = "red")) +
  theme_minimal()

# Convert 'year' column to numeric
# df$year_month <- as.numeric(as.character(df$year_month))

# Group by date and summarize sales and profit
summarized_month_data <- df %>%
  group_by(year_month) %>%
  summarize(total_sales = sum(Sales), total_profit = sum(Profit))

# Label encoding the year_month column
# Convert to factor
summarized_month_data$year_month <- factor(summarized_month_data$year_month)

# Extract the integer codes
summarized_month_data$year_month <- as.integer(summarized_month_data$year_month)


# Line plot with sales and profit
ggplot(summarized_month_data, aes(x = year_month)) +
  geom_line(aes(y = total_sales, color = "Sales"), size = 1) +
  geom_line(aes(y = total_profit, color = "Profit"), size = 1) +
  labs(title = "Monthly Sales and Profit Trend",
       x = "Months of the year", y = "Value") +
  scale_color_manual(values = c("Sales" = "blue", "Profit" = "red")) +
  theme_minimal()


# Grouping data by 'month' and Summing Sales
grouped_data <- df %>%
  group_by(Category) %>%
  summarise(total_order = n())

# Sorting values before plotting
grouped_data <- arrange(grouped_data, total_order)

# Plotting the results
ggplot(grouped_data, aes(x = reorder(Category, total_order), y = total_order)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Demand by product Category", x = "Product Category", y = "Orders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


library(dplyr)

# Group by 'Region' and 'Category' columns and count the occurrences
grouped_data <- df %>%
  group_by(Region, Category) %>%
  summarise(count = n(), .groups = 'drop')

# Plot a bar chart of the grouped data
# Bar plot with values on stacked bars
ggplot(grouped_data, aes(x = Region, y = count, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black", size = 3) +  # Add text labels
  labs(title = "Demand by Product Category by Region", x = "Region", y = "Orders") +
  theme_minimal()



 # Value counting the Sub.category column
value_count <- table(df$Sub.Category)

# Convert the result to a data frame for better visualization 
value_count_df <- as.data.frame(value_count)

# Sort the data frame by Freq in descending order
value_count_df <- value_count_df[order(value_count_df$Freq), ]

# Plot a bar chart of the resulting dataframe
bar_chart <- ggplot(value_count_df, aes(x = factor(Var1, levels = unique(Var1)), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Demand by Product", x = "Product", y = "Orders") +
  theme_minimal() +
  coord_flip()

print(bar_chart)


# Orders by Sub.Category of products by Region


# Group by 'Region' and 'Category' columns and count the occurrences
grouped_data <- df %>%
  group_by(Region, Sub.Category) %>%
  summarise(count = n(), .groups = 'drop')



# Plot a bar chart of the grouped data
# Bar plot with values on stacked bars
ggplot(grouped_data, aes(x = Sub.Category, y = count, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black", size = 3) +  # Add text labels
  labs(title = "Orders by Sub.Category by Region", x = "Sub.Category", y = "Orders") +
  theme_minimal() + coord_flip()


# Group by 'product' and sum the 'sales' column
grouped_data <- aggregate(Sales ~ Category, data = df, sum)

# Plot a bar chart of the grouped data
bar_chart <- ggplot(grouped_data, aes(x = Category, y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by Product", x = "Product", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the bar chart
print(bar_chart)

# Group by 'product' and sum the 'sales' column
grouped_data <- aggregate(Sales ~ Sub.Category, data = df, sum)

# Plot a bar chart of the grouped data
bar_chart <- ggplot(grouped_data, aes(x = Sub.Category, y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by Product", x = "Product", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Display the bar chart
print(bar_chart)



 


#                        Time Series Analysis (Sales Forcasting) 
                        
                        
                        

 #   Data pre processing for Sales Forecasting


# selecting the subset of data relevant for forecasting
Forcast_df <- df[, c("year_month", "Sales")]


# Sort the data frame by the Order_date column in ascending order
Forcast_df <- Forcast_df[order(Forcast_df$year_month), ]


#Converting the date to a date object

Forcast_df$year_month <- paste(Forcast_df$year_month, '-1', sep = '')

# Convert the 'date' column to Date type
Forcast_df$year_month <- as.Date(Forcast_df$year_month)

# Isolating the range of Years present in the dataset

first_date <- Forcast_df$year_month[1]
last_date <- Forcast_df$year_month[length(Forcast_df$year_month)]



library(dplyr)

# Grouping by data to get a unique range of date and summing the sales value
data <- Forcast_df %>%
  group_by(year_month) %>%
  summarise(Sales = sum(Sales))





# ploting the trend of sales since 2015 to 2018



ggplot(data, aes(x = year_month)) +
  geom_line(aes(y = Sales, color = "Sales"), size = 1) +
  labs(title = "Sales Trend since 2015 to 2018",
       x = "Year", y = "Sales") +
  scale_color_manual(values = c("Sales" = "red")) +
  theme_minimal()





       
             
       #       Training AUTO ARIMA Model with the raw seasonal data
              
              
# 1. Split the dataset into training and testing

# load necessary packages
library(forecast)
library(Metrics)

# Convert 'Date' to a Date format
data$year_month <- as.Date(data$year_month)

# Split the data into training and testing sets (e.g., 80% training, 20% testing)
split_index <- floor(0.8 * nrow(data))
train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]


#      Training an ordinary linear regression model
        

# Fit a linear regression model
linear_model <- lm(Sales ~ year_month, data = train_data)

# Print the summary of the model
summary(linear_model)

# Make predictions on the testing set

# Make predictions using the model
lm_predictions <- predict(linear_model, newdata = test_data)

# Print the first few predictions
head(lm_predictions)


# Evaluation of linear regression model

# Evaluate the model's performance on the testing set
mae_value <- mae(test_data$Sales, lm_predictions)
mse_value <- mse(test_data$Sales, lm_predictions)
rmse_value <- rmse(test_data$Sales, lm_predictions)
mape_value <- mape(test_data$Sales, lm_predictions)

# Print the evaluation metrics
cat("Mean Absolute Error (MAE):", mae_value, "\n")
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape_value, "\n")

# Optionally, plot the actual vs. predicted values for visualization
plot(test_data$year_month, test_data$Sales, type = "l", col = "blue", ylab = "Sales", xlab = "Date", main = "Linear Regression Model Evaluation")
lines(test_data$year_month, lm_predictions, col = "red", lty = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1:2)



# Training A Random Forest Model



#  load the required library
library(randomForest)
rf_data<-data
# Create lag features for temporal patterns
rf_data$lag_1 <- lag(rf_data$Sales, 1)
rf_data$lag_2 <- lag(rf_data$Sales, 2)
# Add more lag features as needed

# Remove rows with NAs resulting from lag creation
rf_data <- na.omit(rf_data)

# Split the data into training and testing sets
train_size <- 0.8
train_index <- 1:(train_size * nrow(rf_data))
train_data <- rf_data[train_index, ]
test_data <- rf_data[-train_index, ]

# Train a random forest regressor model
rf_model <- randomForest(Sales ~ lag_1 + lag_2, data = train_data)

print(summary(rf_model))


# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Print the first few predictions
head(rf_predictions)

# Evaluation of the Random forest regressor

# Evaluate the model's performance on the testing set
mae_value <- mae(test_data$Sales, rf_predictions)
mse_value <- mse(test_data$Sales, rf_predictions)
rmse_value <- rmse(test_data$Sales, rf_predictions)
mape_value <- mape(test_data$Sales, rf_predictions)

# Print the evaluation metrics
cat("Mean Absolute Error (MAE):", mae_value, "\n")
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape_value, "\n")

# Optionally, plot the actual vs. predicted values for visualization
plot(test_data$year_month, test_data$Sales, type = "l", col = "blue", ylab = "Sales", xlab = "Date", main = "Random forest regressor Model Evaluation")
lines(test_data$year_month, rf_predictions, col = "red", lty = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1:2)


# Training An Auto ARIMA model


# 2. Train the ARIMA model on the training set by first creating a time series of 12 data points i.e 12 month (1 year)


library(tseries)

# Create a time series object
ts_data <- ts(train_data$Sales, frequency = 12)  # Assuming monthly data, adjust frequency accordingly

# Train an ARIMA model
arima_model <- auto.arima(ts_data)

# Print the summary of the ARIMA model
print(summary(arima_model))



# 3. Make prediction of the testing set

# Make predictions on the testing set

forecast_values <- forecast(arima_model, h = nrow(test_data))

# Print the forecast values
print(forecast_values)


# 4. Extract the prediction values from the prediction report

# Extract the predicted values
predicted_values <- as.numeric(forecast_values$mean)

print(predicted_values)


# 5. Evaluate the performance of the model on the testing set by calculating the various evaluation metrices


# Evaluate the model's performance on the testing set
mae_value <- mae(test_data$Sales, predicted_values)
mse_value <- mse(test_data$Sales, predicted_values)
rmse_value <- rmse(test_data$Sales, predicted_values)
mape_value <- mape(test_data$Sales, predicted_values)

# Print the evaluation metrics
cat("Mean Absolute Error (MAE):", mae_value, "\n")
cat("Mean Squared Error (MSE):", mse_value, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape_value, "\n")



# plot the Actual values and the predicted values 

# Optionally, plot the actual vs. predicted values for visualization
plot(test_data$year_month, test_data$Sales, type = "l", col = "blue", ylab = "Sales", xlab = "Date", main = "ARIMA Model Evaluation")
lines(test_data$year_month, predicted_values, col = "red", lty = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1:2)







# Making future focast with the best trained model

# 1. Make prediction (forecast) for the next 12 data points i.e for the next 12 months

# Make predictions (example: next 12 periods)
forecast_values_12 <- forecast(arima_model, h = 12)



# Print the forecast values
print(forecast_values_12)



# 2. Ploting the future forecast of the ARIMA model

# Plot the forecast with legend
plot(forecast_values_12, main = "ARIMA Forecast for Next 24 Months (2 years)", xlab = "Date", ylab = "Sales")
legend("topright", legend = c("Forecast"), col = c("blue"), lty = 1)