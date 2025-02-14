# To clear the environment
rm(list=ls())

pacman::p_load(
  tidyverse, dplyr, ggplot2, tidyr, stringr, zoo, scales, readr, rvest,
  randomForest,zipcodeR, lattice, foreach, caret, tidytext, wordcloud, syuzhet,
  fmsb, parallel, corrplot, scatterplot3d
)

#------------Import the data set and present few rows------------
sale_report = read.csv("D:/DA_SEM_1/Intro Data Sci R-Python/Final Project/Online Sale Report.csv", header = TRUE, stringsAsFactors = FALSE)
head(sale_report)

#------------Data Quality Checks------------
# Check the datatype of each columns
str(sale_report)

# Summary of the data
print(summary(sale_report))

# Unique values for Sale's Category
unique(sale_report$Category)

# Unique values for Sale's Currency
unique(sale_report$currency)

# Unique values of Size
unique(sale_report$Size)

unique(sale_report$fulfilment)

# Check the duplicate values
sum(duplicated(sale_report))

# Check for null values
colSums(is.na(sale_report))

# Print to total row and column count
nrow(sale_report)
length(sale_report[])

#------------Data Cleaning------------

# Removing columns as it not required
sale_report <- sale_report %>%
  select(-'ASIN', -'ship.postal.code', -'promotion.ids', -'fulfilled.by', -'Unnamed..22')

# Rename column name 
sale_report <- sale_report %>%
  rename(
    Order_ID = orderid,
    Date = date,
    Status = status,
    Fulfilment = fulfilment,
    Age = age,
    Sales_Channel = saleschannel,
    Ship_Service_Level = ship.service.level,
    Courier_Status = courierstatus,
    Qty = pices,
    Currency = currency,
    Actual_Price = actual.price,
    Ship_City = ship.city,
    Ship_State = ship.state,
    Ship_Country = ship.country,
    Review = reviewtext
  )

# Convert specific columns to numeric if they are not already numeric
sale_report$Amount <- as.numeric(as.character(sale_report$Amount))
sale_report$Actual_Amount <- as.numeric(as.character(sale_report$Actual_Price))

# Number of observations before removing duplicate order ID
nrow(sale_report)

# Check duplicate based on order ID 
sale_report <- sale_report %>% 
  distinct(Order_ID, .keep_all = TRUE)

# Number of observations after removing duplicate order ID
nrow(sale_report)

# Filling empty with 'Unknown' for all variables
sale_report[sale_report == ""] <- NA

sale_report <- sale_report[complete.cases(sale_report), ]

# Standardize text data - trimming and convert to lowercase
sale_report <- sale_report %>%
  mutate(across(where(is.character), ~str_trim(.))) %>%
  mutate(across(where(is.character), ~str_to_lower(.)))

# Remove the duplicates from data
sale_report <- sale_report[!duplicated(sale_report), ]

# Check for duplicates
nrow(sale_report[duplicated(sale_report), ])

# Check for the null or empty value
colSums(is.na(sale_report) | is_empty(sale_report))

#---Check unique value because it contain empty value before
# Unique values for Sale's Category after cleaning data
unique(sale_report$Category)

unique(sale_report$B2B)

# Unique values for Sale's Currency after cleaning data
unique(sale_report$Currency)

# Unique values of Size after cleaning data
unique(sale_report$Size)

# Print to total row and column count
nrow(sale_report)
length(sale_report[])

#------------Descriptive Statistics and basic aggregations------------

#Create a new column based on existing data-> Profit = Amount - Actual Price
sale_report$Profit = as.numeric(sale_report$Amount - sale_report$Actual_Price)
sale_report$Profit <- as.numeric(as.character(sale_report$Profit))

# Aggregations
# Total sales by category
sales_by_category <- sale_report %>%
  group_by(Category) %>%
  summarise(
    Total_Sales = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

sales_by_category

# Orders by fulfillment status
orders_by_status <- sale_report %>%
  group_by(Status) %>%
  summarise(Count = n())

orders_by_status

# Central Tendency Summary Statistics and standard deviation
# Summary statistics for numerical variables
numeric_cols <- c("Age", "Qty", "Amount", "Actual_Price", "Profit")
summary(sale_report[numeric_cols])

print(paste('Standard Deviation for Quantity: ', sd(sale_report$Qty)))

print(paste('Standard Deviation for Billing Amount: ', sd(sale_report$Amount)))

# Grouping And Aggregation Data for sales channel
avg_billing_amount <- sale_report %>%
  group_by(Sales_Channel) %>%
  summarise(
    avg_bill = mean(Amount)
  )
avg_billing_amount

#Print total amount by Category
print(aggregate(Amount ~ Category, data = sale_report, sum))

#------------Data Visualization------------

# Histogram of Order Amounts
ggplot(sale_report, aes(x = Amount)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Order Amounts", x = "Amount", y = "Frequency")

# Box plot for fulfilment(provider) with amount
ggplot(sale_report, aes(x = Fulfilment, y = Amount, fill = Fulfilment)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  scale_fill_brewer(palette = "Set2") + # Use a color palette for clarity
  labs(title = "Boxplot for Fulfilment with respect of Amount", 
       x = "Fulfilment(Providers)", y = "Amount") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar Plot for product amount by category
ggplot(sale_report, aes(x = Category, y = Amount, fill = Category)) +
  geom_bar(stat = "identity") +
  ggtitle("Product Price by Category") +
  xlab("Category") +
  ylab("Amount")

# Line plot to present quantity of the product category
ggplot(sale_report, aes(x = Category, y = Qty)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  ggtitle("Quantity of Product Category") +
  xlab("Products") +
  ylab("Quantity") +
  theme_minimal()

# clustered bar plot
tbl2 = table(sale_report$Fulfilment, sale_report$Category)
barplot(tbl2, main = 'Provider distribution by Product Category',
        xlab = 'Product Category',
        col = c('yellow', 'pink', "skyblue"),
        legend = TRUE,
        beside = TRUE,
        args.legend = list(x = "topright", inset = c(0.05, 1), 
                           horiz = TRUE, cex = 0.6, bty = "n"))

# Bar Plot of Orders by Size
ggplot(sale_report, aes(x = Size)) +
  geom_bar(fill = "orange") +
  theme_minimal() +
  labs(title = "Number of Orders by Size", x = "Size", y = "Count")

# Scatter-plot: Amount vs. Pieces
ggplot(sale_report, aes(x = Qty, y = Amount)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Amount vs. Quantity", x = "Quantity", y = "Amount")

#--------------3D Scatter plot
scatterplot3d(sale_report$Amount, 
              sale_report$Actual_Price, 
              sale_report$Profit, 
              main = "3D Scatter Plot of Amount, Actual Price and Profit", 
              xlab = "Amount", 
              ylab = "Actual Price", 
              zlab = "Profit", 
              color = "blue",
              pch=19)

#--------------Co-Relation-----------------
#Select only numeric columns for correlation
numeric_data = sale_report[, c("Age", "Qty", "Amount", "Actual_Amount", "Profit")]

#Compute the correlation matrix
cor_matrix = cor(numeric_data)

#View the correlation matrix
cor_matrix

#Create the correlation plot with a color gradient and smaller text
corrplot(cor_matrix, method = "color", type = "upper",
         col = colorRampPalette(c("red", "white", "blue"))(200), # Red to blue color scale
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.5,
         tl.cex = 0.6,
         mar = c(0,0,0,0))

#-----------MODELS-----------------

#---------- Linear regression model
model <- lm(Amount ~ Age + Qty + Actual_Price + Profit, data = sale_report)
summary(model)

#---------- Logistic Regression
#Split the data into training and testing sets (70/30 split)
trainIndex = createDataPartition(sale_report$Amount, p = 0.7,
                                 list = FALSE)
train_data = sale_report[trainIndex, ]
test_data = sale_report[-trainIndex, ]

# Fit the logistic regression model 
logistic_model = glm(Amount ~ Age + Qty + Actual_Price + Profit, data = train_data) 

# Summary of the model 
summary(logistic_model)

# Predict on test data
predictions = predict(logistic_model, test_data)

# Evaluate model performance
actual = test_data$Amount
MAE = mean(abs(predictions - actual))
RMSE = sqrt(mean((predictions - actual)^2))
cat("MAE:", MAE, "RMSE:", RMSE, "\n")

# Combine Actual and Predicted values into a single dataframe
results = data.frame(Actual = test_data$Amount, Predicted =
                       predictions)

# Scatter plot: Actual vs Predicted
ggplot(data = results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs Predicted Amount", x = "Actual Amount", y
       = "Predicted Price")

#---------- Text Mining
review_text = sale_report %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words)

# Calculate word frequency
word_freq = review_text %>%
  count(word, sort = TRUE)

# Bar plot of most common words
top_words = word_freq %>% top_n(15, n)
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 15 Most Common Words",
       x = "Words",
       y = "Frequency")

# Word cloud
png("wordcloud.png", width = 800, height = 600)
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100,
          colors = brewer.pal(8, "Dark2"))

dev.off()
# Adjusting margins
par(mar = c(1, 1, 1, 1))
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100,
          colors = brewer.pal(8, "Dark2"))


