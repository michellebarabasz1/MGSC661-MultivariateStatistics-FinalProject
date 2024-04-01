library(readr)
chocolate <- read_csv("chocolate.csv")
df = chocolate
attach(df)

#Remove unncessary variables
variables_to_remove = c('REF')
df = df[, setdiff(names(df), variables_to_remove)]

#Check for missing values
missing_values = colSums(is.na(df))
print(paste("Missing Values:\n", missing_values))
#Drop missing values
df <- df[complete.cases(df), ]

names(df) <- gsub("\n", "", names(df))
names(df) <- gsub(" ", "", names(df))
names(df)[1] <- "Company"

##Exploratory Data Analysis##
#For the target variable, Rating
print(table(df$Rating))
barplot(table(df$Rating), col = c("skyblue", "orange"), main = "Distribution of Rating")
print(prop.table(table(df$Rating)))

summary(df$Rating)

# Remove the percentage sign and convert to numeric
df$CocoaPercent <- as.numeric(gsub("%", "", df$CocoaPercent))
df$ReviewDate <- as.numeric(df$ReviewDate)
library(dplyr)
# Replace blank values with NA and then replace NA with "Unknown"
df <- df %>%
  mutate(BeanType = coalesce(na_if(BeanType, ""), "Unknown"))


#For the numerical variables
numerical_vars = c('ReviewDate', 'CocoaPercent')
summary(df[, numerical_vars])

# Plot histograms
par(mfrow = c(2, 1))
for (i in 1:length(numerical_vars)) {
  hist(df[[numerical_vars[i]]], 
       main = paste("Histogram of", numerical_vars[i]),
       col = "skyblue", border = "black", 
       xlab = numerical_vars[i], ylab = "Frequency")
}

par(mfrow = c(1, 2)) 
for (i in 1:length(numerical_vars)) {
  boxplot(df[[numerical_vars[i]]], main = paste("Boxplot of", numerical_vars[i]),
          col = "skyblue", border = "black", notch = TRUE)
}

cor_matrix = cor(df[, numerical_vars])
print(cor_matrix)

scatterplot_matrix = cbind(df[, numerical_vars], Rating = df$Rating)
pairs(scatterplot_matrix, col = ifelse(scatterplot_matrix$deal == 1, "red", "blue"))


#For the categorical variables
categorical_vars = c('Company', 'SpecificBeanOriginorBarName', 'CompanyLocation', 'BeanType', 'BroadBeanOrigin')

for (cat_var in categorical_vars) {
  frequency_table = table(df[[cat_var]])
  sorted_table = sort(frequency_table, decreasing = TRUE)
  top_5_categories = names(sorted_table)[1:5]
  
  if (any(df[[cat_var]] %in% top_5_categories)) {
    df_top_5 = df[df[[cat_var]] %in% top_5_categories, ]
    
    print(paste("Top 5 Categories for", cat_var))
    print(sorted_table[1:5])
    
    barplot(sorted_table[1:5], col = "skyblue", main = paste("Bar Plot of Top 5", cat_var))
    
    percentage_distribution = prop.table(sorted_table[1:5]) * 100
    print(paste("Percentage Distribution for Top 5", cat_var))
    print(percentage_distribution)
  } else {
    print(paste("No matches found in the top 5 categories for", cat_var))
  }
}

par(mfcol = c(2, 3), mar = c(4, 4, 2, 1))

for (cat_var in categorical_vars) {
  frequency_table = table(df[[cat_var]])
  sorted_table = sort(frequency_table, decreasing = TRUE)
  top_5_categories = names(sorted_table)[1:5]
  
  if (any(df[[cat_var]] %in% top_5_categories)) {
    df_top_5 = df[df[[cat_var]] %in% top_5_categories, ]
    
    cat("\n")
    cat(paste("Top 5 Categories for", cat_var), "\n")
    print(sorted_table[1:5])
    
    barplot(sorted_table[1:5], col = "skyblue", main = paste("Bar Plot of Top 5", cat_var))
    
    percentage_distribution = prop.table(sorted_table[1:5]) * 100
    cat("\n")
    cat(paste("Percentage Distribution for Top 5", cat_var), "\n")
    print(percentage_distribution)
  } else {
    cat("\n")
    cat(paste("No matches found in the top 5 categories for", cat_var), "\n")
  }
}

par(mfcol = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


#Numerical variables and the target variable
numerical_vars = c('ReviewDate', 'CocoaPercent')

par(mfrow = c(1, 2))
for (i in 1:length(numerical_vars)) {
  plot(df[[numerical_vars[i]]], df$Rating, 
       main = paste("Scatter Plot of", numerical_vars[i], "vs Rating"),
       xlab = numerical_vars[i], ylab = "Rating", col = "skyblue", pch = 16)
}

correlation_matrix = cor(df[, c('Rating', numerical_vars)])
print(correlation_matrix)

model = lm(Rating ~ ReviewDate + CocoaPercent, data = df)
summary(model)

#Categorical variables and the target variable
categorical_vars = c('Company', 'SpecificBeanOriginorBarName', 'CompanyLocation', 'BeanType', 'BroadBeanOrigin')

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
for (cat_var in categorical_vars) {
  boxplot(Rating ~ df[[cat_var]], data = df, 
          col = "skyblue", border = "black", notch = TRUE,
          main = paste("Boxplot of Rating by", cat_var),
          xlab = cat_var, ylab = "Rating")
}

for (cat_var in categorical_vars) {
  avg_rating_by_category <- tapply(df$Rating, df[[cat_var]], mean)
  barplot(avg_rating_by_category, col = "skyblue",
          main = paste("Average Rating by", cat_var),
          xlab = cat_var, ylab = "Average Rating")
}

for (cat_var in categorical_vars) {
  contingency_table = table(df[[cat_var]], df$Rating)
  
  chi_square_result = chisq.test(contingency_table)
  
  cat(paste("Chi-Square Test for", cat_var, ":\n"))
  print(chi_square_result)
  cat("\n")
}

df = df[, setdiff(names(df), c('BroadBeanOrigin', 'SpecificBeanOriginorBarName', 'BeanType'))]

#Classification
#Logistic Regression
library(caret)
df$RatingBinary = ifelse(df$Rating > 3.5, 1, 0)
df[numerical_vars] = scale(df[numerical_vars])

company_dummies = as.data.frame(model.matrix(~ Company - 1, data = df))
colnames(company_dummies) <- make.names(colnames(company_dummies))
df = cbind(df, company_dummies)

location_dummies = as.data.frame(model.matrix(~ CompanyLocation - 1, data = df))
colnames(location_dummies) <- make.names(colnames(location_dummies))
df = cbind(df, location_dummies)

formula = as.formula(paste("RatingBinary ~ CocoaPercent + ReviewDate + ", 
                            paste(names(company_dummies), collapse = " + "),
                            "+", 
                            paste(names(location_dummies), collapse = " + ")))

logistic_model = glm(formula, data = df, family = "binomial")
summary(logistic_model)

predicted_probs = predict(logistic_model, df, type = "response")
predicted_binary = ifelse(predicted_probs > 0.5, 1, 0)

accuracy = sum(predicted_binary == df$RatingBinary) / nrow(df)
cat("Accuracy:", accuracy, "\n")

#Random Forest
library(randomForest)

company_dummies = as.data.frame(model.matrix(~ Company - 1, data = df))
location_dummies = as.data.frame(model.matrix(~ CompanyLocation - 1, data = df))

colnames(company_dummies) = make.names(colnames(company_dummies))
colnames(location_dummies) = make.names(colnames(location_dummies))

df = cbind(df, company_dummies, location_dummies)

predictors = c("CocoaPercent", "ReviewDate", names(company_dummies), names(location_dummies))

if (!"RatingBinary" %in% colnames(df)) {
  stop("RatingBinary column not found in the dataframe.")
}

rf_model = randomForest(RatingBinary ~ ., data = df[, c(predictors, "RatingBinary")], ntree = 500)

importance(rf_model)
print(importance(rf_model))

rf_predictions = predict(rf_model, df[, predictors], type = "response")
rf_binary_predictions = ifelse(rf_predictions > 0.5, 1, 0)

rf_accuracy = sum(rf_binary_predictions == df$RatingBinary) / nrow(df)
cat("Random Forest Accuracy:", rf_accuracy, "\n")


#Clustering
company_dummies = as.data.frame(model.matrix(~ Company - 1, data = df))
location_dummies = as.data.frame(model.matrix(~ CompanyLocation - 1, data = df))

colnames(company_dummies) = make.names(colnames(company_dummies))
colnames(location_dummies) = make.names(colnames(location_dummies))

df = cbind(df, company_dummies, location_dummies)

predictors = c("CocoaPercent", "ReviewDate", names(company_dummies), names(location_dummies))

if (!"RatingBinary" %in% colnames(df)) {
  stop("RatingBinary column not found in the dataframe.")
}

rf_model = randomForest(RatingBinary ~ ., data = df[, c(predictors, "RatingBinary")], ntree = 500)
feature_importance = importance(rf_model)
sorted_features = feature_importance[order(-feature_importance[,"IncNodePurity"]), ]
print(sorted_features)
top_features = c('ReviewDate', 'CocoaPercent', 'CompanySoma', 'CompanyBonnat', 'CompanyLocationFrance', 'CompanyLocationU.S.A.')


df_selected_features = df[, c(top_features, "RatingBinary")]

k.max = 15 
wss = sapply(1:k.max, 
             function(k){kmeans(df_selected_features, k, nstart=50, iter.max = 15 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

num_clusters = 4
kmeans_result = kmeans(df_selected_features[, top_features], centers = num_clusters)
print(kmeans_result)
df$Cluster = kmeans_result$cluster
table(df$Cluster)
