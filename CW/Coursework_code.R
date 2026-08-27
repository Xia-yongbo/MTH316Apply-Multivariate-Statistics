#MTH316 Course Work R Code
library(ggplot2)
library(dplyr)

#Part1 Data Preparation and Descriptive Statistics (25 marks)
#1.1. Boxplot per day choose Changping
data_Changping = read.csv("air_quality_Changping.csv", header = TRUE)

#Selecte data and variables
Changping_Feb <- data_Changping%>% 
  filter(year == 2015, month == 2)
Changping_Feb <- Changping_Feb %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")))
variables <- c("date","PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP")
plot_data <- Changping_Feb[, variables]

# define variables and make plot
vars <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP")
for (v in vars) {
  daily_list <- split(plot_data[[v]], plot_data$date)
  boxplot(daily_list,
          main = paste("Daily boxplot of", v, "in February 2015 (Changping)"),
          xlab = "",
          ylab = paste(v, "concentration / temperature"),
          las = 2,
          cex.axis = 0.7,
          mgp = c(3, 0.5, 0),
          col = "lightblue",      
          border = "blue",    
          whiskcol = "blue",  
          staplecol = "blue", 
          medcol = "black",         
          outcol = "red",     
          outpch = 16) 
}

#1.2
# First create date variable for the full Changping dataset
data_Changping <- data_Changping %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")))

#Calculate Pi(t)
daily_median <- aggregate(
  x = data_Changping[, c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP")],
  by = list(date = data_Changping$date),
  FUN = median,
  na.rm = TRUE
)
head(daily_median) 

#Calculate ri(t)
P_matrix <- as.matrix(daily_median[, c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP")])
T_days <- nrow(P_matrix)  

r <- matrix(NA, nrow = T_days - 1, ncol = 7)
# Calculate separately for each column (each pollutant)
for (j in 1:7) {
  for (t in 2:T_days) {
    denominator <- P_matrix[t - 1, j]
    
    if (denominator == 0 || is.na(denominator)) {
      r[t - 1, j] <- NA
    } else {
      r[t - 1, j] <- (P_matrix[t, j] - P_matrix[t - 1, j]) / denominator
    }
  }
}

# add name for each colum
colnames(r) <- c("rPM2.5", "rPM10", "rSO2", "rNO2", "rCO", "rO3", "rTEMP")
r_days <- nrow(r)

#Validate numbers of days
r_days
T_days

#Use prior value to interpolate if there is any Inf or NaN, or NA
library(zoo)
# Convert Inf and NaN to NA first
r[is.infinite(r) | is.nan(r)] <- NA

# Forward fill NA values using previous available value
r_matrix <- na.locf(r, na.rm = FALSE)

#statistic table
library(e1071) 
#Define function
my_stats <- function(x) {
  n <- length(x)                         
  mean_val <- mean(x, na.rm = TRUE)     
  sd_val <- sd(x, na.rm = TRUE)         
  min_val <- min(x, na.rm = TRUE)       
  q1 <- quantile(x, probs = 0.25, na.rm = TRUE)   
  median_val <- median(x, na.rm = TRUE) 
  q3 <- quantile(x, probs = 0.75, na.rm = TRUE) 
  max_val <- max(x, na.rm = TRUE)        
  skew <- skewness(x, na.rm = TRUE)      
  kurt <- kurtosis(x, na.rm = TRUE)   
  
  return(c(n = n, 
           mean = mean_val, 
           sd = sd_val, 
           min = min_val, 
           Q1 = q1, 
           median = median_val, 
           Q3 = q3, 
           max = max_val, 
           skewness = skew, 
           kurtosis = kurt))
}
stats_matrix <- apply(r_matrix, 2, my_stats)

# Convert to a data frame and add variable name
stats_table <- as.data.frame((stats_matrix))
stats_table <- round(stats_table, 4)
print(stats_table)

#2. Linear Regression for One-Day-Ahead Prediction of PM2.5
#2.1. Model fitting and output
# 创建时间索引 t
t_index <- 2:(nrow(r_matrix)+1)
r_OLS <- cbind(t = t_index, r_matrix)
#一共有1461天,但是计算变化比例第一天不算所以一r_matrix有n=1460个值
#Observation pairs is n-1=1459
#constract y(t)
y_t <- c(r_OLS[, "rPM2.5"][-1], NA)
r_df <- as.data.frame(r_OLS)
r_df$y_t <- y_t
#Delet last colum
r_df <- r_df[1:(nrow(r_df)-1), ]
#Constract model
m0 <- lm(formula= y_t ~ rPM2.5 +rPM10 +rSO2 +rNO2 +rCO +rO3 +rTEMP,data=r_df )
summary(m0)

#2.2. Explain
#Multicollinearity and Pearson correlation coefficient matrix
pred_vars <- r_df[, c("rPM2.5", "rPM10", "rSO2", "rNO2", "rCO", "rO3", "rTEMP")]
cor_matrix <- cor(pred_vars, method = "pearson")
round(cor_matrix, 4)

#2.3 Evaluate model adequacy
#(a)
par(mfrow = c(2, 2))
plot(m0)
#Residuals versus fitted values,diagnotic the assumptions of linearity
par(mfrow = c(1, 1))
plot(m0,1)
#Q–Q plot of the residuals.diagnotic the normality assumption.
par(mfrow = c(1, 1))
plot(m0,2)

#3.PCA
# 3.1 Standardization
# Extract the seven predictor variables
predictors <- r_df[, c("rPM2.5", "rPM10", "rSO2",
                       "rNO2", "rCO", "rO3", "rTEMP")]

#Standardization (Centralization + Deviation from the Mean)
PCA_matrix <- scale(predictors,
                    center = TRUE,
                    scale = TRUE)

# Convert to data frame if needed
PCA_matrix <- as.data.frame(PCA_matrix)
library(e1071)
my1_stats <- function(x) {
  c(
    Count = length(x),
    Mean = mean(x),
    StdDev = sd(x),
    Min = min(x),
    Q1 = quantile(x, 0.25),
    Median = median(x),
    Q3 = quantile(x, 0.75),
    Max = max(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x)
  )
}

PCA_stats_matrix <- apply(PCA_matrix, 2, my1_stats)
PCA_stats_table <- round(as.data.frame(PCA_stats_matrix), 4)
print(PCA_stats_table)

#3.2
#(a)Eigenvector
x<- cor(PCA_matrix)
eigen_result <- eigen(x)
lambda <- eigen_result$values
round(lambda, 4)

#(b)
pca_result <- princomp(PCA_matrix, cor = TRUE)
# Extract the standard deviations of the principal components
sdev <- pca_result$sdev

# Calculate the variance explained by each principal component
variance_explained <- (sdev^2) / sum(sdev^2) * 100
# Plot the % of variance explained by principal component
bar_positions = barplot(variance_explained, 
                        main = "% of Variance Explained by Each Principal Component", 
                        xlab = "Principal Components", 
                        ylab = "% Variance Explained", 
                        names.arg = paste("PC", 1:length(variance_explained), sep = ""), 
                        col = "skyblue", 
                        border = "black",
                        ylim = c(0, max(variance_explained) + 8))
# Add % values on top of each bar
text(x = bar_positions, 
     y = variance_explained+1, 
     labels = paste0(round(variance_explained, 2), "%"), 
     pos = 3,  # Position the text below the bars
     cex = 0.8,  # Font size
     col = "black")

#Calculate cumulative variance explained
cumulative_variance <- cumsum(variance_explained)
# Plot cumulative variance as a line
# Plot cumulative variance as a line
plot(
  cumulative_variance,
  type = "b",  # "b" for both points and lines
  col = "blue",
  pch = 16,  # Point style (solid dots)
  lwd = 2,   # Line width
  xlab = "Principal Components",
  ylab = "Cumulative Variance Explained (%)",
  main = "Cumulative Variance Explained by Principal Components",
  ylim = c(0, 110)  # Ensure the y-axis goes from 0% to 100%
)

# Add variance values as text on the line
text(
  x = 1:length(cumulative_variance),  # x-coordinates (PC indices)
  y = cumulative_variance,           # y-coordinates (cumulative variance values)
  labels = paste0(round(cumulative_variance, 2), "%"),  # Text labels (rounded values)
  pos = 3,  # Position of text: above the points
  col = "red",  # Color of the text
  cex = 0.8  # Size of the text
)

#(c)
# Extract eigenvalues
eigenvalues <- pca_result$sdev^2
plot(
  eigenvalues,
  type = "b",
  pch = 16,
  lwd = 2,
  xlab = "Principal Components",
  ylab = "Eigenvalue",
  main = "Scree Plot of Eigenvalues",
  xaxt = "n",
  ylim = c(0, max(eigenvalues) + 0.5)
)

axis(1, at = 1:length(eigenvalues), labels = paste0("PC", 1:length(eigenvalues)))

text(
  x = 1:length(eigenvalues),
  y = eigenvalues,
  labels = round(eigenvalues, 4),
  pos = 3,
  cex = 0.8,
  col = "red"
)

#3.3 Loading matrix
# Extract loading matrix
k <- 4
loading_matrix <- pca_result$loadings[, 1:k]
# Convert to matrix/data frame for easier viewing
loading_matrix <- as.matrix(loading_matrix)
# Round to four decimal places
loading_table <- round(loading_matrix, 4)
print(loading_table)

#3.4 PCA regression
#(a)Constrect Model
#Constract Data
PCA_score <- as.data.frame(pca_result$scores[, 1:4])
# Add time index and response variable
PCA_score$t <- r_df$t
PCA_score$y_t <- r_df$y_t
#model
m_pca <- lm(y_t ~ Comp.1 + Comp.2 + Comp.3 + Comp.4, data = PCA_score)
summary(m_pca)



