# Importing libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(reshape)
library(treemap)
require(tidyverse)
library(car)
library(plotly)
library(factoextra)
library(caTools)# required library for spliting data
library(reshape2)


# Reading data from csv files

orders <- read.csv('E:/Dataset/orders.csv')
products <- read.csv('E:/Dataset/products.csv')
order_products <- read.csv('E:/Dataset/order_products__train.csv')
order_products_prior <- read.csv('E:/Dataset/order_products__prior.csv')
aisles <- read.csv('E:/Dataset/aisles.csv')
departments <- read.csv('E:/Dataset/departments.csv')

# Preprocessing the data

# Removing null and 'NA' values from all 6 files of the dataset
orders %>% discard(is.null) # Remove null values
Orders <- na.omit(orders) # Remove NA
head(Orders,8) # Show first 8 rows of the Orders

set.seed(123)
split = sample.split(Orders$order_id, SplitRatio = 0.8)# returns true if observation goes to the Training set and false if observation goes to the test set.

# Creating the training set and test set separately and named it as Orders1 and Orders2
Orders1 = subset(Orders, split == FALSE) # Test set
Orders2 = subset(Orders, split == TRUE) # Training set

# As we have 642975 rows in dataset Orders1 therefore we devide it into df in which data has user_id less than 11.
df <- Orders1[Orders1$user_id<"11",]


products %>% discard(is.null) # Remove null values
Products <- na.omit(products) # Remove NA
head(Products,8) # Show first 8 rows of the Products

order_products %>% discard(is.null) # Remove null values
Order_Products <- na.omit(order_products) # Remove NA
head(Order_Products,8) # Show first 8 rows of the Order_Products

order_products_prior %>% discard(is.null) # Remove null values
Order_Products_Prior <- na.omit(order_products_prior) # Remove NA
head(Order_Products_Prior,8) # Show first 8 rows of Order_Products_Prior

aisles %>% discard(is.null) # Remove null values
Aisles <- na.omit(aisles) # Remove NA
head(Aisles,8) # Show first 8 rows of the Aisles

departments %>% discard(is.null) # Remove null values
Departments <- na.omit(departments) # Remove NA
head(Departments,8) # Show first 8 rows of the Departments

# Data columns that are not necessary can be removed during preprocessing, however in our situation, every column is necessary.

# Structure of dataset
str(Orders)

# Summarize the data
summary(Orders)

# Utilize statistical tools like mean, median, and standard deviation to assess the data.
# For the order_dow of the Orders dataset.
mean(Orders1$order_dow, na.rm = TRUE)
median(Orders1$order_dow)
sd(Orders1$order_dow)

# For the order_hour_of_day of the Orders dataset.
mean(Orders1$order_hour_of_day, na.rm = TRUE)
median(Orders1$order_hour_of_day)
sd(Orders1$order_hour_of_day)


# Autocorrelation

# Prepare the data using 6 columns of Orders dataset
autocor_data <- Orders[, c(1,2,4,5,6,7)]
head(autocor_data)

# Compute the correlation matrix
cor_matrix <- round(cor(autocor_data),2)
head(cor_matrix)

# Create the correlation heatmap 
cor_matrix_melted <- melt(cor_matrix)
head(cor_matrix_melted)

ggplot(data = cor_matrix_melted, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() # used to visualize the correlation matrix

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cor_matrix){
  cor_matrix[upper.tri(cor_matrix)] <- NA
  return(cor_matrix)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_matrix){
  cor_matrix[lower.tri(cor_matrix)]<- NA
  return(cor_matrix)
}

upper_tri <- get_upper_tri(cor_matrix)
upper_tri

# Melt the correlation matrix
cor_matrix_melted <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = cor_matrix_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Reorder the correlation matrix
reorder_cor_matrix <- function(cor_matrix){
  # Use correlation between variables as distance
  dd <- as.dist((1-cor_matrix)/2)
  hc <- hclust(dd)
  cor_matrix <-cor_matrix[hc$order, hc$order]
}

# Reorder the correlation matrix
cor_matrix <- reorder_cor_matrix(cor_matrix)
upper_tri <- get_upper_tri(cor_matrix)

# Melt the correlation matrix
cor_matrix_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(cor_matrix_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Print the heatmap
print(ggheatmap)

# Add correlation coefficients
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



# Visualization Methods & Result

# Scatterplot of the order's day of week by order's time of day
x <- Orders$order_dow[1:500]
y <- Orders$order_hour_of_day[1:500]
plot(x, y, main = "Scatterplot of the order's day of week by order's time of day", 
     pch = 19, col = "steelblue", 
     xlab = "Order's day of the week", ylab = "Order's hour of the day")
lines(lowess(x, y), col = "red") # On the scatterplots, draw a line to depict the order's time over day of week.


# Boxplot of table order-products by order number
ggplot(df, aes(eval_set, order_number)) + 
  geom_boxplot() + geom_jitter(alpha=0.1) + 
  scale_y_log10() + 
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,10), expand = c(0, 0)) +
  ggtitle("Boxplot of table order-products by order number") +
  xlab("Eval set of table order-products") + ylab("Order number")


# Histogram of product sales count with respect to hour of a day
Orders1 %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="blue") +
  xlab("Order hour of the day") + ylab("Count") +
  ggtitle("Histogram of product sales count with respect to hour of a day") 


# Histogram of product sales count with respect to day of a week
Orders1 %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="blue") +
  xlab("Order day of the week") + ylab("Count") +
  ggtitle("Histogram of product sales count with respect to day of a week") 


# Calculating retention
# Taking user_id and days_since_prior_order from orders table
user_prior_order_diff = Orders1[, c("user_id", "days_since_prior_order")]

# Grouping by user_id and calculating mean retention
retention = aggregate(user_prior_order_diff[, -1], 
                      list(user_prior_order_diff$user_id), 
                      function(x) c(mean = ceiling(mean(x, na.rm=TRUE))))


# Histogram of the reordering count of days since last order
Orders1 %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="blue") + 
  xlab("Days since prior order") + ylab("Count") +
  ggtitle("Histogram of the reordering count of days since last order") 



# Calculating probability and plotting a bar graph
Order_Products %>% 
  left_join(Orders1,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="blue") +
  xlab("Days since prior order") + ylab("Mean probability of reorder") +
  ggtitle("Histogram of association between time of last order and probability of reorder") 


# Product bases analysis
tmp <- Order_Products %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 


# Histogram of top 10 bestselling products
tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="blue")+
  ggtitle("Histogram of top 10 bestselling products") +
  xlab("Top 10 best selling products")+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.x = element_blank())


# Plotting multivariate data of four columns using matrix scatterplot
scatterplotMatrix(df[4:7])

# Principal Component Analysis
standardisedconcentrations <- as.data.frame(scale(Orders1[4:7])) # standardise the variables
Orders1.pca <- prcomp(standardisedconcentrations)   

summary(Orders1.pca)

eig.val <- get_eigenvalue(Orders1.pca)
eig.val

# Screeplot of eigen values of PCA data
screeplot(Orders1.pca, type="lines", addlabels = TRUE, title = "Scree plot")

# Deciding how many principal components to retain using screeplot
fviz_screeplot(Orders1.pca, type="lines",addlabels = TRUE, title = "Scree plot",  ylim = c(0, 40))


# Parallel coordinates plot
df1 <- Orders1[Orders1$user_id>"9999",]
fig <- df1 %>% plot_ly(type = 'parcoords',
                      dimensions = list(
                        list(range = c(1,100),
                             label = 'Order Number', values = ~df1$order_number),
                        list(range = c(0,6),
                             label = 'Order day of week', values = ~df1$order_dow),
                        list(range = c(0,24),
                             label = 'Order time of day', values = ~df1$order_hour_of_day),
                        list(range = c(0,30),
                             label = 'Days since prior order', values = ~df1$days_since_prior_order)
                      )  
) %>%
  layout(title="Parallel Coordinates Plot of four attributes")
fig

# create a dataset
eval_set <- df$eval_set
dow <- df$order_dow
Freq <- abs(rnorm(34865))
data <- data.frame(dow,eval_set,Freq)

# Stacked barchart
ggplot(data, aes(fill=eval_set, y=Freq, x=dow)) +  
  geom_bar(position="stack", stat="identity") +
  ggtitle("Stacked barchart of order-products type") +
  xlab("Order-products type") + ylab("Frequency")

# Density plot
# Color by groups
ggplot(df, aes(x=days_since_prior_order, color=eval_set, fill=eval_set)) + 
  geom_density(alpha=.2) + 
  labs(title="Density curve of number of days since last order of eval set", x="Days since prior order", y = "Density")


# How often are products from the department/aisle sold
tmp <- Products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(Departments,by="department_id")
tmp <- tmp %>% left_join(Aisles,by="aisle_id")

tmp2<-Order_Products %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(Products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(tmp, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)

# Tree map 
treemap(tmp2, index=c("department","aisle"), vSize="sumcount",
        title="",palette="Set3",border.col="#FFFFFF")



