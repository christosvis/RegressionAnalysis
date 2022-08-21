# Linear Regression in R
# https://www.scribbr.com/statistics/linear-regression-in-r/

# install libraries
install.packages("datarium")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("gapminder")
install.packages("plotly")

# https://rdocumentation.org/packages/datarium/versions/0.1.0
library(datarium)

# load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(gapminder)
library(plotly)

#Load marketing data from the datarium package, assign it to a dataframe called "sampledf".
data(marketing)
sampledf <- marketing

#View the data
str(sampledf)
View(sampledf)
head(sampledf)
summary(sampledf)

# To check whether the dependent variable follows a normal distribution, use the hist() function.
hist(sampledf$sales)

# The relationship between the independent and dependent variable must be linear. 
# We can test this visually with a scatter plot to see if the distribution of data points 
# could be described with a straight line.
plot(sales ~ facebook, data = sampledf)

# Use the cor() function to test the relationship between your independent variables 
# and make sure they aren’t too highly correlated.

# correlation between 2 variables
cor(sampledf$facebook, sampledf$youtube)

# correlation matrix
sampledf.cor = cor(sampledf)
sampledf.cor

# Visualizing the correlation matrix
install.packages("corrplot")
library(corrplot)

corrplot(sampledf.cor)

# heatmap
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = sampledf.cor, col = palette, symm = TRUE)

# Check Linearity
plot(sales ~ facebook, data=sampledf)
plot(sales ~ youtube, data=sampledf)
plot(sales ~ newspaper, data=sampledf)

# ---------------
# Perform the linear regression analysis
facebook.sales.lm = lm(sales ~ facebook, data = sampledf)
summary(facebook.sales.lm)


# Multiple Regression
media.sales.lm = lm(sales ~ facebook + youtube + newspaper, data = sampledf)
summary(media.sales.lm)

# Check for homoscedasticity
plot(facebook.sales.lm)

# Note that the par(mfrow()) command will divide the Plots window 
# into the number of rows and columns specified in the brackets
# par(mfrow=c(2,2))
# plot(facebook.sales.lm)
# par(mfrow=c(1,1))
# plot(facebook.sales.lm)

plot(media.sales.lm)

# ----------------------
# Visualize results

# Plot the data points on a graph
facebooksales.graph<-ggplot(sampledf, aes(x=facebook, y=sales))+geom_point()

facebooksales.graph

# Add the linear regression line to the plotted data

facebooksales.graph <- facebooksales.graph + geom_smooth(method="lm", col="black")
facebooksales.graph

# Add the equation for the regression line.
facebooksales.graph <- facebooksales.graph + 
  stat_regline_equation(label.x = 15, label.y = 15)

facebooksales.graph

# Make the graph ready for publication

facebooksales.graph +
  theme_bw() +
  labs(title = "Facebook - Sales relationship",
       x = "Facebook",
       y = "Sales")


# Multiple Regression
plotting.data<-expand.grid(
  facebook = seq(min(sampledf$facebook), max(sampledf$facebook), length.out=30),
  youtube=c(min(sampledf$youtube), mean(sampledf$youtube), max(sampledf$youtube)),
  newspaper = c(min(sampledf$newspaper), mean(sampledf$newspaper), max(sampledf$newspaper))
  )

# Predict the values of based on your linear model
plotting.data$predicted.y <- predict.lm(media.sales.lm, newdata=plotting.data)

# Plot the original data
media.sales.plot <- ggplot(sampledf, aes(x=facebook, y=sales)) +
  geom_point()

media.sales.plot

# Add the regression lines
media.sales.plot <- media.sales.plot +
  geom_line(data=plotting.data, aes(x=facebook, y=predicted.y, color=youtube), size=1.25)

media.sales.plot
