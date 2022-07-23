setwd("D:/tutorials/graphic")
airquality
dim(airquality)
colnames(airquality)
airquality$Month
heights <- tapply(airquality$Temp, airquality$Month, mean)
heights
head(airquality)
unique(airquality$Month)
barplot(heights)
barplot(heights, main= "Mean Temp. by Month", names.arg = c("May", "Jun", "Jul", "Aug", "Sep"), 
        ylab="Temp (deg. F)")

#Adding Confidence Intervals to a Bar Chart
library(gplots)
barplot2(x, plot.ci=TRUE, ci.l=lower, ci.u=upper)
attach(airquality)
heights <- tapply(Temp, Month, mean)
lower <- tapply(Temp, Month, function(v) t.test(v)$conf.int[1])
upper <- tapply(Temp, Month, function(v) t.test(v)$conf.int[2])
barplot2(heights, plot.ci=TRUE, ci.l=lower, ci.u=upper, ylim=c(50,90), xpd=FALSE, 
         main="Mean Temp. By Month", names.arg=c("May","Jun","Jul","Aug","Sep"), ylab="Temp (deg. F)")

#adding colors to barplots
barplot2(heights, plot.ci=TRUE, ci.l=lower, ci.u=upper, ylim=c(50,90), xpd=FALSE, 
         main="Mean Temp. By Month", names.arg=c("May","Jun","Jul","Aug","Sep"), ylab="Temp (deg. F)", col = "blue")

barplot(heights, col=colors)
barplot(c(3,5,4), col=c("red","white","blue"))

rel.hts <- rank(heights) / length(heights)

rel.hts <- (heights - min(heights)) / (max(heights) - min(heights))
grays <- gray(1 - rel.hts)
barplot(heights,col=grays,ylim=c(50,90), xpd=FALSE,main="Mean Temp. By Month",
        names.arg=c("May", "Jun", "Jul", "Aug", "Sep"),ylab="Temp (deg. F)")
barplot2(heights,col=grays,ylim=c(50,90), xpd=FALSE,main="Mean Temp. By Month",
         names.arg=c("May", "Jun", "Jul", "Aug", "Sep"),ylab="Temp (deg. F)",
         plot.ci=TRUE, ci.l=lower, ci.u=upper)


rel.hts <- (heights - min(heights)) / (max(heights) - min(heights))
rainbows <- rainbow(1 - rel.hts)
barplot(heights,col=rainbows,ylim=c(50,90), xpd=FALSE,main="Mean Temp. By Month",
        names.arg=c("May", "Jun", "Jul", "Aug", "Sep"),ylab="Temp (deg. F)")
barplot2(heights,col=rainbows,ylim=c(50,90), xpd=FALSE,main="Mean Temp. By Month",
         names.arg=c("May", "Jun", "Jul", "Aug", "Sep"),ylab="Temp (deg. F)",
         plot.ci=TRUE, ci.l=lower, ci.u=upper)


#07-15-2022
# Tutorial on grouped barchart   Author: Joachim Schork
# Source of the code: https://statisticsglobe.com/draw-grouped-barplot-in-r
data <- data.frame(values = c(4, 1, 3, 6, 7, 3),  # Create example data
                   group = rep(c("group 1",
                                 "group 2",
                                 "group 3"),
                               each = 2),
                   subgroup = LETTERS[1:2])

data

#Example 1: Drawing Grouped Barchart Using Base R
# For this, we first have to convert our data frame to a properly formatted matrix:
data_base <- reshape(data,                        # Modify data for Base R barplot
                     idvar = "subgroup",
                     timevar = "group",
                     direction = "wide")

data_base                                       # Print modified data

row.names(data_base) <- data_base$subgroup
data_base <- data_base[ , 2:ncol(data_base)]
colnames(data_base) <- c("group 1", "group 2", "group 3")
data_base <- as.matrix(data_base)
data_base                                         # Print modified data

# use the barplot() function to draw our grouped barplot in Base R
barplot(height = data_base,                       # Grouped barplot using Base R
        beside = TRUE)

# Example 2: Drawing Grouped Barchart Using ggplot2 Package
#In case we want to apply the functions of the ggplot2 package, we first need to install and load ggplot2
install.packages("ggplot2")                       # Install & load ggplot2 package
library("ggplot2")
ggplot(data,                                      # Grouped barplot using ggplot2
       aes(x = group,
           y = values,
           fill = subgroup)) +
  geom_bar(stat = "identity",
           position = "dodge")

p_unload(ggplot2)

#Example 3: Drawing Grouped Barchart Using lattice Package
install.packages("lattice")                       # Install lattice package
library("lattice")                                # Load lattice package

barchart(values ~ group,                          # Grouped barplot using lattice
         data = data,
         groups = subgroup)

p_unload(lattice)

# https://statdoe.com/cld-customisation/        Author: Rosane Rech
# Customising the Compact Letter Display Position
# loading the appropriate libraries
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)
library(datasets)

# analysis of variance
anova <- aov(weight ~ feed, data = chickwts)

# Tukey's test
tukey <- TukeyHSD(anova)

# compact letter display
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
dt <- group_by(chickwts, feed) %>%
  summarise(w=mean(weight), sd = sd(weight)) %>%
  arrange(desc(w))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$feed)
dt$cld <- cld$Letters

print(dt)

#Barplot with error bars
ggplot(dt, aes(feed, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Feed Type", y = "Average Weight Gain (g)") +
  theme_few()

#Adding the letters indicating significant differences
ggplot(dt, aes(feed, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Feed Type", y = "Average Weight Gain (g)") +
  geom_text(aes(label = cld)) +
  theme_few()

#Letter's position: just above the bars and beside the error bars
ggplot(dt, aes(feed, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Feed Type", y = "Average Weight Gain (g)") +
  geom_text(aes(label = cld), vjust = -0.5, hjust = -0.5) +
  theme_few()

#Letter's position: just above the error bars
#Method 1
ggplot(dt, aes(feed, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Feed Type", y = "Average Weight Gain (g)") +
  geom_text(aes(label = cld, y = w + sd), vjust = -0.5) +
  ylim(0,410) +
  theme_few()

#Method 2
ggplot(dt, aes(feed, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-sd, ymax=w+sd), width = 0.2) +
  labs(x = "Feed Type", y = "Average Weight Gain (g)") +
  geom_text(aes(label = cld, y = w + sd + 20)) +
  ylim(0,410) +
  theme_few()

#Saving the final figure
# saving the final figure
ggsave("barplot.png", width = 4.2, height = 3, dpi = 1000)

p_unload(ggplot2, ggthemes, multcompView, dplyr)
detach("package:datasets", unload = TRUE)
