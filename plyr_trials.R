
# TUTORIAL from https://www.r-bloggers.com/a-fast-intro-to-plyr-for-r/

# install.packages("plyr") #run this if you don't have the package already
library(plyr)

#make some example data
dd<-data.frame(matrix(rnorm(216),72,3),c(rep("A",24),rep("B",24),rep("C",24)),c(rep("J",36),rep("K",36)))
colnames(dd) <- c("v1", "v2", "v3", "dim1", "dim2")

#ddply is the plyr function
ddply(dd, 
      c("dim1","dim2"), 
      function(df)mean(df$v1))
# The first two letters of the function tell the input and output data types, respectively (data frame and data frame here)
# The first argument, dd, is the input data frame
# The next argument is the “group by” variables. Since I want to group by two variables I send them as a vector (that’s what the c() bit does). 
# The idea behind function(df)mean(df$v1) is to create a function to which we can pass a data frame and get out a meaningful result. The subset (or split) of the data gets passed to the function and that subset is then known as df. mean(df$v1) calculates the mean of v1 and returns an answer. ddply holds on to the answers of each split and then reassembles them all in the end. 

#  the idea can be extended to a vector of functions in order to perform many operations on each split:

ddply(dd, 
      c("dim1","dim2"), 
      function(df)c(mean(df$v1),mean(df$v2),mean(df$v3),sd(df$v1),sd(df$v2),sd(df$v3)))


# TUTORIAL from http://seananderson.ca/courses/12-plyr/plyr_2012.pdf

# A general example with plyr
# Let’s take a simple example. 
# Take a data frame
# split it up (by year)
# calculate the coefficient of variation of the count, and 
# return a data frame. 
#This could easily be done on one line, but I’m expanding it here to show the format a more complex function could take.

set.seed(1)
d <- data.frame(year = rep(2000:2002, each = 3),count = round(runif(9, 0, 20)))
print(d)

library(plyr)
x <- ddply(d, "year", function(x) {
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count/mean.count
  data.frame(mean.count = mean.count, cv.count = cv)
  })
print(x)

# transform and summarize

# summarize - creates new data frame
ddply(d, "year", summarise, mean.count = mean(count)) # applies "summarise" function to each year group
# accomplishes the same thing as above
ddply(d, "year", function(d){mean.count <- mean(d$count)})

# transform - adds column to existing data frame
print(d)
ddply(d, "year", transform, total.count = sum(count), mean.count = mean(count))

# mutate. mutate works like transform but lets you build on columns you build.
ddply(d, "year", mutate, mu = mean(count), sigma = sd(count), cv = sigma/mu)


# Plotting with plyr
# You can use plyr to plot data by throwing away the output with an underscore (_).
# This is a bit cleaner than a for loop since you don’t have to subset the data manually.

par(mfrow = c(1, 3), mar = c(2, 2, 1, 1), oma = c(3, 3, 0, 0))
d_ply(d, "year", transform, plot(count, main = unique(year), type = "o"))
mtext("count", side = 1, outer = TRUE, line = 1)
mtext("frequency", side = 2, outer = TRUE, line = 1)


ddply(d, "year", summarise, boxplot(count, main = unique(year), type = "o"))


#The basic syntax can be easily extended to break apart the data based on multiple columns:
baseball.dat <- subset(baseball, year > 2000) # data from the plyr package
x <- ddply(baseball.dat, c("year", "team"), summarize, homeruns = sum(hr))
head(x)




# TUTORIAL FROM http://stat545.com/block013_plyr-ddply.html
library(gapminder)
max_le_by_cont <- ddply(gapminder, "continent", summarize, max_le = max(lifeExp))
min_gdppc_by_cont <- ddply(gapminder, "continent", summarize, min_gdppc = min(gdpPercap))
ddply(gapminder, "continent",
      summarize, n_uniq_countries = length(unique(country)))

# does the same thing as above
ddply(gapminder, "continent",function(x) {
        n_uniq_countries <-length(unique(x$country))
        data.frame(n_uniq_countries)})

ddply(gapminder, ~ continent,
      function(x) c(n_uniq_countries = length(unique(x$country))))

# calculating multiple things
ddply(gapminder, ~ continent, summarize,
      min_le = min(lifeExp), max_le = max(lifeExp),
      med_gdppc = median(gdpPercap))


# function for linear model
le_lin_fit <- function(dat, offset = 1952) {
  the_fit <- lm(lifeExp ~ I(year - offset), dat)
  setNames(coef(the_fit), c("intercept", "slope"))
}
le_lin_fit(subset(gapminder, country == "Canada"))

j_coefs <- ddply(gapminder, "country", le_lin_fit)
print(j_coefs)


# TUTORIAL FROM https://gist.github.com/noamross/4558068

library(ggplot2)
data(mpg)
str(mpg)
data <- mpg[,c(1,7:9)]
str(data)

## note the differences between the commands "summarize" and "transform"
ddply(data, "drv", summarize, avgcty = mean(cty))
ddply(data, "drv", transform, avgcty = mean(cty))

## transform is very useful standardizing/normalizing
ddply(data, .(drv), transform, delta = mean(cty)-cty)

## Now let's use plyr to run a simple loop

## We'll ask the question: Does city mpg  differ between car manufacturers, for each class of drivetrains (4x4, forward, or rear-wheel drive)? Let's try to automate these ANOVAs and extract the F-statistics and P-values from the ANOVAs.


## Step1: create function to run ANOVA

model <- function(data) { aov(cty~manufacturer, data=data) }

## Step 2: Use plyr to run model for each and create list (called anova.output) to store output for each drivetrain. For dlply, the syntax means d for input data is data frame and l for output data is list.

anova.output <- dlply(data, "drv", model)

## Step 3: Create function that tells R where to find F-statistic and P-value in the output within the list. The output is somewhat hidden in this example- don't worry about the messy indexing here-- what's important is that this just tells R where the F-stats and P-values are stored. 

juicy <- function(x) { c(summary(x)[[1]][["F value"]][[1]], 
                         summary(x)[[1]][["Pr(>F)"]][[1]]) }

## Step 4: Extract components of model output from the list created in previous step. For ldply, the syntax is: input is list and output is data frame. Note that since the input is a list, we don't have to indicate the 2nd parameter (which variable(s) to apply the function to, as the default is to apply function to all elements of the list.) 

ldply(anova.output, juicy)

## The data frame shows F-statistics (V1) and P-values (V2) for the ANOVAs by drivetrain.


## We could always condense some of the above steps as well:

anova.output <- dlply(data, .(drv), function(data) aov(cty~manufacturer, data=data))
ldply(anova.output, function(x) { c(summary(x)[[1]][["F value"]][[1]], summary(x)[[1]][["Pr(>F)"]][[1]]) })

## Note that there are many shortcuts that plyr uses, such as the functions colwis(), each() and splat(). You can always refer to the original article: http://www.jstatsoft.org/v40/i01/ for more on this.

