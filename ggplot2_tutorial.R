# ggplot2
# Alex Burnham


# Clear memory of varaiables and objects:
rm(list=ls())


# In all ggplots:
# plot_name <- ggplot(data, aes(xVar = x, yVar = y))
# + geom_blank() <-- bar, point, etc.
# plot_name + function1() + function2()


# Create a dataset

# create sample IDs
ID <- c(1:200)

# create a factor called origin
Origin <- c(rep("local", 100), 
            rep("California", 100))

# create a factor called flower type
FlowerType <- rep(c(rep("clover",25), 
                    rep("goldenrod",25), 
                    rep("treefoil",25), 
                    rep("mixed",25)),2)

# create a variable called mass
Mass <- c(rnorm(n = 100,
                mean=32, 
                sd = 8), rnorm(n = 100, 
                               mean=21, 
                               sd=4))
# create a variable called Nosema Load
NosemaLoad <-c(rnorm(n = 100,
                     mean=100000, 
                     sd = 80000), rnorm(n = 100, 
                                        mean=500000, 
                                        sd=40000))

# create a variable called varroa load
VarroaLoad <- c(rnorm(n = 100,
                      mean=5, 
                      sd = 2), rnorm(n = 100, 
                                     mean=9, 
                                     sd=3))

# create a variable called time
Time <- rep(c(rep("Time1", 50), rep("Time2", 50)),2)

# create data frame
DF <- data.frame(ID, Origin, FlowerType, Mass, NosemaLoad, VarroaLoad, Time)

head(DF)


# Basic Graphics in ggplot:

# Summary stats using ddply:

# using ddply to get summary stats for mass:
DF1 <- ddply(DF, c("FlowerType"), summarise, 
             n = length(Mass),
             mean = mean(Mass, na.rm=TRUE),
             sd = sd(Mass, na.rm=TRUE),
             se = sd / sqrt(n))

print(DF1)


# Bar Plot
plot1 <- ggplot(DF1, aes(x=FlowerType,
                         y=mean)) + geom_bar(stat = "identity") 

plot1 + theme_minimal(base_size = 17) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + coord_cartesian(ylim = c(0, 40))


# Histogram
plot2 <- ggplot(DF, aes(Mass))

plot2 + geom_histogram() + stat_bin(bins = 30) + theme_minimal(base_size = 17)


# Scatterplot
plot3 <- ggplot(DF, aes(x=Mass, 
                        y=VarroaLoad))

plot3 + geom_point() + theme_minimal(base_size = 17) 

# Boxplot
plot4 <- ggplot(DF, aes(x=FlowerType, 
                        y=Mass))

plot4 + geom_boxplot() + theme_minimal(base_size = 17)



# More Complicated Graphics in ggplot

# using ddply to get summary stats for mass:
DF2 <- ddply(DF, c("FlowerType", "Origin"), summarise, 
             n = length(Mass),
             mean = mean(Mass, na.rm=TRUE),
             sd = sd(Mass, na.rm=TRUE),
             se = sd / sqrt(n))



# Bar Plot

#choosing color pallet


# Histogram
colors <- c("gray", "blue")

plot6 <- ggplot(DF, aes(x=Mass, fill=Origin)) +
  geom_histogram(binwidth=1, alpha=.7, position="identity") # alpha adds transparency

plot6 + theme_minimal(base_size = 17) + scale_fill_manual(values=colors) 

# Scatterplot
plot7 <- ggplot(DF, aes(x=Mass, 
                        y=VarroaLoad))

plot7 + geom_point(aes(color = Mass)) + theme_minimal(base_size = 17) + geom_smooth( method = "lm", se = FALSE)


# Boxplot
colors1 <- c("slategray3", "dodgerblue4", "blue", "lightblue")

plot8 <- ggplot(DF, aes(x=FlowerType, 
                        y=Mass, 
                        fill=FlowerType))

plot8 + geom_boxplot() + scale_fill_manual(values=colors1, guide_legend(NULL)) + guides(fill=FALSE) + theme_minimal(base_size = 17)


# Graphics for publication
DF3 <- ddply(DF, c("Time", "Origin"), summarise, 
             n = length(NosemaLoad),
             mean = mean(NosemaLoad, na.rm=TRUE),
             sd = sd(NosemaLoad, na.rm=TRUE),
             se = sd / sqrt(n))




plot9 <- ggplot(data = DF3, 
                aes(x = Time, 
                    y = mean, 
                    group = Origin)
) + geom_point(size=3) + scale_colour_manual(values = c("black", "black")) + labs(x = "Time", y = "Nosema Load (spores/bee)") + geom_line(aes(linetype=Origin), size=1) + scale_fill_brewer(palette = "Paired") + theme_classic(base_size = 17) + coord_cartesian(ylim = c(0, 690000), xlim = c(1,2)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.1))

plot9 + theme(legend.position=c(.15, .85)) + labs(linetype="Queen Origin") 