# Animation Package
# Emily Mikucki

library(animation)

#2-faced coin with equal probability (0.5 or NULL). 
#quartz()
flip.coin(faces = 2, prob = NULL, border = "white", grid = "white", col = 1:2,
          type = "p", pch = 21, bg = "transparent", digits = 3) 

#2-faced coin with unequal probability (0.9/0.1).
flip.coin(faces = 2, prob = c(0.9, 0.1), border = "white", grid = "white", col = 1:2,
          type = "p", pch = 21, bg = "transparent", digits = 3) 

#Coin with 3 potential outcomes
oopt = ani.options(interval = 0.3, nmax = ifelse(interactive(), #if else(interactive() meaning assumed human operator
                                                 50, 2))

## A simulation where a coin could either be heads, tails or "stand on a table"
flip.coin(faces = c("Head", "Stand", "Tail"), type = "n", prob = c(0.475, 0.05, 0.475), col = c(1, 2, 4))

## Save as a HTML animation page (links to your working directory)
saveHTML({
  ani.options(interval = 0.3, nmax = ifelse(interactive(), 50, 
                                            2)) 
  par(mar = c(2, 3, 2, 1.5), mgp = c(1.5, 0.5, 0))
  flip.coin(faces = c("Head", "Stand", "Tail"), type = "n", 
            prob = c(0.475, 0.05, 0.475), col = c(1, 2, 4))
}, img.name = "flip.coin", htmlfile = "flip.coin.html", ani.height = 500, 
ani.width = 600, title = "Probability of flipping coins", 
description = c("This animation has provided a simulation of flipping coins", "which might be helpful in understanding the concept of probability."))

# Suppose there are two plant species in a field: A and B. 
# One of them will die at each time and a new plant will grow in the place where the old plant died. 
# The species of the new plant depends on the proportions of two species: the larger the proportion is, the greater the probability for this species to come up will be

oopt = ani.options(nmax = ifelse(interactive(), 25, 2), interval = 0.3)
par(ann = FALSE, mar = rep(0, 4))
ecol.death.sim(nr = 10, nc = 10, num.sp = c(50, 50), col.sp = c(8, 2), pch.sp = c(20,17), col.die = 1, pch.die = 4, cex = 3)

## Save as a HTML animation page (links to your working directory)
saveHTML({
  ani.options(interval = 0.3, nmax = ifelse(interactive(), 25, 
                                            2)) 
  par(ann = FALSE, mar = rep(0, 4))
  ecol.death.sim(nr = 10, nc = 10, num.sp = c(50, 50), col.sp = c(8, 2), pch.sp = c(20,17), col.die = 1, pch.die = 4, cex = 3)
}, img.name = "ecol.death.sim", htmlfile = "ecol.death.sim.html", ani.height = 500, 
ani.width = 600, title = "Ecological Death Simulation", 
description = c("This animation shows the simulation of the death of two species with certain probabilities."))
#ani.options(oopt)

ecol.death.sim = function(
  nr = 10, nc = 10, num.sp = c(50, 50), col.sp = c(1, 2), pch.sp = c(1, 2),
  col.die = 1, pch.die = 4, cex = 3, ...
) {
  x = rep(1:nc, nr)
  y = rep(1:nr, each = nc)
  p = factor(sample(c(rep(1, 25), rep(2,75))), levels = 1:2)
  nmax = ani.options('nmax')
  for (i in 1:nmax) {
    dev.hold() #this gives a way to hold/flush output on certain on-screen devices
    plot(1:nc, 1:nr, type = 'n', xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5), ...)
    abline(h = 1:nr, v = 1:nc, col = 'lightgray', lty = 3)
    points(x, y, col = col.sp[p], pch = pch.sp[p], cex = cex)
    ani.pause()
    idx = sample(nr * nc, 1)
    points(x[idx], y[idx], pch = pch.die, col = col.die, cex = cex, lwd = 3)
    tbl = as.vector(table(p))
    tbl = tbl + if (as.integer(p[idx]) > 1) c(0, -1) else c(-1, 0)
    p[idx] = sample(1:2, 1, prob = tbl)
    ani.pause()
  }
  invisible(p)
}
ecol.death.sim()

# simple animation
oopt = ani.options(interval = 1.5)
saveHTML({
  for (i in 1:5) {
    plot(runif(10), ylim = c(0, 1))
    ani.pause()
  }
}, htmlfile = "simple.plot.html", img.name = "simple.plot")


# animation of ranwalk
RanWalk <- function(times=100, 
                    n1=50,
                    lambda=1.001, 
                    noiseSD=10) {
  n <- rep(NA, times)
  n[1] <- n1
  noise <- rnorm(n=times, mean=0, sd=noiseSD)
  
  for (i in 1:(times-1)) {
    n[i + 1] <- lambda*n[i] + noise[i] #would write in a manusript as n(t+1) = lambda(nt) + e
    if(n[i + 1] <= 0){
      n[i+ 1] <- NA
      cat("Population extinction at time", 
          i-1, "\n")
      #tkbell()
      break} #end of conditional statement
  } #end of for loop
  return(n)
} #end of function
head(RanWalk())

saveHTML({
  for (i in 1:10) {
    plot(RanWalk(), type="o")
    ani.pause()
  }
}, htmlfile = "RanWalk.html", img.name = "RanWalk.plot", description = "Animation of stochastic random walks (model population growth)")



#For this example expand your plot window!
#This animation plots the density functions of 150 draws of 100 values from a normally distributed random variable
#Set delay between frames when replaying
ani.options(interval=.05)

# Set up a vector of colors for use below 
col.range <- heat.colors(15)

# Begin animation loop
saveHTML({
  layout(matrix(c(1, rep(2, 5)), 6, 1))
  par(mar=c(3,3,1,1) + 0.1)
  for (i in 1:150) { # Begin the loop that creates the 150 individual graphs
    chunk <- rnorm(100)+sqrt(abs((i)-51))  # Pull 100 observations from a normal distribution
    # and add a constant based on the iteration to move the distribution
    par(fg=1) # Reset the color of the top chart every time (so that it doesn’t change as the 
    # bottom chart changes)
    # Set up the top chart that keeps track of the current frame/iteration
    plot(-5, xlim = c(1,150), ylim = c(0, .3), axes = F, xlab = "", ylab = "", main = "Iteration") 
    abline(v=i, lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    abline(v=i-1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    abline(v=i-2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    # Bring back the X axis
    axis(1)
    # Set the color of the bottom chart based on the distance of the distribution’s mean from 0
    par(fg = col.range[mean(chunk)+3])
    # Set up the bottom chart
    plot(density(chunk), xlab = "x-value", xlim = c(-5, 15), ylim = c(0, .6))
    # Add a line that indicates the mean of the distribution. Add additional lines to track
    # previous means
    abline(v=mean(chunk), col = rgb(255, 0, 0, 255, maxColorValue=255))
    if (exists("lastmean")) {abline(v=lastmean, col = rgb(255, 0, 0, 50, maxColorValue=255)); prevlastmean <- lastmean;}
    #Fix last mean calculation
    lastmean <- mean(chunk)
  }
}, img.name = "density_normal_dist", htmlfile = "normal.dist.html", ani.height = 500, 
ani.width = 600, title = "Normal Distribution", 
description = c("This animation plots the density functions of 150 draws of 100 values from a normally distributed random variable.") )