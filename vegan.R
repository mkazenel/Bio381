# Vegan Tutorial
# Peter Clark
# 26 April 2017

library(vegan)

data(package = "vegan") ## names of data sets in the package
data(dune) # Vegetation and Environment in Dutch Dune Meadows
str(dune) #a data frame of observations of 30 species at 20 sites

# Diversity functions

diversity(dune,index = "simpson") # calculate Simpson's 1-D Index of Diversity for each site. # closer to 1 = greater diversity

simpson <- diversity(dune, "simpson") # or assign to var.
simpson 

shannon <- diversity(dune) # note that Shannon's is default
shannon #Typically ranges from 1.5 - 3.4, higher = more diverse 

# lets compare the two
par(mfrow = c(1, 2))  # use par to generate panels with 1 row of 2 graphs
hist(simpson)
hist(shannon)


# vegdist - pairwise comparisons of dissimilarity
par(mfrow = c(1, 2))
bray = vegdist(dune, "bray") 
gower = vegdist(dune, "gower")
hist(bray, xlim = range(0.0,1.0))
hist(gower, xlim = range(0.0,1.0))


# Rarefaction

spAbund <- rowSums(dune)  #gives the number of individuals found in each plot
spAbund # view observations per plot

raremin <- min(rowSums(dune))  #rarefaction uses the smallest number of observations per sample to extrapolate the expected number if all other samples only had that number of observations
raremin # view smallest # of obs (site 17)

sRare <- rarefy(dune, raremin) # now use function rarefy
sRare #gives an "expected"rarefied" number of species (not obs) if only 15 individuals were present

par(mfrow = c(1, 1))
rarecurve(dune, col = "blue") # produces rarefaction curves # squares are site numbers positioned at observed space. To "rarefy" a larger site, follow the rarefaction curve until the curve corresponds with the lesser site obs. This gives you rarefied species richness


# NMDS

set.seed(2) # random no. generator / way to specify seeds, 2=no. of integers?
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10, # counts up to 100, 300 cells
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))
head(community_matrix)

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions. Increase if high stress is problem. 

plot(example_NMDS)

ordiplot(example_NMDS,type="n") #Ordination plot function especially for congested plots
orditorp(example_NMDS,display="species",col="red",air=0.01) #The function adds text or points to ordination plots
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)

treat=c(rep("Treatment1",5),rep("Treatment2",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

#spider plot
ordiplot(example_NMDS,type="n")
ordispider(example_NMDS,groups=treat)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

# Define random elevations for previous example
elevation=runif(10,0.5,1.5)
# Use the function ordisurf to plot contour lines
ordisurf(example_NMDS,elevation,main="",col="forestgreen")
# Finally, display species on plot
orditorp(example_NMDS,display="species",col="grey30",air=0.1,
         cex=1)

