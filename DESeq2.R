# DESeq2
# Lauren Ashlock

library( "DESeq2" )
library(ggplot2)

# Data

countsName <- "http://bioconnector.org/workshops/data/airway_scaledcounts.csv"
download.file(countsName, destfile = "airway_scaledcounts.csv", method = "auto")

countData <- read.csv('airway_scaledcounts.csv', header = TRUE, sep = ",")
head(countData)

metaDataName <- "http://bioconnector.org/workshops/data/airway_metadata.csv"
download.file(metaDataName, destfile = "airway_metadata.csv", method = "auto")

metaData <- read.csv('airway_metadata.csv', header = TRUE, sep = ",")
metaData


# Construct DESEQDataSet Object

dds <- DESeqDataSetFromMatrix(countData=countData, 
                              colData=metaData, 
                              design=~dex, tidy = TRUE)

dds <- DESeq(dds)

?DESeq

res <- results(dds)
head(results(dds, tidy=TRUE)) #let's look at the results table
summary(res) #summary of results

# Sort summary list by p-value
res <- res[order(res$padj),]
head(res)


# plotCounts

par(mfrow=c(2,3))

plotCounts(dds, gene="ENSG00000152583", intgroup="dex")
plotCounts(dds, gene="ENSG00000179094", intgroup="dex")
plotCounts(dds, gene="ENSG00000116584", intgroup="dex")
plotCounts(dds, gene="ENSG00000189221", intgroup="dex")
plotCounts(dds, gene="ENSG00000120129", intgroup="dex")
plotCounts(dds, gene="ENSG00000148175", intgroup="dex")



# Volcano plot

#reset par
par(mfrow=c(1,1))
# Make a basic volcano plot
with(res, plot(log2FoldChange, -log10(pvalue), pch=20, main="Volcano plot", xlim=c(-3,3)))

# Add colored points: blue if padj<0.01, red if log2FC>1 and padj<0.05)
with(subset(res, padj<.01 ), points(log2FoldChange, -log10(pvalue), pch=20, col="blue"))
with(subset(res, padj<.01 & abs(log2FoldChange)>2), points(log2FoldChange, -log10(pvalue), pch=20, col="red"))


# PCA
vsdata <- vst(dds, blind=FALSE)
plotPCA(vsdata, intgroup="dex") #using the DESEQ2 plotPCA fxn we can

