library(RColorBrewer)
library(classInt) 
library(sp)
library(rgdal)
library(maptools)
library(spdep)
library(spgwr)
library(PerformanceAnalytics)

#Number 1
#Extract the data frame from your shapefile
columbusdata <- columbus@data

class(columbusdata) # Class of object
## [1] "data.frame"
names(columbusdata) # Names of variables
##  [1] "AREA"       "PERIMETER"  "COLUMBUS_"  "COLUMBUS_I" "POLYID"    
##  [6] "NEIG"       "HOVAL"      "INC"        "CRIME"      "OPEN"      
## [11] "PLUMB"      "DISCBD"     "X"          "Y"          "NSA"       
## [16] "NSB"        "EW"         "CP"         "THOUS"      "NEIGNO"
str(columbusdata)   # Structure of the object (gives you details of variable types)

columbusdata_sub <- columbusdata [, 7:12]
summary(columbusdata_sub)
  
#Number 3
cor(columbusdata_sub) # Correlation 
# Correlation plot using PerformanceAnalytic package
chart.Correlation(columbusdata_sub, histogram=TRUE, pch=19)

#Number 4
# Plot the zones from the Columbus, Ohio data
plot(columbus,col='wheat') # Create a plot of columbus
# Add labels for each of the zones
text(coordinates(columbus),
     labels=as.character(columbus@data$POLYID),
     cex=0.6,font=2, col="darkred")
box(which='outer',lwd=2)

# Visualising a variable - Binary classification
plot(columbus, col = ifelse(columbus$CRIME > 40, "lightgrey", "red"))

#Number 5
pal = brewer.pal(7,"Greens") # Makes a 7-color spectral palette
display.brewer.pal(7, "Greens")  # This displays the colors

# Classifying Data using classIntervals() 
# Create class breaks 
brks.eq = classIntervals(columbus$CRIME, n = 7, style = "equal")
brks.qt = classIntervals(columbus$CRIME, n = 7, style = "quantile")
brks.jk = classIntervals(columbus$CRIME, n = 7, style = "jenks")
# Other style options "fixed", "sd", "pretty", "kmeans", "hclust", "bclust" and "fisher"

# Link the color pallette to the class breaks (categories) using
# findColours(CATEGORIES,PALETTE)
brks.eqcol = findColours(brks.eq,pal)
brks.qtcol = findColours(brks.qt,pal)
brks.jkcol = findColours(brks.jk,pal)

# Plot with Equal Breaks
plot(columbus,  col=brks.qtcol, border="black")
legend("topleft",leglabs(round(brks.eq$brks,digits=2)), fill=pal, cex=0.8, bty="n")
title(main="Columbus OH: Crime per 1000 households, 1980  \n (Equal breaks)")

# Plot with Quantile Breaks
plot(columbus,  col=brks.qtcol, border="black")
legend("topleft",leglabs(round(brks.qt$brks,digits=2)), fill=pal, cex=0.8, bty="n")
title(main="Columbus OH: Crime per 1000 households, 1980  \n (Quantile breaks)")
# Plot with Jenks Breaks
plot(columbus,  col=brks.jkcol, border="black")
legend("topleft",leglabs(round(brks.jk$brks,digits=2)), fill=pal, cex=0.8, bty="n")
title(main="Columbus OH: Crime per 1000 households, 1980  \n (Jenks breaks)", cex=0.5)

#Number 6
# To deduce neighbourhood structure from spatial polygons by poly2nb

columbus.nb = poly2nb(columbus, queen = T)  # Queen's case is the default. To change to Rook's case, set "queen = F i.e  columbus.nb = poly2nb(columbus, queen = F)

#row standardize the weight matrix
columbus.wts = nb2listw(columbus.nb, style="W") 
m = length(columbus$CRIME)
s = Szero(columbus.wts)

# Explore the weight matrix

# coordinates function extracts coordinates of the shapefile
columbuscoords <- coordinates(columbus)  
plot(columbus)
plot(columbus.wts, columbuscoords, pch=19, cex=0.6, add=TRUE)











#Number 12
# Load necessary libraries
library(spgwr)

# Load the "columbus" dataset
data(columbus)

# Fit GWR model with Gaussian weighting for variable selection
crime.bw <- gwr.sel(CRIME ~ INC + HOVAL, 
                    data=columbus,
                    coords=cbind(columbus$X, columbus$Y))

# Fit GWR model with selected bandwidth
crime.gauss <- gwr(CRIME ~ INC + HOVAL, 
                   data=columbus,
                   coords=cbind(columbus$X, columbus$Y),
                   bandwidth=crime.bw)

# Plot the distribution of beta coefficients
d <- cbind(crime.gauss$SDF$INC, crime.gauss$SDF$HOVAL)

# Set plot margins and create the boxplot
par(mar=c(3,4,2,2))
boxplot(d, xaxt="n", yaxt="n", pars=list(boxwex=0.3))
axis(1, at=1:2, label=c("Income", "Housing"))
axis(2, at=seq(-4, 2, 0.2), las=1)
abline(h=0, lty="4343", col="#7E7E7E")
mtext("Beta i", 2, line=3)





