# Assignment 3 
## GEOG418
## Bethany Worsnop


## Introduction

Spatial autocorrelation is an important tool in spatial pattern analysis as it helps quantify how much data is clustered, dispersed, or randomly distributed.  It relates a variable and its values at their geographical location to neighboring locations to determine if they are similar (positive autocorrelation), or dissimilar to one another (negative autocorrelation). This tutorial breaks down how to perform spatial autocorrelation analyses (Global and Local Moran’s I) and determine the significance with a Z test. We also break down how to display the results to better understand the patterns using different maps and plots.

R uses libraries to perform tasks beyond what is included in R base package.  Libraries are not included with R so you must download each package individually. These libraries/packages are created by the community and need to be updated frequently. Then you need to load each package into your current R session to use the functions that they provide.


```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
#install.packages("knitr")
#install.packages("sp")
#install.packages("tmap")
#install.packages("spdep")
#install.packages("raster")
#install.packages("shinyjs")
#install.packages("e1071")
#install.packages("sf")

library("knitr")
library("sp")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")
library("sf")


```

The working directory sets the path to the folder where your files are stored. This simplifies future code so you don't have to call on the entire path each time and instead can just use the file name as it will already know where to look for the file by using the working directory. To set a working directory you want to input the path to your folder where your projects files/data is stored into the directory. We also use a check to make sure we have the correct working directory.
```{r set directory, echo=TRUE, eval=TRUE, warning=FALSE}
#setting the directory
dir <- "C:/Users/betha/OneDrive/Desktop/GEOG418/Assn3"
setwd(dir)
getwd()

```
First you will want to read in the census data into a data frame and read the shape file for census boundaries into R as a st dataframe. The .csv file is our census data including information collected using census. The .shp file is boundary information for census data which we will be using to specify which City we are going to be analyzing. 

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#From the working directory read in the csv
csv <- read.csv("C:/Users/betha/OneDrive/Desktop/GEOG418/Assn3/ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file
shp <- st_read("C:/Users/betha/OneDrive/Desktop/GEOG418/Assn3/lda_000b16a_e.shp") 

```

The next step is to make our data easier to use by cleaning it up. To do this we will create a vector with the column names so we can refer to the data easily when calling on it. Next, we will remove unnecessary rows and combine our simplified census data with our boundary dataframe. Lastly, we will subset our data to our city of interest ‘Nanaimo’ and turn any count data into a rate for mapping and analysis.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for nanaimo
Municp <- subset(census_DAs, census_DAs$CMANAME == "Nanaimo")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Missing data sometimes takes the value of 0 or NA which does not accurately represent. We now want to remove any 0 or NA values for our analyses to run properly as they can change our results. To do this we remove any polygons with NA or 0 values for the variables of interest. The variables of interest for this study are median total income or knowledge of French.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$`PercFrench`)),]
```

Now we will take a closer look at the variables of interest for this study: median total income and percentage of respondents with French knowledge. We will do some descriptive stats including mean, standard deviation, and skewness and we created a table to show our results.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- skewness(French_noNA$PercFrench)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
![alt text](https://github.com/bworsnop/Assignment3GEOG418/blob/main/table.png)
```

Next, we will be creating maps using the tmap package. First map to create is our median total income map for Nanaimo. We first select the joint information of our census data and boundary polygon and decide on our color pallet, style of pallet, and polygon border color. We also create titles and decide the layout of the maps. We repeat this for the French knowledge map. Lastly, we print the maps side by side using tmap_arrange function.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Nanaimo's census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
# tmaptools::palette_explorer() #Tool for selecting pallettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "BuPu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "BuPu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
![alt text](https://github.com/bworsnop/Assignment3GEOG418/blob/main/TmMapoverall.png)
## Neighbourhood matrix

A weighted neighbourhood matrix quantifies the relationship between points/features and their neighbouring points/features. The definition of neighbour is subjective however, we have standards when it comes to defining out weighted neighbourhood matrix for spatial analysis. For this study we will be using the Rook and Queen standards.

Rook Weights is where the four adjacent locations are considered neighbours.

Queen Weights defines the neighbours as the surrounding eight points/polygons.

Luckily R has a the poly2nb() function in the ‘spdep’ package which helps create our neighbours list and weights. To change from queen weighting to rook weighting we can change the ‘queen= TRUE’  to ‘queen = FALSE’ as queen weighting is our default standard.


```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)

```

The neighbourhood matrix maps are created using the tm_shape() function in the tmap package. Each map is first individually created with the borders defined and then lines for defining the neighbours for the corresponding neighbourhood scheme. Once all three maps are created they are arranged side by side with the tmap_arrange() function.

The weighting maps show each polygon and a connecting line to each of its determined neighbors for each scheme. As you can see, changing between queen and rook scheme in this example does not really alter the results of the neighbourhood matrix.


```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Nanaimo census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='red')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='yellow', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='yellow', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

![alt text](https://github.com/bworsnop/Assignment3GEOG418/blob/main/threepane.png)
```

Next a weighted matrix is created. We use a binary weighting scheme where each neighbour is given value of 1 and all other polygons are assigned 0. To create a weighted matrix in R we use the “nb2listw” function from the “spdep” library. We apply this function to the vri.nb variable as it contains the neighbour links. To get the function to run even if there is a polygon with zero neighbour links we st “zero.policy” as equal to “TRUE”.


```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```


## Global Moran’s I

Now we can apply our determined weights for our neighbours to calculate Global Moran’s I. Global morans I is a measure for spatial autocorrelation across the whole study area ranging between -1 and 1. The equation for Global Moran’s I statistic is:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

$x$ is the variable under analysis and $x_i$ is the location of interest for this variable. $x_j$ denotes the neighbouring value to $x_i$. $x_j$ locations were determined from out weighting scheme (queen scheme). The spatial weighting matrix ($W_{I,j}$) is applied by being multiplied by the difference between $x_i$ and the mean of $x$  and $x_j$ and the mean of $x$. 

To standardize these values, we have our denominator. This causes high values of Moran’s I to indicate positive spatial autocorrelation and low represent negative spatial autocorrelation. Here we are calculating Global Moran’s I thus out value represents our entire dataset and provides insight into the measure of spatial autocorrelation for the whole study area.


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

The Global Moran’s I for Income was calculated as 0.5843 and for French knowledge is 0.1318. These results indicate that there is global spatial autocorrelation (clustering) in both variables. 

To go a step further we can determine if these results are statistically significant by performing a Z-test. For this case our null hypothesis is our data is significantly spatially autocorrelated and our alternative hypothesis is our data is not significantly autocorrelated. Using 95% confidence ( $\alpha$ of 0.5) we can use our z-score and compare to the value of 1.96. If our z-score lands above 1.96 indicates significant positive spatial autocorrelation and if it lands below, It indicates insignificant positive spatial autocorrelation.

The Z-score can be calculated using the following code:


```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

The zscores for both variable confirm that there is significant spatial autocorrelation. Median Income has a z-score if 12.9944 and z-score for French knowledge is 3.0577. Both of these scores suggest that our positive spatial autocorrelation is significant.

## Local spatial autocorrelation

Local spatial autocorrelation gives a Morans I value for each point location signifying how similar that location is autocorrelated to its neighbours. This Morans I does not represent the whole data set but instead that individual variable at $x_i$. The calculation for Local Moran’s I is very similar to the Global calculation however, is slightly re arranged.


$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Instead of typing out this entire calculation we can use the localmoran() function to perform the calculations for us. We do need to input the variable of interest as well as our determined weighting scheme for this function to work.


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

Now using the same mapping function as before we can visualize our results and analyze what this test is doing.

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Nanaimo's census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
![alt text](https://github.com/bworsnop/Assignment3GEOG418/blob/main/LocalI_map.png)
```

The Local Moran’s I scores for the median total income in Nanaimo show Lantzville, downtown Nanaimo, and Hammond Bay exhibit high Local Moran’s I values indicating positive spatial correlation. The most negative Moran’s I values for Nanaimo were in southern Newcastle indicating negative spatial autocorrelation in this area. For areas with French knowledge the Local Moran’s I value show that Hammond bay has positive spatial autocorrelation due to the high Local Moran’s I values. The areas on negative spatial autocorrelation for French knowledge are not in any particular area.

These maps are great at showing where the polygons are positively spatially autocorrelated however, it can be useful to also to graph this data so show better how many of our polygons are significantly positively or negatively autocorrelated. 


```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
![alt text](https://github.com/bworsnop/Assignment3GEOG418/blob/main/income_plot.png)
```


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
![alt text](https://github.com/bworsnop/Assignment3GEOG418/blob/main/french_plot.png)
```

Both of these plots show clustering in our variables. The diamonds are points that are considered statistically significant (95% confidence). The total median income scatter plot shows a even slope showing high similarity between our data points and each of their neighbours. The respondents with knowledge of French scatter plot shows that our data is skewed so that census tracks are more similar to their neighbours than dissimilar. The slope if also shallow indicating that it is not as strong of clustering.



## Summary

Overall, we went through how perform vital calculations for doing analyzing spatial autocorrelation as well as shown different ways of displaying our data. We showed how to create tables, maps, and plots in R. 
We used Nanaimo as an example for a spatial autocorrelation analysis of total median income and knowledge of French. Both our local and global Moran's I generally indicated positive spatial autocorrelation. 
Our local Moran’s I for total median income highlighted the communities of Lantzville, Hammond Bay, and downtown Nanaimo to exhibit the as significantly positively autocorrelated and south Newcastle as an area of significant negative spatial autocorrelation.  Both global and local Moran’s I suggest that there is clustering of total median income.  
The local Moran’s I for French knowledge only highlighted Hammond bay and a small area downtown to have significant positive autocorrelation with while showing a dispersal of significantly negative autocorrelated polygons. The skewness of our plot of local Moran's I showed that clustered areas are more similar than dissimilar for areas of French knowledge.
Our results are useful for businesses when determining the demographic which they are intending to aim for. Such as the areas of significantly clustered high-income businesses may feel inclined to charge more or provide a higher service because they will have the demographic to back them up. We can use the clusters of French knowledgeable communities to implement more resources for these communities to help them cherish their French knowledge.
