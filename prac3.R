library(sf)
library(here)
st_layers(here("prac3_data", "gadm36_AUS.gpkg"))
library(sf)
Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
print(Ausoutline)
library(sf)
st_crs(Ausoutline)$proj4string

#reproject data to unit measure in meteres, because WGS84 measures in degrees
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)
#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
#AusoutlineSF <- AusoutlineSP %>%
#  st_as_sf()

#load worldclim data
library(raster)
jan<-raster(here("prac3_data", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan
plot(jan)

# set the proj 4 to a new object
newproj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  projectRaster(., crs=newproj)
plot(pr1)

#if we just wanted to go back from Mollweide to WGS84 we can simply set the crs to "+init=epsg:4326"

pr1 <- pr1 %>%
  projectRaster(., crs="+init=epsg:4326")
plot(pr1)

#more efficient way to load data
# look in our folder, find the files that end with .tif and 
library(fs)
dir_info("prac3_data/") 
#dir info lists all the files in the directory
#now select with dplyr, note select exist withiin both raster and dplyr
library(tidyverse)
listfiles<-dir_info("prac3_data/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles

#Then load all of the data straight into a raster stack. A raster stack is a collection of raster layers with the same spatial extent and resolution.
worldclimtemp <- listfiles %>%
  stack()

#have a look at the raster stack
worldclimtemp
#In the raster stack you’ll notice that under dimensions there are 12 layers (nlayers). The stack has loaded the 12 months of average temperature data for us in order.
# access the january layer
worldclimtemp[[1]]

#We can also rename our layers within the stack:
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month
#Now to get data for just January use our new layer name
worldclimtemp$Jan

#Using a raster stack we can extract data with a single command!! For example let’s make a dataframe of some sample sites — Australian cities/towns.
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- raster::extract(worldclimtemp, samples)

#Add the city names to the rows of AUcitytemp
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")




#Descriptive statistics
#Let’s take Perth as an example. We can subset our data either using the row name:
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
#Or the row location:
Perthtemp <- Aucitytemp2[3,]

#HIstogram
#Make a histogram of Perth’s temperature. The tibble stored the data as double and the base hist() function needs it as numeric..
hist(as.numeric(Perthtemp))
#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)
hist(as.numeric(Perthtemp), 
     breaks=userbreak, 
     col="green", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")
#check out the historgram info
histinfo <- Perthtemp %>%
  as.numeric()%>%
  hist(.)
histinfo

#to plot the whole astrulia data, first check layer 
plot(Ausoutline$geom)

#But as the .shp is quite complex (i.e. lots of points) we can simplify it first with the rmapshaper package — install that now..if it doesn’t load (or crashes your PC) this isn’t an issue. It’s just good practice that when you load data into R you check to see what it looks like…
#load the rmapshaper package
library(rmapshaper)
#simplify the shapefile
#keep specifies the % of points
#to keep
AusoutSIMPLE<-Ausoutline %>%
  ms_simplify(.,keep=0.05)

plot(AusoutSIMPLE$geom)

#Next, set our map extent (where we want to clip the data to) to the outline of Australia then crop our WorldClim dataset to it.
#HOWEVER, we need to make sure that both of our layers are in the same coordinate reference system when we combine them…so..
print(Ausoutline)
#this works nicely for rasters
crs(worldclimtemp)
## CRS arguments: +proj=longlat +datum=WGS84 +no_defs

#Perfect! Now let’s contiune…

Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  crop(worldclimtemp,.)

# plot the output
plot(Austemp)

#You’ll notice that whilst we have the whole of Australia the raster hasn’t been perfectly clipped to the exact outline….the extent just specifies an extent box that will cover the whole of the shape.

#If want to just get raster data within the outline of the shape:
  exactAus <- Austemp %>%
  mask(.,Ausoutline, na.rm=TRUE)

#Let’s re-compute our histogram for Australia in March. We could just use hist like we have done before. We can either subset using the location (we know March is thrid in the RasterBrick).
#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")
#We can also subset based on the name of the Brick, sadly we can’t apply filter() from dplyr (like we did earlier when filtering Perth) yet to rasters…

#OR
#subset with the word Mar
hist(raster::subset(exactAus, "Mar"), col="red", main ="March temperature")

#ggplot will be nicer
#We need to make our raster into a data.frame to be compatible with ggplot2, using a dataframe or tibble
exactAusdf <- exactAus %>%
  as.data.frame()
library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

#How about plotting multiple months of temperature data on the same histogram
#as we did in practical 2, we need to put our variaible (months) into a one coloumn using pivot_longer(). Here, we are saying select columns 1-12 (all the months) and place them in a new column called Month and their values in another called Temp
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )
#Then subset the data, selecting two months using filter() from dplyr
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")
#Get the mean for each month we selected, remember group_by() and summarise() from last week?
  meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))
  meantwomonths 
#Select the colour and fill based on the variable (which is our month). The intercept is the mean we just calculated, with the lines also based on the coloumn variable.
  ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
    geom_histogram(position="identity", alpha=0.5)+
    geom_vline(data=meantwomonths, 
               aes(xintercept=mean, 
                   color=Month),
               linetype="dashed")+
    labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
         x="Temperature",
         y="Frequency")+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))
#Have you been getting an annoying error message about bin size and non-finate values? Me too!…Bin size defaults to 30 in ggplot2 and the non-finate values is referring to lots of NAs (no data) that we have in our dataset. In the code below i’ve:
#dropped all the NAs with drop_na()
#made sure that the Month column has the levels specified, which will map in descending order (e.g. Jan, Feb, March..)
#selected a bin width of 5 and produced a faceted plot…
  data_complete_cases <- squishdata %>%
    drop_na()%>% 
    mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                            "Apr","May","Jun",
                                            "Jul","Aug","Sep",
                                            "Oct","Nov","Dec")))
  
  # Plot faceted histogram
  ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
    geom_histogram(color="black", binwidth = 5)+
    labs(title="Ggplot2 faceted histogram of Australian temperatures", 
         x="Temperature",
         y="Frequency")+
    facet_grid(Month ~ .)+
    theme(plot.title = element_text(hjust = 0.5))
#Does this seem right to you? Well…yes. It shows that the distribution of temperature is higher (or warmer) in the Australian summer (Dec-Feb) than the rest of the year, which makes perfect sense.
#How about an interactive histogram using plotly…
#See if you can understand what is going on in the code below. Run each line seperately.
  library(plotly) 
# split the data for plotly based on month  
  jan <- squishdata %>%
    drop_na() %>%
    filter(., Month=="Jan")
  
  jun <- squishdata %>%
    drop_na() %>%
    filter(., Month=="Jun")
# give axis titles
  x <- list (title = "Temperature")
  y <- list (title = "Frequency")  
# set the bin width
  xbinsno<-list(start=0, end=40, size = 2.5)
  
# plot the histogram calling all the variables we just set
  ihist<-plot_ly(alpha = 0.6) %>%
    add_histogram(x = jan$Temp,
                  xbins=xbinsno, name="January") %>%
    add_histogram(x = jun$Temp,
                  xbins=xbinsno, name="June") %>% 
    layout(barmode = "overlay", xaxis=x, yaxis=y)
  
  ihist
  