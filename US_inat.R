require(sp)
require(rgdal)
library("openair")
library("osmar")
library("tigris")
library("acs14lite")
api.key.install("6286bb04c4ced1805bc02cee7ac2ee06fc6f3d34")

#download zipcode shape file from acs
us<-zctas(cb=FALSE)

#download income for each zipcode tigris
income_us<-acs.fetch(endyear = 2012, 
                     geography = geo.make(zip.code="*"), 
                     variable = "B19013_001")
income_df <- as.data.frame(income_us@estimate)
colnames(income_df) <- c("hhincome")

#download race data acs for each zipcode
race_us<-acs.fetch(endyear=2014, span=5, geography = geo.make(zip.code="*"),
                   table.number="B03002")
race_df<-as.data.frame(race_us@estimate)
race_df<-data.frame( nhwhite= race_us@estimate[, "B03002_003"],
                   nhblack=race_us@estimate[, "B03002_004"], 
                   nhother=apply(race_us@estimate[, c(5:11)], MARGIN = 1, sum ),
                   hispanic =race_us@estimate[, "B03002_012"])

geography_df<-as.data.frame(race_us@geography)
rownames(geography_df)<-geography_df$NAME

us_income_race <-merge(income_df, race_df, by=0, all=TRUE)
rownames(us_income_race)<-us_income_race$Row.names
us_income_race<-merge(us_income_race, geography_df, by=0, all=TRUE)
#nstate <-sprintf("%02d", us_income_race$state)
#us_income_race$nstate<-nstate
#us_income_race$GEOID<-paste(as.character(us_income_race$nstate), as.character(us_income_race$county), sep="")
us_merge<-merge(us, us_income_race, "ZCTA5CE10", "zipcodetabulationarea")

us_landmarks<-landmarks(1,1 ,type="point")
us_roads<- roads(1,1)
us_water<- area_water(1,1)


inat <- read.csv("/Users/priyankadesouza/Downloads/inaturalistdata.csv", sep="\t")
inat <- inat[which(!is.na(inat$decimallatitude) & !is.na(inat$decimallongitude)),]
cnc <-read.csv("/Users/priyankadesouza/Downloads/citynaturechallenge.csv", sep= ",")

#subsetting CNC values which have valid latitudes and longitudes
cnc <- cnc[which(!is.na(inat$latitude) & !is.na(inat$longitude)),]

coordinates(inat) <- c("decimallongitude", "decimallatitude")
coordinates(cnc)<-c("longitude", "latitude")

proj4string(inat)<-CRS("+init=epsg:4326")
proj4string(cnc)<-CRS("+init=epsg:4326")
inat.df<-as.data.frame(inat)

us_merge<-spTransform(us_merge, CRS("+init=epsg:4326"))
us_water<-spTransform(us_water, CRS("+init=epsg:4326"))
us_roads<-spTransform(us_roads, CRS("+init=epsg:4326"))

#OSM parks file from Ian
osmparks <- readOGR("/Users/priyankadesouza/Desktop/Parks/NA_Parks_Forests/", "OSM_Parks_Forest_2163_cities")
proj4string(osmparks)<-CRS("+init=epsg:2163")
osmparks<-spTransform(osmparks, CRS("+init=epsg:4326"))

#Download urban areas from tigris package
urban=urban_areas(cb=TRUE)
urban1<-spTransform(urban1, CRS("+init=epsg:4326"))

#Urban shape file from Ian
urban2 <-readOGR("/Users/priyankadesouza/Desktop/Poly_US_Urban_areas/", "Poly_Urban_Areas2010_cUSA")
proj4string(urban2)<-CRS("+init=epsg:2163")
urban2<-spTransform(urban2, CRS("+init=epsg:4326"))

#WPDA parks from Ian
wpdaparks <- readOGR("/Users/priyankadesouza/Desktop/Parks/WDPA/", "WDPA_US_clean")
proj4string(wpdaparks)<-CRS("+init=epsg:2163")
wpdaparks<-spTransform(wpdaparks, CRS("+init=epsg:4326"))


#Finding intersections
wpdaparks_inat<-over(wpdaparks, inat)
urban1_inat<-over(urban1, inat)
urban2_inat<-over(urban2, inat)
us_merge_inat<-over(us_merge, inat)

