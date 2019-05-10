# setwd("/home/rokkuran/workspace/maps/")
setwd("c:/workspace/maps/")

packages <- c('maps', 'mapdata', 'ggplot2', 'ggmap', 'rjson', 'plyr')
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, dependencies=TRUE, repos='https://cran.ms.unimelb.edu.au/')
}


library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(rjson)
library(plyr)

config <- fromJSON(file='config.json')
register_google(key=config$google_api_key) 


world <- map_data('world')

visited <- read.csv('visited.csv')  # list of countries visited
person_a <- as.vector(visited[visited$person=='PersonA', ]$country)
person_b <- as.vector(visited[visited$person=='PersonB', ]$country)
planned <- as.vector(visited[visited$person=='Planned', ]$country)


# get flights data
df <- read.csv('flights.csv')

locations.fname <- 'c:/workspace/maps/locations.csv'
if (file.exists(locations.fname)) {
  locations.saved <- read.csv(file=locations.fname)
} else {
  locations.saved <- data.frame(city=c(), lat=c(), lon=c())
}

cities <- Reduce(union, list(df$start, df$end))

# get missing locations
locations.missing <- setdiff(cities, locations.saved$city)

# get missing locations using google geocode api
if (length(locations.missing > 0)) {
  locations.new <- geocode(locations.missing)
  locations.new$city <- locations.missing
  print(locations.new)

  locations.all <- rbind(locations.saved, locations.new)
} else {
  locations.all <- locations.saved
}

# save for next time
write.csv(locations.all, "locations.csv", row.names=FALSE)


apply_map <- function(df, from, to) {
  return(as.numeric(as.character(mapvalues(df, from=from, to=to))))
}

# map cities lat/lon to flights dataframe
df$start.lat <- apply_map(df$start, locations.all$city, locations.all$lat)
df$start.lon <- apply_map(df$start, locations.all$city, locations.all$lon)
df$end.lat <- apply_map(df$end, locations.all$city, locations.all$lat)
df$end.lon <- apply_map(df$end, locations.all$city, locations.all$lon)


add_country <- function(df, country, fill) {
  gpoly <- geom_polygon(
  data=df[df$region == country,],
    aes(x=long, y=lat, group=group),
    fill=fill,
    alpha=0.5
  )
  return(gpoly)
}

add_countries <- function(p, df, countries, colour) {
  for (country in countries) {
    p <- p + add_country(df, country, colour)
  }
  return(p)
}


mp <- ggplot() + borders("world", colour="black", fill="white", size=0.5)
# shade red/blue and jointly visited countries are purple (alpha is 0.5)
mp <- add_countries(mp, world, person_a, 'red')
mp <- add_countries(mp, world, person_b, 'blue')
ggsave('output/plot.png', plot=mp, width=20, height=12)


# add cities dots from route paths
mp <- mp + geom_point(
  aes(x=locations.all$lon, y=locations.all$lat),
  color='green', size=1
)
ggsave('output/plot_with_cities.png', plot=mp, width=20, height=12)


# add route paths (flights in this example) to map using curves between cities
mp <- mp + geom_curve(
  aes(x=df$start.lon, y=df$start.lat, xend=df$end.lon, yend=df$end.lat),
  size=0.25,
  color="magenta",
  curvature=0.1
)
ggsave('output/plot_with_cities_and_curve.png', plot=mp, width=20, height=12)
