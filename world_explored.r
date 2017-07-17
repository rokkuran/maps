setwd("/home/rokkuran/workspace/maps/")
# path_libs <- "/home/rokkuran/workspace/maps/"
packages <- c('maps', 'mapdata', 'ggplot2', 'ggmap')

# new_packages <- packages[!(packages %in% installed.packages(lib=path_libs)[,"Package"])]
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, lib=path_libs, dependencies=TRUE)
}

# library(maps, lib=path_libs)
# library(mapdata, lib=path_libs)
# library(ggplot2, lib=path_libs)
# library(ggmap, lib=path_libs)

library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)


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


world <- map_data('world')

visited <- read.csv('visited.csv')  # list of countries visited
lachlan <- as.vector(visited[visited$person=='Lachlan', ]$country)
bronwyn <- as.vector(visited[visited$person=='Bronwyn', ]$country)
planned <- as.vector(visited[visited$person=='Planned', ]$country)


mp <- ggplot() + borders("world", colour="black", fill="white", size=0.5)

# shade red/blue and jointly visited countries are purple (alpha is 0.5)
mp <- add_countries(mp, world, lachlan, 'red')
mp <- add_countries(mp, world, bronwyn, 'blue')

ggsave('output/plot.png', plot=mp, width=20, height=12)

mp <- add_countries(mp, world, planned, 'green')
ggsave('output/plot_with_planned.png', plot=mp, width=20, height=12)


df <- read.csv('flights.csv')
print(df)

# TODO: save values, this is stupid.
starts <- geocode(as.vector(df$start))
ends <- geocode(as.vector(df$end))


# add cities dots from route paths
locations <- unique(rbind(starts, ends))
mp <- mp + geom_point(
  aes(x=locations$lon, y=locations$lat),
  color='green', size=1
)

ggsave('output/plot_with_cities.png', plot=mp, width=20, height=12)


# add route paths (flights in this example) to map using curves between cities
mp <- mp + geom_curve(
  aes(x=starts$lon, y=starts$lat, xend=ends$lon, yend=ends$lat),
  size=0.25,
  color="magenta",
  curvature=0.1
)

ggsave('output/plot_with_cities_and_curve.png', plot=mp, width=20, height=12)
