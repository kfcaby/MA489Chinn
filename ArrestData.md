ArrestData
================
Kevin Cummiskey
10/21/2020

``` r
library(tidyverse)
library(spacetime)
library(sp)
library(lubridate)
library(ggmap)
```

I recommend caching the results below (see Rmd file). As long as you
don’t edit this R chunk, the results will load from memory which is
much faster than redownloading every time.

``` r
arrests <- read_csv(file = "data/NYPD_Arrests_Data__Historic_.csv")

# convert the ARREST_DATE column to a Date
# this will make it easier later because you can use functions in the lubridate package
arrests <- arrests %>% 
  mutate(ARREST_DATE = mdy(ARREST_DATE))

# use only a sample of 2015 arrests for now to speed things up
set.seed(10)
arrests2015 <- arrests %>% 
  filter(year(ARREST_DATE) == 2015) %>% 
  sample_n(5000)

#outline of New York City
nyc <- c(left = -74.3,
         right = -73.6, 
         top = 40.95,
         bottom = 40.4)
```

The below code caches the map so you don’t have to download it every
time.

``` r
#it's easiest if you load the map once and then cache it, so it doesn't have to 
# import it everytime
nyc_map <- get_stamenmap(bbox = nyc, zoom = 10, type = "toner-lite")
```

Now, plot using the downloaded map.

``` r
nyc_map %>% ggmap() +
  geom_point(aes(x = Longitude, y = Latitude), data = arrests2015)
```

![](ArrestData_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->