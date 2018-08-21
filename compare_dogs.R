library(weights)
library(data.table)
library(magrittr)
library(ggplot2)
library(rgdal)

repo <- "C:/Users/Laptop/Documents/Repos/explore_housing_data"
data_path <- "/data/housing_dataset.csv"
# this data is in .gitignore, not publicly avaliable in repo

df <- read.csv(paste0(repo,data_path)) %>% data.table
# TODO: make sure to use weighted means/tests with Weight_Completes
# to make means nationally representative estimates/conclusions
# Research question: Do homeowners and renters with dogs look for dwellings
# with more space (in square feet)?
# Hypothesis: Yes they do. More inhabitants means more space.

# Make separate dfs for people with and without dogs
big_dogs <- df$D1_007 == 1
small_dogs <- df$D1_008 == 1
any_dogs <- big_dogs | small_dogs
df[any_dogs, has_dog := 1]
df[!any_dogs, has_dog := 0]

# assumption: sq feet that people buy in a house is a better indicator of
# what they want than renting. overwrite rent values with
# bough values if avaliable
df[, desired_sq_ft := R20_003_001]
df[, desired_sq_ft := B40_003_001]
# the documentation says this is question 4 but few houses have thousands of
# partial bathrooms so I'm going with it

# are these variables related? use weighted t test
wtd_t_test <- wtd.t.test(df$has_dog, df$desired_sq_ft, weight=df$Weight_Completes)
# weighted t-test value rounds to 0 (trying unweighted because of
# the 0 rounded p-value)
t_test <- t.test(df$has_dog, df$desired_sq_ft)
# unweighted p-value is 2.2e-16
# t.test p values are less than 0.05
# indicating that having a dog plays some part in the square feet
# of a dwelling.

dogs <- df[any_dogs,]
no_dogs <- df[!any_dogs,]

# multiplying desired_sq_ft vectors by weights to produce weighted means
dog_sqft_mean <- weighted.mean(dogs$desired_sq_ft, dogs$Weight_Completes, na.rm=T)
no_dog_sqft_mean <- weighted.mean(no_dogs$desired_sq_ft, no_dogs$Weight_Completes, na.rm=T)

# people with no dogs report having, on average, 46 more square feet
# in their homes -- about 1% bigger. This indicates that having a dog
# isn't having an impact of people's decisons to desire larger houses
diff <- no_dog_sqft_mean - dog_sqft_mean
pct_diff <- no_dog_sqft_mean/dog_sqft_mean

# is this^ the case for all census regions?
census_path <- '/shapefiles/us_census_regions'
shp <- readOGR(dsn=paste0(repo, census_path), layer='cb_2017_us_division_500k')
poly <- fortify(shp)
id <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
geoid <- c(1, 2, 3, 4, 6, 7, 8, 5, 9)

lookup <- data.frame(id=id, geoid=geoid)
poly_m <- merge(poly, lookup, by='id')
# collapse by region
df <- df[, sqft_by_reg := weighted.mean(desired_sq_ft, Weight_Completes, na.rm=T), by=c('DV_NineRegions', 'has_dog')]
# unique by region
sq_ft_reg_df <- unique(df, by=c('DV_NineRegions', 'sqft_by_reg', 'has_dog'))
sq_ft_reg_df <- sq_ft_reg_df[order(DV_NineRegions, has_dog), .(DV_NineRegions, sqft_by_reg, has_dog)]

# calculate housing size difference by region
sq_ft_reg_df <- sq_ft_reg_df[, size_diff := diff(sqft_by_reg), by=DV_NineRegions]
# In the East South Central region (Kentucky to Mississippi), dog owners have significantly larger
# homes: about 443 or 23% more square feet than their dogless counterparts. 
# In every region except Easth South Central, homes with dogs are smaller or, on average,
# less than 50 square feet larger.
# allow.cartesian because many-to-many merge allows disaggregation by dog ownership later

m_poly <- merge(sq_ft_reg_df, poly_m, by.x='DV_NineRegions', by.y='geoid', allow.cartesian=T)

m_poly$has_dog <- factor(m_poly$has_dog)
levels(m_poly$has_dog) <- c("No Dog", "Dog")

# Map both
ggplot(m_poly, aes(long, lat, group=group)) +
  ggtitle("Where do dogs live in bigger houses?") + 
  geom_polygon(aes(fill=sqft_by_reg), color='white') +
  scale_x_continuous(limits=c(-125, -60)) +
  scale_fill_gradient(low='gray85', high='navy', name='Average Square Feet') +
  facet_grid(has_dog ~ .) +
  theme(axis.line=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())

# Map the differences
dogs_only <- m_poly[has_dog == 'Dog']
ggplot(dogs_only, aes(long, lat, group=group)) +
  ggtitle("How much bigger are homes with dogs?") + 
  geom_polygon(aes(fill=size_diff), color='white') +
  scale_x_continuous(limits=c(-125, -60)) +
  scale_fill_gradient2(low='orange',
                       mid='grey80',
                       high='blue',
                       breaks=seq(-100, 400, by=100),
                       name='Difference in Square \nFeet Between Homes \nwith and without Dogs') +
  theme(axis.line=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank())
