library(tidyverse)  # for data manipulation
library(urbnmapr)   # for plotting map

df <- data.frame(
  FIPS = fips,
  votes_dem = no,
  votes_gop = yes,
  area = areas
)

county_votes <- as_tibble(df)

# calculates hue based on party
hue <- function(party_winner){
  ifelse(party_winner == "democrat", 240, 0)
}

# calculates saturation based on vote margin
saturation <- function(dem_votes, rep_votes){
  total_votes = sum(dem_votes, rep_votes)
  
  abs(dem_votes - rep_votes) / total_votes
}

# calculates lightness based on total vote amount
lightness <- function(total_votes){
  # if county vote total >= upper fence then 50% lightness. 
  # lower vote total = higher lightness
  UPPERFENCE = 59828  # statistical upperfence
  tot_votes = if_else(total_votes >= UPPERFENCE, UPPERFENCE, total_votes)
  (
    ( (( 1 - ((tot_votes)/UPPERFENCE) ) * 100 ) / 2 ) + 50
  ) * .01
}

# calculates lightness based on votes per area
lightness_area <- function(total_votes, area){
  # if county vote total >= upper fence then 50% lightness. 
  # lower vote total = higher lightness
  UPPERFENCE = 100  # statistical upperfence, what is the best method to set this value?
  tot_votes = if_else(total_votes / area >= UPPERFENCE, UPPERFENCE, total_votes / area)
  (
    ( (( 1 - ((tot_votes)/UPPERFENCE) ) * 100 ) / 2 ) + 50
  ) * .01
}

###### Color Conversions
# specify h as whole input degrees (e.g 0-360)
# s = 0.0 - 1 (0 - 100%)
# l = 0.0 - 1, (0 - 100%)
# returns output from R's rgb() function
# source: https://stackoverflow.com/questions/28562288/how-to-use-the-hsl-hue-saturation-lightness-cylindric-color-model

hsl_to_rgb <- function(h, s, l) {
  h <- h / 360
  r <- g <- b <- 0.0
  if (s == 0) {
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) { t <- t + 1.0 }
      if (t > 1) { t <- t - 1.0 }
      if (t < 1/6) { return(p + (q - p) * 6.0 * t) }
      if (t < 1/2) { return(q) }
      if (t < 2/3) { return(p + ((q - p) * ((2/3) - t) * 6)) }
      return(p)
    }
    q <- ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))
    p <- 2.0 * l - q
    r <- hue_to_rgb(p, q, h + 1/3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1/3)
  }
  return(rgb(r,g,b))
}

county <- county_votes %>% 
  rowwise() %>% 
  mutate(dem_pct = votes_dem/(votes_dem + votes_gop),
         rep_pct = votes_gop/(votes_dem + votes_gop),
         party = if_else(votes_dem > votes_gop, "democrat", "republican"),
         
         # hue = party
         hue = hue(party),
         
         # saturation = party vote margin
         sat = saturation(votes_dem, votes_gop),
         
         # lightness = vote counts
         light = lightness_area(sum(votes_dem, votes_gop), area),
         
         # map color
         county_color = hsl_to_rgb(hue, sat, light),
         
         # map county boundaries. constant lightness of 50%
         county_boundary = hsl_to_rgb(hue, sat, 0.5)
         )


# adding spatial component
county_df <- left_join(county, counties, by = c("FIPS" = "county_fips"))

county_df %>% 
  ggplot(aes(long, lat, group=group)) +
  geom_polygon(aes(color = county_boundary, fill=county_color)) +
  coord_map("conic", lat0 = 30) +
  scale_color_identity() +
  scale_fill_identity()
