library(tidyverse)  # for data manipulation
install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)   # for plotting map


fips = c("21001", "21003", "21005", "21007", "21009", "21011", "21013", "21015", "21017", "21019", "21021", "21023", "21025", "21027", "21029", "21031", "21033", "21035", "21037", "21039", "21041", "21043", "21045", "21047", "21049", "21051", "21053", "21055", "21057", "21059", "21061", "21063", "21065", "21067", "21069", "21071", "21073", "21075", "21077", "21079", "21081", "21083", "21085", "21087", "21089", "21091", "21093", "21095", "21097", "21099", "21101", "21103", "21105", "21107", "21109", "21111", "21113", "21115", "21117", "21119", "21121", "21123", "21125", "21127", "21129", "21131", "21133", "21135", "21137", "21139", "21141", "21143", "21145", "21147", "21149", "21151", "21153", "21155", "21157", "21159", "21161", "21163", "21165", "21167", "21169", "21171", "21173", "21175", "21177", "21179", "21181", "21183", "21185", "21187", "21189", "21191", "21193", "21195", "21197", "21199", "21201", "21203", "21205", "21207", "21209", "21211", "21213", "21215", "21217", "21219", "21221", "21223", "21225", "21227", "21229", "21231", "21233", "21235", "21237", "21239")
yes = c(4074, 3662, 5013, 1964, 7369, 1942, 4025, 22540, 2756, 6810, 4739, 1757, 2039, 3821, 12116, 2611, 2931, 5988, 14700, 1468, 1243, 4345, 3432, 8131, 6051, 3303, 2252, 3980, 1494, 17298, 2211, 1142, 2430, 27887, 2612, 6527, 6563, 820, 1465, 3496, 4236, 7971, 4385, 3087, 12706, 1766, 14546, 5092, 3327, 3567, 6169, 2692, 1018, 8121, 2963, 75905, 8769, 3733, 22530, 2836, 5207, 3104, 11926, 2530, 1219, 1924, 3406, 2285, 4574, 2111, 4921, 1752, 11925, 2953, 2210, 14280, 2933, 3071, 7810, 1653, 2822, 5007, 1241, 4468, 2075, 2250, 4664, 2435, 6165, 8037, 921, 4282, 10961, 2197, 982, 2783, 3864, 8840, 1809, 14281, 485, 3376, 3028, 4111, 8386, 8226, 3065, 4081, 5422, 2118, 3116, 1548, 2728, 16644, 2891, 2700, 2357, 6594, 1169, 4227)
no = c(1873, 2068, 4152, 1028, 5922, 1929, 2094, 21581, 3832, 6638, 5242, 1273, 1388, 2714, 12108, 1377, 1578, 5492, 19615, 586, 1455, 3238, 1473, 6294, 6348, 1435, 740, 1986, 700, 14462, 1742, 795, 1865, 76028, 2017, 4874, 13202, 669, 1337, 2242, 2954, 3609, 3101, 1106, 10696, 1340, 15397, 1832, 3119, 2326, 7236, 3067, 430, 6073, 629, 188875, 8198, 1837, 27256, 1781, 2183, 1926, 5359, 1321, 796, 799, 2222, 1246, 2612, 978, 3152, 1323, 10050, 1102, 1277, 15327, 1703, 2534, 4713, 706, 2663, 4331, 719, 3439, 1606, 982, 4010, 1268, 3647, 8253, 1354, 2622, 14938, 1528, 496, 1890, 2778, 5438, 1676, 6285, 437, 1189, 4100, 1792, 11521, 9035, 2424, 3489, 3095, 1141, 2058, 1457, 1871, 20581, 1865, 1298, 1603, 3074, 1022, 6742)

df <- data.frame(
  FIPS = fips,
  votes_dem_2016 = no,
  votes_gop_2016 = yes
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
  mutate(dem_pct = votes_dem_2016/(votes_dem_2016 + votes_gop_2016),
         rep_pct = votes_gop_2016/(votes_dem_2016 + votes_gop_2016),
         party = if_else(votes_dem_2016 > votes_gop_2016, "democrat", "republican"),
         
         # hue = party
         hue = hue(party),
         
         # saturation = party vote margin
         sat = saturation(votes_dem_2016, votes_gop_2016),
         
         # lightness = vote counts
         light = lightness(sum(votes_dem_2016, votes_gop_2016)),
         
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
