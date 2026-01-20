
install.packages('SPEI')
library(SPEI)
library(dplyr)
library(zoo)

# paths
path_w <- "C:/Users/canzolbr/Documents/GitHub/Forecasting-World-Coffee-Prices/Data/Weather/"

# load panel data

# load data set
df_br = read.csv(paste0(path_w,"br_weather.csv"))
df_co = read.csv(paste0(path_w,"co_weather.csv"))

df_br['country'] = 'Brazil'
df_co['country'] = 'Colombia'

# load states centroids
df_br_c <- read.csv(paste0(path_w,"google-earth/Brazil/brazil_states_centroids.csv"))
df_co_c <- read.csv(paste0(path_w,"google-earth/Colombia/colombia_states_centroids.csv"))

# keep relevant information
df_br_c = df_br_c %>%  select(c('lat','lon','state_code','state_name')) 
df_co_c = df_co_c %>%  select(c('lat','lon','state_code','state_name')) 

# merge states centroids
df_br = left_join(df_br,df_br_c,c('state_code','state_name'))
df_co = left_join(df_co,df_co_c,c('state_code','state_name'))

df <- bind_rows(df_br,df_co)

# filter name unknown and buenaventura
df<-
  df %>% 
   filter(!state_name %in% c("Name Unknown","Buenaventura"))

df <- df %>%
  mutate(
    date = as.yearmon(paste(year, month), "%Y %m")
  ) %>%
  arrange(state_code, date)

compute_spei <- function(p, t, lat, k) {
  pet <- thornthwaite(Tave = t, lat = lat)
  wb  <- p - pet
  as.numeric(fitted(SPEI::spei(wb, scale = k)))
}

df <- df %>%
  group_by(state_code) %>%
  mutate(
    spei_3  = compute_spei(precip_mm, t2m_c, lat[1], 3),
    spei_6  = compute_spei(precip_mm, t2m_c, lat[1], 6),
    spei_12 = compute_spei(precip_mm, t2m_c, lat[1], 12)
  ) %>%
  ungroup()

# save SPEI computation
save(df,file = paste0(path_w,"spei.Rdata"))
