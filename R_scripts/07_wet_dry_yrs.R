
# dependencies ------------------------------------------------------------

# combines output of 05_precipitation reduction calculations.R
# for multiple lags (1-4 years prior to biomass harvests)
source("R_scripts/06_combine_lagged_ppt.R"); ppt_comb1 # dataframe of interest
theme_set(theme_classic())



# read in other data ------------------------------------------------------

# climate data of annual metrics
ann1 <- read_csv(file.path(path, "IDE/data_processed/climate",
                           "climate_mean_annual_by_site.csv"),
                 show_col_types = FALSE)

# table providing the n_treat_years associated with each calendar year/sitecode
p1 <- newest_file_path(
  path = file.path(path, "IDE/data_processed"),
  file_regex = "IDE_treatment_years_\\d{1,2}-\\d{1,2}-\\d{4}.csv",
  mdy = TRUE)

trt_yr <- read_csv(p1, show_col_types = FALSE)

# prepare MAP -------------------------------------------------------------

map_mswep1 <- ann1 %>% 
  filter(data_source == "mswep") %>% 
  select(site_code, MAP)

map_sub1 <- map_mswep1 <- ann1 %>% 
  filter(data_source == "submitted") %>% 
  select(site_code, MAP)


# calculate drt sev by method-------------------------------------------------

map_sub2 <- map_sub1 %>% 
  mutate(method = 'original')

map_mswep2 <- map_mswep1 %>% 
  mutate(method = "mswep")

ppt_orig <- ppt_comb1 %>% 
  filter(was_source_used) %>% 
  mutate(method = 'original') %>% 
  select(-was_source_used)

ppt_mswep <- ppt_comb1 %>% 
  filter(data_source == "mswep") %>% 
  mutate(method = 'mswep') %>% 
  select(-was_source_used)

ppt_comb2 <- bind_rows(ppt_orig, ppt_mswep) %>% 
  left_join(bind_rows(map_sub2, map_mswep2), by = c("site_code", "method")) %>% 
  mutate(drt_sev = (ppt - MAP)/MAP,
         # drought severity applies to the drought treatments
         drt_sev = ifelse(trt == "Drought", drt_sev, NA_real_)) %>% 
  left_join(trt_yr, by = c("site_code", "year"))


# compare drought severities ----------------------------------------------

drt_sev1 <- ppt_comb2 %>% 
  filter(trt == 'Drought') %>% 
  select(site_code, year, drt_sev, n_treat_years, method, lag) %>% 
  pivot_wider(names_from = 'method',
              values_from = 'drt_sev')
  
df <- drt_sev1 %>% 
  filter(lag == "365-0days", !is.na(n_treat_years), n_treat_years > 0)

range_sev <- range(c(df$original, df$mswep), na.rm = TRUE)

ggplot(df, aes(original, mswep)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = range_sev, ylim = range_sev) +
  labs(subtitle = "Drought severity in all sites",
       x = "Drought severity calculated using 'best' annual ppt and submitted MAP",
       y = "Drought severity calculated from only mswep data source",
       caption = "Only showing years with > 0 n_treat_years")


# wet/dry years -----------------------------------------------------------
# 
lag_ctrl1 <- ppt_comb2 %>% 
  filter(trt == 'Control', lag == "365-0days",
         n_treat_years > 0) %>% 
  mutate(wet_year = ppt > MAP,
         perc_diff = (ppt - MAP)/MAP*100, # percent different from normal
         diff = ppt - MAP) # mm different from normal
  
is_wet_yr <- lag_ctrl1 %>% 
  select(site_code, year, method, wet_year, n_treat_years) %>% 
  pivot_wider(id_cols = c("site_code", "year", "n_treat_years"), 
              values_from = "wet_year",
              names_from = 'method')
  
# percent of years when methods (original vs mswep) agree on 
# a given year being wet
mean(is_wet_yr$original == is_wet_yr$mswep, na.rm = TRUE)*100


# * consecutive wet/dry years ---------------------------------------------

# step 2--subset to find sites with at least 3 consecutive wet years
# step3--find sites with at least 3 consecutive wet years

# step 1--determine which sites have at least 3 consecutive years of
# treatment
lag_ctrl1 %>% 
  filter(n_treat_years >= 1) %>% 
  arrange(method, site_code, n_treat_years) %>% 
  group_by(site_code, method) %>% 
  summarise(n = n(),
            n_consec = max_consecutive_length(n_treat_years)) %>% 
  filter(n != n_consec)


lag_ctrl1 %>% 
  filter(n_treat_years >= 1,
         site_code == "thompson.us")

# explore -----------------------------------------------------------------

# years/sites where tim found that
# in a particular year they have a super severe drought and a positive ANPP effect. 
drt_sev1 %>% 
  filter(year == 2018, site_code %in% c("charleville.au", "scruzl.us"),
         lag == "365-0days", lag == "365-0days")

