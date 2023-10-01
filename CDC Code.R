library('tidyverse')
library(lubridate)

library(scales)


state_region = read.csv("/Users/kevinsu/Downloads/us census bureau regions and divisions.csv")

fires = read.csv('fires.csv') |> 
  #mutate(FIRE_YEAR = lubridate::year(lubridate::mdy(FIRE_YEAR))) |> 
  inner_join(state_region, by = c("STATE" = "State.Code"))
head(fires)



fires_cln = fires[fires$STAT_CAUSE_DESCR != c("Miscellaneous", "Missing/Undefined"),]
practice = ggplot(fires, aes(x = FIRE_SIZE, y = FIRE_YEAR, color = STAT_CAUSE_DESCR)) + geom_point()
practice

practice2 = ggplot(fires, aes(x = FIRE_SIZE, y = FIRE_SIZE, color = STAT_CAUSE_DESCR)) + geom_point()
practice2


practice3 = ggplot(fires_cln, aes(x = FIRE_SIZE, y = FIRE_YEAR, color = STAT_CAUSE_DESCR)) + geom_point()
practice3

# ID checking
# `LOCAL_FIRE_REPORT_ID` N=8234
# `LOCAL_INCIDENT_ID` N=243426
# `FIRE_CODE` N=110962
# `ICS_209_INCIDENT_NUMBER` N=14561
# `MTBS_ID` N=5474

fires_id_code = fires |> 
  mutate(ics_yn = ifelse(ICS_209_INCIDENT_NUMBER != "", T, F),
         mtbs_yn = ifelse(MTBS_ID != "", T, F)) |> 
  group_by(ICS_209_INCIDENT_NUMBER) |> 
  mutate(ics_ct = sum(ics_yn),
         ics_fire_sum = sum(FIRE_SIZE)) |> 
  ungroup() |> 
  group_by(MTBS_ID) |> 
  mutate(mtbs_ct = sum(mtbs_yn),
         mtbs_fire_sum = sum(FIRE_SIZE)) |> 
  ungroup()
  # select(ics_ct) |> 
  # arrange(desc(ics_ct)) |> head(10)

fires_id_code |> 
  filter(ics_ct != 0) |> 
  ggplot(aes(x = ics_ct, y = ics_fire_sum)) + geom_point()

fires_id_code |> 
  filter(mtbs_ct != 0) |> 
  ggplot(aes(x = mtbs_ct, y = mtbs_fire_sum)) + geom_point()

# ICS report
fires_id_code |> 
  filter(ics_ct == 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()

fires_id_code |> 
  filter(ics_ct != 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()

# MTBS report
fires_id_code |> 
  filter(mtbs_ct == 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()

fires_id_code |> 
  filter(mtbs_ct != 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()

## If there is an MTBS or ICS report, the fires tend to be larger (higher size class)

# Exploratory Analysis
fires |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar() + facet_wrap(~STAT_CAUSE_DESCR) + coord_flip()

fires |> 
  ggplot(aes(x = OWNER_DESCR)) + geom_bar() + facet_wrap(~FIRE_SIZE_CLASS) + coord_flip()

fires |> 
  group_by(STAT_CAUSE_DESCR) |> 
  summarize(max_fire = max(FIRE_SIZE)) |> 
  arrange(desc(max_fire))

fires |> 
  group_by(FIRE_YEAR, Region) |> 
  mutate(max_fire_yr = max(FIRE_SIZE)) |> 
  ggplot(aes(x=FIRE_YEAR, y = max_fire_yr, color =Region)) + geom_line() + theme_minimal() + 
  scale_x_continuous(limits = c(2000,2010), n.breaks = 9, minor_breaks = NULL)

fires |> 
  ggplot(aes(y = CONT_TIME, x = FIRE_SIZE, color = Region)) + geom_point()


fires |> 
  group_by(STAT_CAUSE_DESCR, Region) |> 
  summarize(median = median(CONT_TIME,na.rm=T)) |> 
  ggplot(aes(y = STAT_CAUSE_DESCR, x = Region, fill = median)) + geom_tile() + 
  geom_text(aes(label = median), color = "white", size = 4) + theme_classic()

fires |> 
  group_by(STAT_CAUSE_DESCR, Region) |> 
  summarize(diff = max(CONT_TIME,na.rm=T) - min(CONT_TIME,na.rm=T)) |> 
  ggplot(aes(y = STAT_CAUSE_DESCR, x = Region, fill = diff)) + geom_tile() + 
  geom_text(aes(label = diff), color = "white", size = 4) + theme_classic()


fires |> 
  group_by(STAT_CAUSE_DESCR) |> 
  arrange(desc(FIRE_SIZE)) |> 
  slice_head(n=1) |> 
  select(OBJECTID, STAT_CAUSE_DESCR, FIRE_YEAR,FIRE_NAME, FIRE_SIZE) |> 
  arrange(desc(FIRE_SIZE))


# explore missing cause
fires |> 
  filter(STAT_CAUSE_DESCR == "Missing/Undefined") |> 
  ggplot(aes(FIRE_SIZE_CLASS)) + geom_bar() + scale_y_continuous(labels = label_comma())

fires |> 
  filter(STAT_CAUSE_DESCR != "Missing/Undefined") |> 
  ggplot(aes(FIRE_SIZE_CLASS)) + geom_bar() + scale_y_continuous(labels = label_comma())

## distributions of fire size class between missing and non-missing causes seems similar

