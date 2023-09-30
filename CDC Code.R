library('tidyverse')


fires = read.csv('fires.csv')
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

fires_id_code |> 
  filter(ics_ct == 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()

fires_id_code |> 
  filter(ics_ct != 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()


fires_id_code |> 
  filter(mtbs_ct == 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()

fires_id_code |> 
  filter(mtbs_ct != 0) |> 
  ggplot(aes(x = FIRE_SIZE_CLASS)) + geom_bar()

## If there is an MTBS or ICS report, the fires tend to be larger (higher size class)

