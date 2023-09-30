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

