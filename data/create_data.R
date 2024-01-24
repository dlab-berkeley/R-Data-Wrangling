# Program for creating datasets for use in R-Data-Wrangling Pilot
# Created: HA 10/27
# Updated: HA 10/30
#          HA 12/20/23 - comment out data not used

# Set up ----
rm(list = ls())
library(pacman)
p_load(tidyverse, data.table, here)
setwd(here())

# Raw data ----
d_raw <- fread("./data/usa_00087.csv")
  # there are 3 million observations - probably want to limit this

head(d_raw)
colnames(d_raw)

# Clean micro-data ----

# column names
d <- d_raw
colnames(d) <- tolower(colnames(d))

# # take a random sample of 10000 households 
# serial <- sample(d_raw$SERIAL, size = 10000) %>% unique()
# d <- d_raw %>%
#   filter(SERIAL %in% serial)

# # keep only needed variables
# d <- d %>%
#   select(year, statefip, serial, pernum, race, hispan, educ, inctot, age, sex, empstat)
# head(d)

# make state a string variable
fips <- fread("./data/us-state-ansi-fips.csv") %>%
  rename(statefip = st)
d <- left_join(d, fips, by = 'statefip')

# make race a factor
d[ , race := fcase(race == 1, 1,
                   race == 2, 2,
                   race == 3, 3,
                   race >= 4 & race <= 6, 4,
                   race >= 7, 5)]
d[ , race := factor(race, labels = c('white', 'black', 'native', 'asian', 'other'))]
summary(d$race)

# make education a factor
d[ , education := fcase(educ < 6, 1, 
                        educ == 6, 2,
                        educ < 10, 3,
                        educ == 10, 4,
                        educ == 11, 5)]
d[ , education := factor(education, labels = c("less than HS", "HS", "some college", "BA", "graduate"), ordered = T)]
table(d$educ, d$education, useNA = "always")

# create college
d[ , college := ifelse(educ>=10, 1,0)]

# clean income - change to NA 
summary(d$inctot)
d[ , inctot := ifelse(inctot == 9999999, NA, inctot)]
d[ , ftotinc := ifelse(ftotinc == 9999999, NA, ftotinc)]
summary(d$inctot)

# create essential worker category - don't factor (will do this in Part 1)
d[ , worker_type := fcase(empstat == 3, 1,
                          occ2010 >= 3000 & occ2010 <= 9750, 2,
                          default = 3)]
#d[ , worker_type := factor(worker_type, labels = c("nonLF", "essential", "nonessential"), ordered = T)]
table(d$empstat, d$worker_type)

# # make state codes randomly missing but leave state name
# d_slice <- slice_sample(select(d, serial, pernum, stusps), prop = 0.9)
# d_slice[ , state_abb := stusps]
# d <- left_join(d, d_slice, by = c('serial', 'pernum', 'stusps')) %>%
#   mutate(stusps = ifelse(stusps == "", NA, stusps))

# leave educ, gender and hispanic numeric - will later convert to factors 

# Part 1 Data ----

# reorder and drop unneeded columns 
d_part1 <- d %>%
  select(year, serial, pernum, race, hispan, education, sex, empstat, worker_type, 
         age, inctot, stname, stusps)

# rename to be more intuitive 
colnames(d_part1) <- c('year', 'family_ID', 'person_num', 'race', 'hispanic', 'educ', 'sex', 
                 'employment_status', 'worker_type', 'age', 'income', 'state', 'state_abb')

head(d_part1)

# take a sample 
d_sample <- slice_sample(d_part1, n = 100000)

# save
saveRDS(d_sample, "./data/part1_data.rds")


# Part 2 Data ----

# limit to 18+
d <- d[age >= 18]

# look at state populations -> looks good
d[ , .(pop = sum(perwt)), by = stname]

# make essential worker factor
d[ , worker_type := factor(worker_type, labels = c("nonLF", "essential", "nonessential"), ordered = T)]
table(d$empstat, d$worker_type)

# clean sex
d[ , sex := factor(sex, labels = c("men", "women"))]

# create college
d[ , college := ifelse(educ>=10, 1,0)]

# create state x occupation level data
d_state <- d[ , .(pop = sum(perwt),
       income = weighted.mean(inctot, w = perwt, na.rm = T),
       #avg_age = weighted.mean(age, w = perwt),
       college = weighted.mean(college, w = perwt)),
   by = c("stname", "statefip", "worker_type")] %>%
  arrange(stname, worker_type)

d_state

saveRDS(d_state, "./data/part2_data.rds")

# Q: why is this data helpful?
# A: plotting!

## Messy ----

### (1) lengthen ----
# problem: data in column names 
dt <- d_state %>%
  #summarize(pop = sum(pop), .by = c('stname', 'worker_type')) %>%
  pivot_wider(id_cols = worker_type,
              values_from = pop,
              names_from = stname)

saveRDS(dt, "./data/part2_data_wide_1.rds")

### (2) lengthen ----
# problem: data in column names - same as #1 

dt <- d_state %>%
  #summarize(pop = sum(pop), .by = c('stname', 'worker_type')) %>%
  pivot_wider(id_cols = stname,
              values_from = pop,
              names_from = worker_type,
              names_prefix = "pop_")

saveRDS(dt, "./data/part2_data_wide_2.rds")

### (3) lengthen ----
# problem: multiple types of data in column names 
dt <- d[ , .(pop = sum(perwt)),
     by = c("stname", "statefip", "worker_type", "college")] %>%
  mutate(college = ifelse(college ==0, "noncollege", "college")) |>
  pivot_wider(id_cols = stname,
              values_from = pop,
              names_from = c(worker_type, college))

saveRDS(dt, "./data/part2_data_wide_3.rds")

### (4) lengthen ----
# problem: data and variable names in column headers
dt <- d_state %>%
  pivot_wider(id_cols = stname, 
              values_from = c(pop, income),
              names_from = worker_type)

saveRDS(dt, "./data/part2_data_wide_4.rds")

### (5) challenge ----
# for challenge
dt <- d_state %>%
  pivot_wider(id_cols = worker_type, 
              values_from = c(pop, college),
              names_from = c(stname))

saveRDS(dt, "./data/part2_data_wide_5.rds")

## Joining ----

# two state level with different pop counts 
table(d$worker_type)
d_state <- d %>%
  group_by(stname) %>%
  summarize(pop = sum(perwt),
            essential_pop = sum(perwt*(worker_type=="essential")))

# save first one minus DC
d_state %>%
  select(stname, pop) %>%
  filter(stname != "District of Columbia") %>%
  saveRDS("./data/part2_data_join_1.rds")

# save second one minus Hawaii and Alaska
d_state %>%
  select(stname, essential_pop) %>%
  filter(stname != "Hawaii" & stname != "Alaska") %>%
  saveRDS("./data/part2_data_join_2.rds")

# for the final challenge - data at state x educ level on pop
d_educ <- d %>%
  filter(education == "BA" | education == "graduate") %>%
  group_by(stname, education) %>%
  summarize(educ_pop = sum(perwt)) %>%
  rename(degree = education) %>%
  mutate(degree = ifelse(degree == "BA", "bachelors", "graduate"))

saveRDS(d_educ, "./data/part2_data_join_3.rds")
  

