## Solutions to DLab's R Data Wrangling - Part 2

## Challenge 1: Using tidyverse functions on tidy data ----
d |>
  group_by(stname) |>
  mutate(state_pop = sum(pop),
         pct_worker_type = pop/state_pop) |>
  filter(worker_type == "essential") |>
  arrange(pct_worker_type)

## Challenge 2: pivot_longer() ----

# 1. Why is the dataset not tidy? Is there one observation per row? Is there one variable per column?
# there are values of variables in the column names: "essential" and "nonessential" are values of
# "worker_type" and should be in their own column

# 2. What would the tidy version of this dataset look like? What would the rows be? What would the columns be?
# one column for each state [51] X worker_type [3] = 153 rows total
# columns for each variable: stname, worker_type, pop

# 3. What function(s) will wrangle the data into tidy form?
# pivot_longer()

# fill in your code for pivoting d_ex2 into tidy form 
d_tidy2 <- d_ex2 |>
  pivot_longer(cols = -stname,
               names_to = "worker_type",
               values_to = "pop")

## Challenge 3: pivot_wider() ----
d |>
  pivot_wider(id_cols = stname,
              names_from = worker_type,
              values_from = college) |>
  mutate(difference = nonessential-essential) |>
  arrange(desc(difference))

### Additional challenge ----
d |>
  pivot_wider(id_cols = stname,
              names_from = worker_type,
              values_from = c(college, income)) |>
  mutate(coll_diff = college_nonessential-college_essential,
         coll_inc = income_nonessential-income_essential)

## Challenge 4: join and pivot ----

# OPTION #1 - pivot first 
d_educ_wide <- d_educ %>%
  pivot_wider(id_cols = stname,
              values_from = educ_pop,
              names_from = degree)
dim(d_educ_wide)

# perform left join - drops DC from d_educ_wide b/c we don't have state population on this
d_educ_pop <- left_join(x = d_pop, y = d_educ_wide, by = 'stname') %>%
  # estimate share with college and graduate degrees
  mutate(share_college = bachelors/pop,
         share_grad = graduate/pop,
         # calculate difference in shares
         difference = share_college-share_grad) %>%
  # sort by smallest difference first 
  arrange(difference)

head(d_educ_pop)

# OPTION #2 - join first 
d_join <- left_join(x = d_pop, y = d_educ, by = "stname")
dim(d_join)

# notice that now pop is repeated for two rows b/c we joined 1 row in d_pop to 2 rows in d_educ for each state
head(d_join)

# pivot wider 
d_join_wide <- d_join %>%
  pivot_wider(id_cols = c(stname, pop),
              values_from = educ_pop,
              names_from = degree) %>%
  # calculate share and difference
  mutate(share_college = bachelors/pop,
         share_grad = graduate/pop,
         # calculate difference in shares
         difference = share_college-share_grad) %>%
  # sort by smallest difference first 
  arrange(difference)

head(d_join_wide)

## Extra challenge: pivot_longer() with .value ----
d_tidy5 <- d_ex5 |>
  pivot_longer(cols = -worker_type,
               names_to = c(".value", "state"),
               names_sep = "_")

