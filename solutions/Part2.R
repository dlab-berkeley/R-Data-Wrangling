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

## Challenge 3: pivot_longer() with .value ----
d_tidy5 <- d_ex5 |>
  pivot_longer(cols = -worker_type,
               names_to = c(".value", "state"),
               names_sep = "_")

## Challenge 4: pivot_wider() ----
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


