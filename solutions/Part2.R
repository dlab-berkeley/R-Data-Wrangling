## Solutions to DLab's R Data Wrangling - Part 2

## Challenge 1: Using tidyverse functions on tidy data ----
d |>
  group_by(stname) |>
  mutate(state_pop = sum(pop),
         pct_worker_type = pop/state_pop) |>
  filter(worker_type == "essential") |>
  arrange(desc(pct_worker_type))

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
              values_from = age) |>
  mutate(difference = nonessential-essential) |>
  arrange(desc(difference))

## Challenge 4: pivot and join ----

# make d_pop tidy by pivotting
d_pop_long <- d_pop %>%
  pivot_longer(cols = -variable,
               names_to = "stname", 
               values_to = "pop")

# check now has the same number of rows as d_rent
dim(d_pop_long)
dim(d_rent)

# join together - left_join, right_join, and full_join should all be the same here
d_new <- left_join(d_pop_long, d_rent, by = "stname")

# check the join worked as expected
head(d_new)
dim(d_new)

# calculate share of pop renting
d_rent_share <- d_new %>%
  mutate(rent_share = renters/pop) %>%
  arrange(desc(rent_share))

# look at the dataframe and sort by rent_share and avg_rent - do we see the same states at the top and bottom?
View(d_rent_share)

## Extra challenge: pivot_longer() with .value ----
d_tidy5 <- d_ex5 |>
  pivot_longer(cols = -worker_type,
               names_to = c(".value", "state"),
               names_sep = "_")

