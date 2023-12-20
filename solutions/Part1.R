## Solutions to DLab's R Data Wrangling - Part 1

## Challenge 1: Exploring the data ----

# read in data - watch out for relative file paths!
d <- readRDS("../data/part1_data.rds")

# explore the data using head(), glimpse(), View()
head(d)
glimpse(d)
View(d)

## Challenge 2: Using documentation to improve select()
# (1) select all variables to do with state
d |> select(starts_with("state"))

# (2) contains an underscore "_"
d |> select(contains("_"))

# (3) select all variables that are numbers 
## hint: the description at the top of ?select says how to do this
d |> select(where(is.numeric))

## Challenge 3: Combining conditional statements in filter()

# simplest answer 
d_ss <- d |>
  filter((sex == 1 & age >= 60 & state != "California") |
           (sex == 2 & age >= 55 & state != "California") |
           (age >= 50 & state == "California"))


# shorthand, but less intuitive
d_ss_v2 <- d |> 
  filter(state != "California" & (sex == 1 & age >= 60 | sex == 2 & age >= 55) |
           state == "California" & age >= 50)


# check both methods get the same dataframe
all.equal(d_ss, d_ss_v2)

# check if we have the right age range
summary(d_ss$age)

# check age range for california
d_ca <- d_ss |>
  filter(state == "California")
summary(d_ca$age)

# check age range for outside california
d_not_ca <- d_ss |>
  filter(state != "California")
summary(d_not_ca$age)

## Challenge 4: Using factor() within mutate() ----
d_ch4 <- d |>
  mutate(sex_factor = factor(sex, labels = c("man", "women")),
         empstat_factor = factor(employment_status, labels = c("NA", "employed", "unemployed", "not in LF")))
table(d_ch4$empstat_factor)
table(d_ch4$sex_factor)

## Challenge 5: Missing values ----
# return the frequency, the minimum income, maximum income, and mean income by racial group
d |>
  # first group by education (we could also use d_group which is already grouped)
  group_by(educ) |>
  summarize(freq = n(),
            min_inc = min(income, na.rm = T),
            max_inc = max(income, na.rm = T),
            avg_inc = mean(income, na.rm = T))

## Challenge 6: Putting it all together ----
d_state <- d |>
  # limit to working adults
  filter(employment_status == 1 & age >= 18) |>
  # group by state
  group_by(state) |>
  # calculate summary statistics
  summarize(avg_income = mean(income, na.rm = T),
            college = mean(educ == "BA" | educ == "graduate"),
            essential_worker = mean(worker_type == 3)) |>
  # sort by share with BA or graduate degree
  arrange(college)

head(d_state)
