# shared.R first!


dp_year_labels <- c(1795, 1800, 1820, 1825, 1850, 1855, 1860, 1865, 1870, 1875, 1880, 1890, 1895, 1900, 1905)


## PUBLIC DATA ####

# injuries data lacks a unique id, so make a rowid at the beginning
dp_injuries_xlsx <-
  read_excel(here::here("data/v20231130/dp_injury.xlsx"), guess_max = 100000) |>
  rowid_to_column()

dp_person_xlsx <-
  read_excel(here::here("data/v20231130/dp_person.xlsx"), guess_max = 100000) # public data


# recreate dp_descriptions data (exclude the full_descriptions which won't unnest correctly)
dp_person_desc <-
  dp_person_xlsx |>
  select(person_id, description_count, born, gender, description_ids, description_ages, description_years, description_datasets) |>
  mutate(across(c(description_ids, description_ages, description_years, description_datasets), ~str_remove_all(., "\\[|\\]"))) |>
  mutate(across(c(description_ids, description_ages, description_years, description_datasets), ~str_split(., ", *"))) |>
  unnest(c(description_ids, description_ages, description_years, description_datasets)) |>
  mutate(across(c(description_ages, description_ids, description_datasets), ~str_remove_all(., "'"))) |>
  rename_with(~str_remove(., "s$")) |>
  # fix duplicated description IDs. 
  distinct(person_id, description_count, born, gender, description_id, description_age, description_year, description_dataset) |>
  left_join(
    dp_injuries_xlsx |>
      filter(!is.na(injury)) |> # couple of NAs
      group_by(person_id, description_id) |>
      summarise(injuries = paste(injury, collapse = " "), .groups = "drop_last") |>
      ungroup(), by=c("person_id", "description_id")
  ) |>
  mutate(injury = if_else(!is.na(injuries), TRUE, FALSE))



# processing data ####


dp_injuries <-
  dp_injuries_xlsx|>
  select(person_id, description_id, injury, body_location, cause, full_description, description_dataset, description_year, gender, born, age) |>
  
  gender_simplify() |>
  # age group using groups in skeletons data for adults. 
  mutate(age=parse_number(age)) |>
  mutate(age_group = case_when(
    is.na(age) ~ "unknown",
    between(age, 0, 17) ~ "0-17",
    between(age, 18, 25) ~ "18-25",
    between(age, 26, 35) ~ "26-35",
    between(age, 36, 45) ~ "36-45",
    age>45 ~ "46+"
  ))  |>
  
  # injury_region
  # initial tidying up
  mutate(body_location=str_to_lower(body_location)) |>
  mutate(body_location=str_trim(str_replace_all(body_location, "\\s\\s+", " "))) |>
  
  mutate(injury_region = case_when(
    str_detect(body_location, arm_rgx) ~ "arm",
    str_detect(body_location, hand_rgx) ~ "hand",
    str_detect(body_location, foot_rgx) ~ "foot",
    str_detect(body_location, leg_rgx) ~ "leg",
    str_detect(body_location, head_rgx) ~ "head",
    str_detect(body_location, torso_rgx) ~ "torso",
    # put "side" and "body" in torso after more specific descriptions are accounted for
    # (some will be wrong...)
    body_location %in% c("side", "body") ~ "torso",
    
  )) |>
  # tweak injury_region to separate ribs from rest of torso because so many in OS
  mutate(injury_region_ribs = case_when(
    str_detect(body_location, "\\brib") ~ "ribs",
    .default = injury_region
  )) |>
  
  mutate(injury_side = case_when(
    str_detect(body_location, "^(both)\\b") ~ "both",
    str_detect(body_location, "\\b(left)\\b") ~ "left",
    str_detect(body_location, "\\b(right)\\b") ~ "right",
  )) |>
  
  # injury categories *after* region
  
  mutate(injury = str_to_lower(injury)) |>
  mutate(injury = str_trim(str_replace_all(injury, "  +", " "))) |>
  
  # plural might be meaningful...
  mutate(injury_plural = case_when(
    str_detect(injury, "\\b(marks)\\b|s$") ~ "y"
  )) |>
  # then slight std to make rgx easier. don't need to keep original.
  mutate(injury = str_remove(injury, "s$|^marks of *")) |>
  
  injury_classify() |>
  
  ## tweak for injury category "lost-tbd". needs injury region; prefer not to do that inside injury_classify
  mutate(injury_category = case_when(
    injury_region %in% c("foot", "hand", "leg", "arm") & injury_category=="lost-tbd" ~ "amputation",
    is.na(injury_region) & injury_category=="lost-tbd" ~ NA,
    injury_category=="lost-tbd" ~ "other",
    .default = injury_category
  )) |>
  
  
  mutate(dataset_rev = case_when(
    description_dataset %in% c("tlm", "pld") ~ "pl",
    .default = description_dataset
  )) |>
  
  # version of description year that NAs iffy bits and pieces and rhc/pl overlaps
  mutate(year_cln = case_when(
    description_year < 1790 ~ NA,
    description_dataset=="hcr" & description_year>1801 ~ NA,
    #description_year %in% c(1843, 1844, 1845, 1846, 1847, 1848, 1849, 1884, 1885, 1886, 1887, 1888) ~ NA,
    #description_year > 1901 ~ NA, # gone now
    dataset_rev =="pl" & description_year>1883 ~ NA, 
    dataset_rev=="pl" & description_year<1850 ~ NA,
    dataset_rev=="rhc" & description_year<1889 ~ NA,
    .default = description_year
  )) |>
  
  # filter out NA injury categories before doing anything.
  filter(!is.na(injury_category))


dp_desc <-
  dp_person_desc |>
  rename(age=description_age)|>
  #mutate(age=if_else(age=="None", NA, age)) |>
  mutate(across(c(age, description_year), ~if_else(.=="None", NA, .))) |>
  mutate(across(c(age, description_year), parse_number )) |>
  #mutate(age = parse_number(age) ) |>  #, description_year_orig = parse_number(description_year_orig))  |>
  mutate(age = case_when(
    age<10 ~ NA,
    age>95 ~ NA,
    .default = age
  ))  |>
  # age group using groups in skeletons data for adults. except undetermined for NA since they might be children.
  mutate(age_group = case_when(
    is.na(age) ~ "unknown",
    between(age, 0, 17) ~ "0-17",
    between(age, 18, 25) ~ "18-25",
    between(age, 26, 35) ~ "26-35",
    between(age, 36, 45) ~ "36-45",
    age>45 ~ "46+"
  ))  |>
  # fix gender "m" in pld if it's still there
  mutate(gender = case_when(
    gender=="m" & description_dataset=="pld" ~ "f",
    .default = gender
  )) |>
  # none of these datasets play nicely with decades but make it anyway....
  mutate(description_decade = description_year - (description_year %% 10) ) |>
  # version of description_dataset with male and female licences combined
  mutate(dataset_rev = case_when(
    description_dataset %in% c("tlm", "pld") ~ "pl",
    .default = description_dataset
  )) |>
  # version of description year that NAs iffy bits and pieces and rhc/pl overlaps
  mutate(year_cln = case_when(
    description_year < 1790 ~ NA,
    description_dataset=="hcr" & description_year>1801 ~ NA,
    #description_year %in% c(1843, 1844, 1845, 1846, 1847, 1848, 1849, 1884, 1885, 1886, 1887, 1888) ~ NA,
    #description_year > 1901 ~ NA, # gone now
    dataset_rev =="pl" & description_year>1883 ~ NA, 
    dataset_rev=="pl" & description_year<1850 ~ NA,
    dataset_rev=="rhc" & description_year<1889 ~ NA,
    .default = description_year
  )) |>
  # put new cols in "sensible" places
  relocate(dataset_rev, .after = description_dataset) |>
  relocate(description_year, year_cln, description_decade, .after = description_id) |>
  relocate(age_group, .after = age) 




dp_person <-
  dp_person_xlsx |> 
  mutate(across(c(latest_trial_year, earliest_trial_year, trials), parse_number))  





  


