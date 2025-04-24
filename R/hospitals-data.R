# need shared.R first


# short codes for hospitals
# 	  ghs -	Guy's Hospital 1810-39 (unbroken?)
# 	  mxh -	Middlesex Hospital 1760-78 (break in the middle)
# 	  rlh -	Royal London Hospital 1760-1805 (4 years)
# 	  sth -	St Thomas Hospital 1773-1809 (kind of bitty)



## PUBLIC DATA ####

# checked admission year vs description year
# 5 digit years in description year can be fixed by removing the first digit.
# 3 digit years can't be fixed. anything < 1760 -> NA.

# no unique injury id, add a rowid at the start
hp_injury_xlsx <-
  read_excel(here::here("data/v20231130/hp_injury.xlsx") , guess_max=18000) |>
  # basic year fixes
  mutate(description_year = case_when(
    description_year>2000 ~ parse_number(str_sub(as.character(description_year), 2, 5)),
    description_year<1760 ~ NA, 
    description_year==1874 & description_dataset=="sth" ~ 1784, # a typo
    .default=description_year
  )) |>
  mutate(date_of_admission = case_when(
    date_of_admission=="26/02/1874" & description_dataset=="sth" ~ "26/02/1784",
    .default = date_of_admission
  ))  |>
  rowid_to_column()




## file was originally hp_desc, renamed to hp_person but is still the description. cba to rename everything.

hp_desc_xlsx <-
  read_excel(here::here("data/v20231130/hp_person.xlsx"), guess_max=18000) |>
  # basic year fixes
  mutate(description_year = case_when(
    description_year>2000 ~ parse_number(str_sub(as.character(description_year), 2, 5)),
    description_year==1874 & description_dataset=="sth" ~ 1784, # typo
    description_year<1760 ~ NA, 
    .default=description_year
  ))  |>
  mutate(date_of_admission = case_when(
    date_of_admission=="26/02/1874" & description_dataset=="sth" ~ "26/02/1784",
    .default = date_of_admission
  )) 





## simplified version of injuries for public data. 
## any work on specific locations/categories will need extra processing

hp_injuries <-
  hp_injury_xlsx |>
  select(rowid, person_id, description_id, gender, born, age, description_dataset, description_year, injury, body_location, full_description, date_of_admission, discharge_date, cause, reason, result)  |>
  
  # map body_location to regions
  # (regions_locations is in shared.R)
  left_join(regions_locations |> rename(injury_region=region), by=c("body_location"="injury_location")) |>
  mutate(injury_region=if_else(is.na(injury_region), "unknown", injury_region)) |>
  
  # tweak for ribs comparisons.
  mutate(injury_region_ribs = case_when(
    str_detect(body_location, "\\brib") ~ "ribs",
    .default = injury_region
  )) |>
  
  #injury categories - use new injury_classify
  injury_classify() |>

  # gender
  gender_simplify() |>

  # age group using same groups as skeletons data but with extra 0-17 for children; any NAs=unknown
  mutate(age_group = case_when(
    is.na(age) ~ "unknown",
    between(age, 0, 17) ~ "0-17",
    between(age, 18, 25) ~ "18-25",
    between(age, 26, 35) ~ "26-35",
    between(age, 36, 45) ~ "36-45",
    age>45 ~ "46+"
  ))  |>

  # a few dates that lubridate can handle but could do with fixing eg 08/031814
  mutate(date_admission_std = parse_date_time(date_of_admission, c("dmy", "my"))) |>
  mutate (year_admission = year(date_admission_std)) |>
  
  ## cleanup
  mutate(date_admission_std = case_when(
    year_admission < 1760 ~ NA,
    .default = date_admission_std
  )) |>
  mutate(year_admission = case_when(
    year_admission < 1760 ~ NA,
    .default = year_admission
  )) |>
  mutate(decade_admission = year_admission - (year_admission %% 10)) |>
  
  # a few fixes for discharge date
  mutate(discharge_date = case_match(
    discharge_date,
    "04/44/1778" ~ "04/11/1778", 
    "29/02/1791" ~ "28/02/1791",
    "29/02/1793" ~ "28/02/1793",
    "Made outpatient" ~ NA,
    "1791" ~ NA, # a couple of these in the final data
    .default = discharge_date
  ))  |>
  
  mutate(date_discharge_std = parse_date_time(discharge_date, c("dmy", "my"))) |>
  mutate(year_discharge = year(date_discharge_std)) |>
  # cleanup
  mutate(date_discharge_std = case_when(
    year_discharge < 1760 ~ NA,
    .default = date_discharge_std
  )) |>
  mutate(year_discharge = case_when(
    year_discharge < 1760 ~ NA,
    .default = year_discharge
  )) |>

  # add length of stay
  # subtracting one date from the other -> seconds -> seconds_to_period converts to days "S4:Period".
  mutate(stay = seconds_to_period(date_discharge_std-date_admission_std)) |>
  # convert Period to number of days
  mutate(stay = as.numeric(stay, "days")  ) |>
  mutate(stay = if_else(stay<0, NA, stay)) |>
  
  # simplify result
  #i thought this was only rlh but there are 1000 outcomes in mxh as well ?????

  mutate(result = str_to_lower(result)) |>
  mutate(result = case_when(
    is.na(result) ~ NA,
    str_detect(result, "\\b(op)\\b") ~ "outpatient",
    result %in% c("cured", "died", "discharged", "relieved", "left", "removed", "returned", "elsewhere", "returned", "received", "unknown") ~ result,
    str_detect(result, "\\b(died|d'd|dead)\\b") ~ "died",
    str_detect(result, "\\b(cured)") ~ "cured",
    str_detect(result, "discharg") ~ "discharged", 
    str_detect(result, "ran away|run away|went away|went out|without leave|left the h|would not stay|refractory|went on tuesday last") ~ "left",
    str_detect(result, "taken away|taken out|taken to prison") ~ "removed",
    str_detect(result, "out.?patient") ~ "outpatient",
    str_detect(result, "gone on board|to smallpox hospital") ~ "elsewhere", # (to another institution of some sort)
    str_detect(result, "no result listed|very abusive to nurse douglas|irregular|a child|an inft|\\[o\\]") ~ "unknown",
    #.default = result
  )) |>
  ##now *really* simplify it
  mutate(result_s = case_match(
    result,
    c("cured", "relieved") ~ "cured",
    c("left", "removed", "discharged", "returned", "elsewhere", "returned", "received", "outpatient") ~ "other", # added discharged
    NA ~ "unknown",
    .default = result
  )) |>
  
  mutate(rlh = case_when(
    description_year %in% c(1791, 1792) ~ "rlh1791-2",
    description_year==1805 ~ "rlh1805",
    description_year==1760 ~ "rlh1760"
  )) |>
  
  filter(!is.na(injury_category))





# basic processing: drop unwanted columns, fix dates
hp_desc <-
  hp_desc_xlsx |>
  mutate(translation = str_to_lower(translation))  |>
  
  # a few dates that lubridate can handle but could do with fixing eg 08/031814
  mutate(date_admission_std = parse_date_time(date_of_admission, c("dmy", "my"))) |>
  mutate (year_admission = year(date_admission_std)) |>
  
  ## cleanup
  mutate(date_admission_std = case_when(
    year_admission < 1760 ~ NA,
    .default = date_admission_std
  )) |>
  mutate(year_admission = case_when(
    year_admission < 1760 ~ NA,
    .default = year_admission
  )) |>
  mutate(decade_admission = year_admission - (year_admission %% 10)) |>
  
  # fixes still needed
  mutate(discharge_date = case_match(
    discharge_date,
    "04/44/1778" ~ "04/11/1778", 
    "29/02/1791" ~ "28/02/1791",
    "29/02/1793" ~ "28/02/1793",
    "Made outpatient" ~ NA,
    "1791" ~ NA, # a couple of these in the final data, don't really see the point of keeping.
    .default = discharge_date
  ))  |>

  mutate(date_discharge_std = parse_date_time(discharge_date, c("dmy", "my"))) |>
  mutate(year_discharge = year(date_discharge_std)) |>
  # cleanup
  mutate(date_discharge_std = case_when(
    year_discharge < 1760 ~ NA,
    .default = date_discharge_std
  )) |>
  mutate(year_discharge = case_when(
    year_discharge < 1760 ~ NA,
    .default = year_discharge
  )) |>

  # add stay
  # subtracting one date from the other -> seconds -> seconds_to_period converts to days "S4:Period".
  mutate(stay = seconds_to_period(date_discharge_std-date_admission_std)) |>
  # convert Period to number of days
  mutate(stay = as.numeric(stay, "days")  ) |>
  mutate(stay = if_else(stay<0, NA, stay)) |>

  # simplify result
  #mutate(result_orig = result) |>
  mutate(result = str_to_lower(result)) |>
  #there are 1000 outcomes in mxh as well ?????
  mutate(result = case_when(
    is.na(result) ~ NA,
    str_detect(result, "\\b(op)\\b") ~ "outpatient",
    result %in% c("cured", "died", "discharged", "relieved", "left", "removed", "returned", "elsewhere", "returned", "received", "unknown") ~ result,
    str_detect(result, "\\b(died|d'd|dead)\\b") ~ "died",
    str_detect(result, "\\b(cured)") ~ "cured",
    str_detect(result, "discharg") ~ "discharged", 
    str_detect(result, "ran away|run away|went away|went out|without leave|left the h|would not stay|refractory|went on tuesday last") ~ "left",
    str_detect(result, "taken away|taken out|taken to prison") ~ "removed",
    str_detect(result, "out.?patient") ~ "outpatient",
    str_detect(result, "gone on board|to smallpox hospital") ~ "elsewhere", # (to another institution of some sort)
    str_detect(result, "no result listed|very abusive to nurse douglas|irregular|a child|an inft|\\[o\\]") ~ "unknown",
    #.default = result
  )) |>
  ##now *really* simplify it
  mutate(result_s = case_match(
    result,
    c("cured", "relieved") ~ "cured",
    c("left", "removed", "discharged", "returned", "elsewhere", "returned", "received", "outpatient") ~ "other", # added discharged
    NA ~ "unknown",
    .default = result
  )) |>

  # age group using groups in skeletons data for adults. except undetermined for NA since they might be children.
  mutate(age_group = case_when(
    is.na(age) ~ "unknown",
    between(age, 0, 17) ~ "0-17",
    between(age, 18, 25) ~ "18-25",
    between(age, 26, 35) ~ "26-35",
    between(age, 36, 45) ~ "36-45",
    age>45 ~ "46+"
  )) |>
  
  # gender is m/f/u, std it.
  gender_simplify() |>
  
  mutate(rlh = case_when(
    description_year %in% c(1791, 1792) ~ "rlh1791-2",
    description_year==1805 ~ "rlh1805",
    description_year==1760 ~ "rlh1760"
  )) |>
  
  # drop a few descriptions that don't have a non-NA injury category
  
  semi_join(hp_injuries, by="description_id") |>
  
  select(person_id, description_id, description_dataset, description_year, injury, injury_digest, full_description, translation, 
         date_admission_std, date_discharge_std, stay, year_admission, year_discharge, decade_admission,
         date_of_admission, discharge_date, result, result_s, 
         name, given, surname, gender, born, age, age_group, occupation, abode, notes, rlh)
