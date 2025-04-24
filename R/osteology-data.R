## run shared.R first

## os_ is the public data

# lacks a unique injury_id, so make a rowid at the beginning
# injury counts were omitted from public data; not sure now if that was intentional or an oversight
os_injury_xlsx <-
  read_excel(here::here("data/v20231130/os_injury.xlsx") ) |>
  rowid_to_column()


os_person_xlsx <-
  read_excel(here::here("data/v20231130/os_person.xlsx") )


os_injury <-
os_injury_xlsx |>
  # slightly simplified april 2025
  separate(injury, into=c("injury", "i2"), sep=" *\\| *", fill="right", extra = "merge")  |>
  # tidy up
  mutate(injury_category = str_remove(injury, "\\..+$")) |> 
  mutate(injury_category = str_trim(str_to_lower(injury_category))) |>
  
  select(-i2) |>
  mutate(injury_category = case_when(
    str_detect(injury_category, "subluxation") ~ "dislocation",
    str_detect(injury_category, "fracture|avulsion") ~ "fracture",
    injury_category=="soft tissue trauma" ~ "muscle",
    str_detect(injury_category, "projectile") ~ "wound", # there will only be one.
    str_detect(injury_category, "trauma") ~ word(injury_category),
    .default = injury_category
  )) |>

  # simpler process for region; would need extra processing for more detail.
  separate(body_location, into = c("location1", "location2"), sep=" *\\| *", extra = "drop", fill="right", remove = FALSE) |>
  mutate(injury_region = case_when(
    str_detect(location1, arm_rgx) ~ "arm",
    str_detect(location1, hand_rgx) ~ "hand",
    str_detect(location1, foot_rgx) ~ "foot",
    str_detect(location1, leg_rgx) ~ "leg",
    str_detect(location1, head_rgx) ~ "head",
    str_detect(location1, torso_rgx) ~ "torso",
    # a few don't match on location1 column but do match on location2.
    str_detect(location2, arm_rgx) ~ "arm",
    str_detect(location2, head_rgx) ~ "head",
    str_detect(location2, leg_rgx) ~ "leg",
    str_detect(location2, torso_rgx) ~ "torso",
    str_detect(location2, foot_rgx) ~ "foot",
    str_detect(location2, hand_rgx) ~ "hand",
    .default="unknown"
  )) |>
  # tweak injury_region to separate ribs from rest of torso because so many in OS
  mutate(injury_region_ribs = case_when(
    str_detect(body_location, "\\b(rib)") ~ "ribs",
    .default = injury_region
  )) |>
  
  # get side
  mutate(injury_side = case_when(
    str_detect(body_location, "left and right") ~ "L&R",
    str_detect(body_location, "\\bleft\\b") ~ "L",
    str_detect(body_location, "\\bright\\b") ~ "R",
    .default = "U"
  ))  |>
  
  # add cemetery info
  mutate(cemetery_code = str_match(person_id, "-(.+)-")[,2]) |>
  mutate(cemetery_id = str_match(person_id, "^([a-z]+)")[,2]) |>
  mutate(cemetery = case_when(
    cemetery_id=="pr" ~  "Payne Road/Bow Baptist",
    cemetery_id=="rl" ~ "Royal London Hospital",
    cemetery_id=="sb" ~ "St. Bride's"
  )) |>
  
  rename(age_group=agegroup)  |>
  gender_simplify()




os_injured_persons <-
os_person_xlsx |>
  mutate(cemetery_code = str_match(person_id, "-(.+)-")[,2]) |>
  mutate(cemetery_id = str_match(person_id, "^([a-z]+)")[,2]) |>
  mutate(cemetery = case_when(
    cemetery_id=="pr" ~  "Payne Road/Bow Baptist",
    cemetery_id=="rl" ~ "Royal London Hospital",
    cemetery_id=="sb" ~ "St. Bride's"
  )) |>
  gender_simplify() |>
  rename(age_group = agegroup) |>
  inner_join(
    os_injury |>
      count(person_id, name="n_records"), by="person_id"
  )

# uninjured persons were omitted from the public data.
# uninjured individuals basic info only.
os_uninjured_persons <-
  read_csv(here::here("data/ost_uninjured_persons.csv")) |>
  mutate(age_group = str_to_title(age_group))


# combined injured and uninjured
# was ost_all_persons
os_all_persons <-
  bind_rows(
    os_injured_persons |> mutate(injury=TRUE),
    os_uninjured_persons |> mutate(n_records=1, injury=FALSE))
