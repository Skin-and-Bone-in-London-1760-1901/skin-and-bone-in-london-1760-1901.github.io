## stuff for all scripts ####


# libraries for wrangling

library(readxl)
library(janitor)
library(scales)
library(glue)

# not used a lot but need to go before tidyverse
library(knitr)
library(kableExtra)


library(tidytext)
library(tidyverse)

# extra ggplot packages in trimmings.r


# abbreviations and suchlike
#
# OS Osteology
# 	  pr - Payne Road/Bow Baptist 1816-54
# 	  rl - Royal London Hospital 1825-41
# 	  sb - St. Bride's 1770-1849
#
# HP Hospitals
# 	  ghs -	Guy's Hospital 1810-39
# 	  mxh -	Middlesex Hospital 1760-78
# 	  rlh -	Royal London Hospital 1760-1805
# 	  sth -	St Thomas Hospital 1773-1809
#
# DP Digital Panopticon
# 	  hcr - Home Office Criminal Registers 1791-1802
# 	  mpr - Millbank Prison Registers 1816-28
# 	  tlm - Male Prison Licences 1853?-188x?
# 	  pld - Female Prison Licences 1853?-1883?
#   	  pl - combined male and female licences (for some analysis)
# 	  rhc - Registers of Habitual Criminals 1881-82, 1889-1901
#


## sex/gender ####

## tidy up sex/gender, if not already done. should cover all variations.

gender_simplify <- function(df){
## assumes a column named gender already exists.
  df |>
    mutate(gender = str_to_lower(gender)) |>
    mutate(gender = case_match(
      gender,
      "f" ~ "female",
      "m" ~ "male",
      "undetermined" ~ "unknown", 
      "u" ~ "unknown",
      NA ~ "unknown",
      .default = gender
    ))
}


## age groups from ages ####

make_age_groups <- function(df){
## assumes a column named age exists, and that it's numerical.
  df |>
    mutate(age_group = case_when(
      is.na(age) ~ "unknown",
      between(age, 0, 17) ~ "0-17",
      between(age, 18, 25) ~ "18-25",
      between(age, 26, 35) ~ "26-35",
      between(age, 36, 45) ~ "36-45",
      age>45 ~ "46+"
    ))
}




## body locations ####

## regions lookup

regions_locations <-
  read_csv(here::here("data/locations_regions.csv"))

regions_locations_rgx <-
  regions_locations |>
  filter(!injury_location %in% c("body", "side")) |>
  group_by(region) |>
  summarise(locations_rgx = glue_collapse(injury_location, sep="|")) |>
  ungroup() |>
  mutate(locations_rgx=glue("\\b({locations_rgx})")) 

arm_rgx <-  pluck(regions_locations_rgx, "locations_rgx", 1)

foot_rgx <-  pluck(regions_locations_rgx, "locations_rgx", 2)

hand_rgx <- pluck(regions_locations_rgx, "locations_rgx", 3)

head_rgx <- pluck(regions_locations_rgx, "locations_rgx", 4)

leg_rgx <- pluck(regions_locations_rgx, "locations_rgx", 5)

torso_rgx <- pluck(regions_locations_rgx, "locations_rgx", 6)


## injury categories ####

injury_categories_for_analysis <- c("fracture", "blunt", "sharp", "wound", "burn", "muscle", "dislocation", "amputation")
# eight categories for analysis 
# fracture
# blunt 
# sharp
# wound (diverse category of surface injuries not classifiable as blunt/sharp)
# burn
# muscle (sprains, strains, etc)
# dislocation
# amputation

# plus
# injury - description is non-specific
# chronic - various kinds of physical impairment which *might* be results of accidents but can't be sure
# other = injuries (probably...) but descriptions are not specific enough to judge what sort of injury
  # keep other, injury, chronic,  for analysis when type of injury isn't important 

# NA = not relevant, not injuries, too fragmentary to tell what they are, etc, to be removed *before* any analysis (only a few)


injury_classify <- function(df){
# there must be an injury column
  
  df |>
    mutate(injury_category = str_to_lower(injury)) |>
    mutate(injury_category = case_when(
      str_detect(injury_category, "\\b(struck by lightening|flash of lightning|frost.?bite|frost.?bitten)\\b") ~ NA, # environmental. [was "remove"]
      str_detect(injury_category, "\\b(fractur|broken|compound|avulsion|hairline|splintered)") ~ "fracture",
      # burn needs to be before scar/mark.
      str_detect(injury_category, "\\b(burn|scald|mortar in|d (on )?left )") ~ "burn",
      str_detect(injury_category, "\\b(sharp|cut|scar|stab|bit|needle|pin|punctur|nail (in|through))|slit") ~ "sharp",
      str_detect(injury_category, "\\b(sprain|strain|soft tissue|muscle|spain)") ~ "muscle",
      str_detect(injury_category, "\\b(amputat)") ~ "amputation",
      # DP only. some are amputation, but final choice will depend on location
      str_detect(injury_category, "\\b(lost|missing)") ~ "lost-tbd",
      str_detect(injury_category, "\\b(blunt|lacerat|contus|bruis|kick|concus|jam|compres|strang|r.n over|fall|lump|swelling|knocked up|ruptured|split|torn|flogg|corporal|internal|inward)") ~ "blunt", 
      str_detect(injury_category, "\\b(dislocat|sublux|luxat|displaced)") ~ "dislocation",
      # chronic; some might be results of injuries but not enough info to be sure. DP only
      str_detect(injury_category, "\\b(bent|crooked|inclined|contracted|crippled|defect|deficient|deformed|disfigured|lame|limp|blind|cast)") ~ "chronic",
      # not enough info to categorise
      str_detect(injury_category, "\\b(been injured|injured|injury|inury)") ~ "injury",
      str_detect(injury_category, "\\b(gun|shot|gun.shot|wound)") ~ "wound",
      str_detect(injury_category, "\\b(blue|red|purple|coal)") ~ "wound", # a bit uncertain. probably industrial but some might be tattoos 
      # non-specific, not enough info, etc, but probably are injuries.
      str_detect(injury_category, "\\b(accident|suffocated|suffocation|drowned|hurt leg)|^hurt") ~ "other",

    ))
}

