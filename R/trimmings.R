# extra libraries for vis - but check in case any of these need to go back above tidyverse...

library(ggthemes)

### devtools::install_github("hrbrmstr/waffle")
library(waffle)

library(patchwork)

# gg theme replacing minimal
theme_set(theme_gray())
theme_update(panel.background = element_rect(fill="#fafafa"), 
             strip.background = element_rect(fill="#f0f0f0"),
             axis.ticks = element_line(colour = 'grey'))


# colours etc


## https://personal.sron.nl/~pault/ ptol colours for a change from colorbrewer
## TODO check groups for accessibility issues.


## Gender

col_gender <- c("female"= '#fc8d59', "male"= '#74add1', unknown="#dddddd")  # excl unknown, can c() it. src... brewer i think?
# these should be fixed with gender_simplify. label is undetermined rather than unknown in skeletons data. and u atm in dp, which will need fixing.


## Age groups

# 4 groups + Adult for OS only.
col_age_groups4_adult <- c("18-25"='#fecc5c', "26-35"= '#fd8d3c', "36-45"= '#fc4e2a', "46+"= '#800026', "Adult"='#dddddd')

# 5 + unknown
col_age_groups5_unknown <- c("0-17"='#ffeda0', "18-25"='#fecc5c', "26-35"= '#fd8d3c', "36-45"= '#fc4e2a', "46+"= '#800026', "unknown"='#dddddd')



## Injury categories

inj_category_relevel <- c("amputation", "dislocation", "blunt", "sharp", "burn", "muscle", "fracture", "wound")



ptol_inj_category <- 
  c("amputation"='#77AADD', 
  "dislocation"= '#EE8866', 
  "blunt"=  '#BBCC33',
  "sharp"= '#FFAABB', 
  "burn"= '#99DDFF', 
  "muscle"='#44BB99',
  "fracture"= '#EEDD88' , 
  "injury"='#AAAA00', 
  "wound"="#C2A5CF"
  )


# categories will vary a bit between datasets but aim for similar mapping. might reorder osteo and if make sure relevel matches!
# osteo: need amputation, dislocation, injury, muscle injury, blunt force, sharp force, fracture. 

ptol_cat_ost <- c('#C2A5CF',  '#E7D4E8', '#44BB99', '#FFAABB',  '#EEDD88' ,   '#AAAA00' , '#99DDFF')


inj_cat_ost_relevel <- c("amputation", "dislocation",  "muscle", "blunt", "sharp", "fracture")



ptol_cat_s <- c('#C2A5CF', '#EE8866',  '#EEDD88', '#E7D4E8',  '#99DDFF',  '#77AADD',   '#AAAA00',  '#FFAABB', '#44BB99', '#BBCC33' )  # ptol light plus 2 purple from another ptol scale 

ptol_cat_ss <- c( '#EE8866', '#EEDD88',  '#99DDFF', '#44BB99', '#BBCC33') # reuse  colours in remaining categories



## Injury locations

#find in files didn't find this in a qmd ??
#inj_reg_relevel <-  c( "head", "ribs", "torso", "arm", "hand", "leg",  "foot")

# region colour and ordering should work across datasets. 
inj_region_relevel <-  c( "head", "ribs", "torso", "arm", "hand", "leg",  "foot")
# version without ribs
inj_region_relevel6 <-  c( "head", "torso", "arm", "hand", "leg",  "foot")


ptol_region <- c("head"= '#44BB99' , 
                 "ribs"= '#882255', 
                 "torso"= '#AA4499', 
                 "arm"= '#0077BB', 
                 "hand"= '#33BBEE',  
                 "leg"= '#F67E4B',  
                 "foot"= '#FDB366', 
                  "unknown"= '#dddddd',
                 "unrecorded"="#dddddd")



## RLH outcomes
 
ptol_result3 <- c("#33bbee", "#ee7733", "#ccbb44") # ptol vibrant

ptol_result2 <-  c( '#5AAE61', '#9970AB')  # lighter c( '#C2A5CF',  '#ACD39E')

