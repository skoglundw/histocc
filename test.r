devtools::load_all()
library(tidyverse)
df = data.frame(occupation = c("Smed ", " snickare", "sold.", "sldr", "lärling", 'larling'))
df
df = standardize_occupation(df, 'occupation', fuzzy_threshold = 0.85, fuzzy_matching = TRUE)
df
print(names(df))
print("standard_occupation" %in% names(df))



df = df %>% occ_sector('occupation', label = FALSE, standardize_occupations = TRUE, fuzzy_matching = FALSE)
df

df = df %>% occ_hisco('occupation', description = TRUE, hisclass = FALSE, inc_score = TRUE, inc_score_type = 'mean', standardize_occupations = TRUE, status = FALSE)
df

df = df %>% occ_hisco('standard_occupation', description = FALSE, hisclass = TRUE, inc_score = FALSE)
df
library(haven)
library(tidyverse)
occ_crosswalk <- read_dta("C:/Users/wilsk523/Dropbox/occ_occ_stand_st_table_crosswalk.dta") %>%
  filter(!is.na(st_sector))

usethis::use_data(occ_crosswalk, overwrite = TRUE)

devtools::document()
devtools::load_all()


st_crosswalk = occ_crosswalk %>%
  rename('occ_dirty' = 'occ',
         'sector' = 'st_sector',
         'subsector' = 'st_subsector',
         'worker_type' = 'st_class') %>%
  distinct(occ_stand, .keep_all = TRUE) %>%
  select(-occ_dirty, -n)

usethis::use_data(st_crosswalk, overwrite = TRUE)
devtools::document()

library(readxl)
hisco_crosswalk = read_excel("C:/Users/wilsk523/Dropbox/Unions and Inequality/data/HISCO_swedpop.xlsx") %>%
  mutate(occ_stand = tolower(OCCUPATION_STANDARD)) %>%
  select(-OCCUPATION_STANDARD)



#Define function to code into HISCLASS
join_hisclass <- function(df, hisco_col, status_col, filepath) {
  #Read in the lookup table and keep the first two columns
  hisclass <- read.table(filepath, sep = ";", header = TRUE) %>%
    select(1, 2)
  #Create symbols for the columns in mutate
  status_sym <- rlang::sym(status_col)
  hisco_sym  <- rlang::sym(hisco_col)
  #Perform the left join and update hisclass_12
  df %>%
    #Left join on the renamed HISCO column (the first column in hisclass is named "hisco")
    left_join(
      hisclass,
      by = setNames("hisco", hisco_col)
    ) %>%
    #case_when logic
    mutate(
      hisclass_12 = case_when(
        #Masters (21) become "formen" => 6
        !!status_sym == 21 ~ 6,
        #Journeymen (22) with missing hisclass_12 => 7 (medium skilled)
        !!status_sym == 22 & is.na(hisclass_12) ~ 7,
        #Apprentices (23) => skill level one lower (hisclass_12 + 2) if 1 <= hisclass_12 <= 8 and not 4
        !!status_sym == 23 & !is.na(hisclass_12) &
          hisclass_12 >= 1 & hisclass_12 <= 8 & hisclass_12 != 4 ~ hisclass_12 + 2,
        #If 4, then => 5
        !!status_sym == 23 & hisclass_12 == 4 ~ 5,
        #Artisans (24) with missing hisclass_12 => 7 (medium skilled)
        !!status_sym == 24 & is.na(hisclass_12) ~ 7,
        #Principals (31): if 2 <= hisclass_12 <= 7 => hisclass_12 - 1, if 9 => 6
        !!status_sym == 31 & hisclass_12 >= 2 & hisclass_12 <= 7 ~ hisclass_12 - 1,
        !!status_sym == 31 & hisclass_12 == 9 ~ 6,
        #Subordinates (33): shift skill level down one => +2, including if == 4
        !!status_sym == 33 & hisclass_12 >= 1 &
          hisclass_12 <= 10 ~ hisclass_12 + 2,
        #Students (41) => NA
        !!status_sym == 41 ~ NA_real_,
        #Doctors (42) => 2 (higher professional)
        !!status_sym == 42 ~ 2,
        #51 or 52 with missing hisclass_12 => 1 (higher managers)
        ((!!status_sym == 51 | !!status_sym == 52) & is.na(hisclass_12)) ~ 1,
        #Otherwise, keep existing hisclass_12
        TRUE ~ hisclass_12
      )
    )
}



hisco_crosswalk <- join_hisclass(df = hisco_crosswalk, hisco_col = "HISCO",
                                  status_col = "STATUS",
                                  filepath = "C:/Users/wilsk523/Box/Projekt/Monopsonies/hisclass.csv")

inc_score = read_excel("C:/Users/wilsk523/Downloads/swedpop_inc_score.xlsx") %>%
  select(-count_n)

hisco_crosswalk = hisco_crosswalk %>% left_join(inc_score, by = c('HISCO' = 'hisco_swedpop')) %>%
  rename('hisco_description' = 'HISCO DESCRIPTION',
         'hisco' = 'HISCO',
         'status' = 'STATUS',
         'inc_score_median' = 'inc_median',
         'inc_score_mean' = 'inc_mean')
usethis::use_data(hisco_crosswalk, overwrite = TRUE)
