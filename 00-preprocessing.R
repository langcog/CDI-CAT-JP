require(tidyverse)
require(readxl)

# import Sho's WG data
sho_d <- read_csv("data/Sho/data_wordsandgestures_2023-08-10-16-53-17.csv")
# warning: has birthdays
sum(sho_d$ID!=sho_d$id)
# id and ID are same

# response:
table(sho_d$value)
# blank = No?
# what is "yes"? produces?

sho_proc <- sho_d %>%
  rename(age = agemonths,
         category = category_en) %>%
  mutate(sex = ifelse(`Sex.(f/m)`=="m", "Male",
                      ifelse(`Sex.(f/m)`=="f", "Female", NA))) %>%
  select(id, age, sex, item_id, type, category, definition_en,
         definition_ja1, definition_ja2) # ja1 is kanji/hiragana; ja2 is Latin alphabet

# import Hiromichi's data
hiro1 <- read_xlsx("data/Hiromichi/仛se_20200619.xlsx", sheet="data")
# hiro2 <- read_xlsx("data/Hiromichi/仛sp1st_2nd.xlsx") # GK formatted and re-saved
hiro2 <- read_xlsx("data/Hiromichi/sp1st_2nd_GKformatted.xlsx")

# import Yasuyo's data, which are split by sex and each child's data is in a column
yas_m <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019BoysPilot.xlsx")
yas_f <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019GirlsPilot.xlsx")
yas_demo <- read_xlsx("data/Yasuyo/AgeBoys&Girls.xlsx") # 80
yas_demo <- yas_demo %>%
  mutate(sex = ifelse(`...1`=="Boy", "Male", "Female"))

wg <- read_csv2("forms/wordsandgestures.csv") %>% # 548
  filter(type=="word") # 448
ws <- read_csv2("forms/wordsandsentences.csv") %>% # 816
  filter(type=="word") # 711


