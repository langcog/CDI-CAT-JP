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
  mutate(sex = toupper(`Sex.(f/m)`)) %>% # f/m -> F/M
  select(id, age, sex, item_id, type, category, definition_en,
         definition_ja1, definition_ja2) # difference between these?

# import Hiromichi's data
hiro1 <- read_xlsx("data/Hiromichi/仛se_20200619.xlsx")
hiro2 <- read_xlsx("data/Hiromichi/仛sp1st_2nd.xlsx")


# import Yasuyo's data
yas_m <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019BoysPilot.xlsx")
yas_f <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019GirlsPilot.xlsx")
yas_demo <- read_xlsx("data/Yasuyo/AgeBoys&Girls.xlsx")

