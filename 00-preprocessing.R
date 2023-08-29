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

hiro1_header = hiro1[1:2,]
# cols 1:5 -> demo: seq, ID, gender, ageM, ageD
# first row is category
# 6: "A幼児語" -> infant language
# 19: "B動物の名前" -> animal sounds
# 62: "C乗り物" -> vehicles
# 77: "Dおもちゃ" -> toys
# 95: "E食べ物と飲み物" ->
# 163: "F衣類" -> clothing
# 188: "G体の部分" -> body parts
# ... etc
# 786: "Ⅴ結合後発話" -> "post-joining utterances"
# 802: "Sum" <- delete cols >=801?

hiro2_header = hiro2[1:3,]
# ...
# 787: "Ⅴ結合後発話"

hiro1_proc <- hiro1[3:nrow(hiro1),1:801]
names(hiro1_proc) = hiro1_header[2,1:801] # seq, ID, .., A1, A2, etc. - use row 1 for Hiragana

hiro2_proc <- hiro2[4:nrow(hiro2),]
names(hiro2_proc) = hiro2_header[2,]


# import Yasuyo's data, which are split by sex and each child's data is in a column
yas_m <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019BoysPilot.xlsx")
yas_f <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019GirlsPilot.xlsx")
yas_demo <- read_xlsx("data/Yasuyo/AgeBoys&Girls.xlsx") # 80
yas_demo <- yas_demo %>%
  mutate(sex = ifelse(`...1`=="Boy", "Male", "Female"))

wg <- read_csv("forms/wordsandgestures.csv") %>% # 548
  filter(type=="word") # 448
ws <- read_csv("forms/wordsandsentences.csv") %>% # 816
  filter(type=="word") # 711


