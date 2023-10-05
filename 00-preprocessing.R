require(tidyverse)
require(readxl)

# load instruments
wg <- read_xlsx("forms/wordsandgestures.xlsx") %>% # 548
  filter(type=="word") # 448
ws <- read_xlsx("forms/wordsandsentences.xlsx") %>% # 816
  filter(type=="word") # 711


# import Sho's WG data
sho_wg_d <- read_csv("data/Sho/data_wordsandgestures_2023-09-25-17-01-25.csv") %>%
  filter(type=="word") %>%
  select(-`AgeD.(days)`) %>%
  mutate(produces = ifelse(value=="produces", 1, 0),
         produces = ifelse(is.na(value), 0, produces))

sum(sho_wg_d$ID!=sho_wg_d$id)
# id and ID are same


# proposed recoding:
# "", NA, "understands" -> 0;
# "produces" -> 1;

sho_proc <- sho_wg_d %>%
  rename(age = `AgeM.(months)`,
         category = category_en) %>%
  mutate(sex = ifelse(`Sex.(f/m)`=="m", "Male",
                      ifelse(`Sex.(f/m)`=="f", "Female", NA))) %>%
  select(id, age, sex, item_id, type, category, definition_en, produces,
         definition_ja1, definition_ja2) # ja1 is kanji/hiragana; ja2 is Latin alphabet

# tail(sort(table(sho_wg_items$definition_ja1)))
# duplicate definition: "さかな"

# some longitudinal data
sho_wg_wide <- sho_proc %>% select(id, age, sex, item_id, produces) %>% # definition_ja1
  pivot_wider(id_cols = c(id,age,sex), names_from=item_id, values_from=produces) %>% # item_id if trouble with unicode
  arrange(id,age)

# 448 - same as wg
sho_wg_items <- sho_wg_d %>% distinct(item_id, category_ja, category_en, definition_ja1, definition_ja2, definition_en)
sum(sho_wg_items$item_id != wg$item_id)

# N=101
sho_wg_demo <- sho_wg_wide %>% select(id, age, sex) %>%
  mutate(source = "Sho")
sho_wg_demo$production = rowSums(sho_wg_wide[,4:451])

sho_wg_demo %>% ggplot(aes(x=jitter(age), y=production, color=sex)) +
  geom_point() + theme_classic() +
  xlab("Age (months)") + ylab("Total Words Produced")

# sho_wg_demo and sho_wg_wide ready to merge



# import Sho's WS data
sho_ws_d <- read_csv("data/Sho/data_wordsandsentences_2023-09-25-15-56-16.csv") %>%
  filter(type=="word") %>%
  select(-`AgeD.(days)`) %>%
  mutate(produces = ifelse(value=="produces", 1, 0),
         produces = ifelse(is.na(value), 0, produces))

sho_ws_proc <- sho_ws_d %>%
  rename(age = `AgeM.(months)`,
         category = category_en) %>%
  mutate(sex = ifelse(`Sex.(f/m)`=="m", "Male",
                      ifelse(`Sex.(f/m)`=="f", "Female", NA))) %>%
  select(id, age, sex, item_id, type, category, definition_en, produces,
         definition_ja1, definition_ja2) # ja1 is kanji/hiragana; ja2 is Latin alphabet



sho_ws_wide <- sho_ws_proc %>% select(id, age, sex, item_id, produces) %>% # definition_ja1
  pivot_wider(id_cols = c(id,age,sex), names_from=item_id, values_from=produces) %>% # item_id if trouble with unicode
  arrange(id,age)

# 711 - same as ws
sho_ws_items <- sho_ws_d %>% distinct(item_id, category_ja, category_en, definition_ja1, definition_ja2, definition_en)
sum(sho_ws_items$item_id != ws$item_id)

# N=240
sho_ws_demo <- sho_ws_wide %>% select(id, age, sex) %>%
  mutate(source = "Sho")
sho_ws_demo$production = rowSums(sho_ws_wide[,4:714])

sho_ws_demo %>% ggplot(aes(x=jitter(age), y=production, color=sex)) +
  geom_point() + theme_classic() +
  xlab("Age (months)") + ylab("Total Words Produced")


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

hiro1_proc <- hiro1[3:(nrow(hiro1)-1),1:801] # last row is a header
names(hiro1_proc) = hiro1_header[2,1:801] # seq, ID, .., A1, A2, etc. - use row 1 for Hiragana

hiro2_proc <- hiro2[4:nrow(hiro2),]
names(hiro2_proc) = hiro2_header[2,]

hiro1_proc <- hiro1_proc %>% select(-starts_with("pg")) # remove these (pg1 - pg11)
hiro2_proc <- hiro2_proc %>% select(-starts_with("pg")) # remove these (pg1 - pg9)

# import Yasuyo's data, which are split by sex and each child's data is in a column
yas_m <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019BoysPilot.xlsx") #  847 x 64
# first 7 rows (translated with Google translate):
# Ⅰ Expressive Vocabulary A～X: Say ⇒ "1", Do not say ⇒ "0"
# II How to use words: Say often ⇒ “2”, say occasionally ⇒ “1”, never say ⇒ “0”
# Part 2 Grammar A: Say often ⇒ “2”, say occasionally ⇒ “1”, never say ⇒ “0”
# Part 2 Grammar B to C: Say ⇒ "1", Do not say ⇒ "0"
# Part 2 Grammar D: I speak quite often ⇒ 2, I speak occasionally ⇒ 1, I can't speak yet ⇒ 0
# Part 2 Grammar F: Lower selection ⇒ "1" Upper selection ⇒ "0"
yas_m_proc <- yas_m[10:nrow(yas_m),1:63] # final column is all NA
names(yas_m_proc) = c("category","word_id","definition",paste0("m",1:61))

yas_f <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019GirlsPilot.xlsx") # 847 x 63
# similar to yas_m: data starts on row 10
yas_f_proc <- yas_f[10:nrow(yas_f),]
names(yas_f_proc) = c("category","word_id","definition",paste0("f",1:20))
# ToDo: look at categories, fill in (does order of definitions match Sho's forms?)

yas_demo <- read_xlsx("data/Yasuyo/AgeBoys&Girls.xlsx") # 80
yas_demo <- yas_demo %>%
  rename(age = `Age(month)`) %>%
  mutate(sex = ifelse(`...1`=="Boy", "Male", "Female")) %>%
  select(-`...1`) %>%
  mutate(ID = ifelse(sex=="Male", paste0("m",ID), paste0("f",ID))) %>% # unique-ify the IDs
  mutate(source = "Yasuyo")
# age range is 36-42 months..a little old?


hiro_words1 = hiro1_header[1,7:ncol(hiro1_header)]
hiro_words2 = hiro2_header[1,7:ncol(hiro2_header)]
length(intersect(unlist(hiro_words1[1:779]), unlist(hiro_words2[1:780]))) # 766 out of 780
setdiff(unlist(hiro_words1[1:779]), unlist(hiro_words2[1:780])) # none
setdiff(unlist(hiro_words2[1:780]), unlist(hiro_words1[1:779])) # none...


# matching Hiro items to WG/WS:
length(intersect(wg$definition_ja1, unlist(hiro_words1[1:779]))) # WG and hiro1: 318 (of 448)
length(intersect(ws$definition_ja1, unlist(hiro_words1[1:779]))) # WS and hiro1: 539 (of 711)

length(intersect(wg$definition_ja1, unlist(hiro_words2[1:780]))) # WG and hiro1: 318 (of 448)
length(intersect(ws$definition_ja1, unlist(hiro_words2[1:780]))) # WS and hiro1: 539 (of 711)

intersect(wg$definition_ja1, unlist(hiro_words1[1:779]))
intersect(ws$definition_ja1, unlist(hiro_words1[1:779]))

# matching Sho items to WG/WS:
length(intersect(wg$item_id, sho_proc$item_id)) # 448
length(intersect(ws$item_id, sho_proc$item_id)) # 547
