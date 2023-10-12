require(tidyverse)
require(readxl)

# load instruments
wg <- read_xlsx("forms/wordsandgestures.xlsx") %>% # 548
  filter(type=="word") %>%  # 448
  mutate(definition_ja2 = tolower(definition_ja2),
         definition_en = tolower(definition_en))
ws <- read_xlsx("forms/wordsandsentences.xlsx") %>% # 816
  filter(type=="word") %>% # 711
  mutate(definition_ja2 = tolower(definition_ja2),
         definition_en = tolower(definition_en))

length(unique(ws$definition_ja1)) # 710 / 711 unique
length(unique(ws$definition_ja2)) # 697
length(unique(ws$definition_en)) # 662

wg_items <- table(wg$definition_en)
wg_items[which(wg_items>1)]
# some items appear 2 or even 3 times:
# airplane, bath, clean_up, cold, cry, do, eat, fall, fish, good, grandfather
# many of these appear under sounds and a noun category (e.g., grandfather/grandmother, airplane.)
# there

intersect(wg$definition_ja1, ws$definition_ja1) # 399 / 448 WG items
intersect(wg$definition_ja2, ws$definition_ja2) # 440 / 448 WG items
intersect(wg$definition_en, ws$definition_en) # 415



setdiff(wg$definition_ja2, ws$definition_ja2)
# "Meemee" "Gyuunyuu" "Baibai" "kore" "Taitai_Ototo" "Aret_ara" "Yada_iyada"

length(unique(wg$definition_ja2)) # 440
length(unique(wg$definition_en)) # 416



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
  mutate(source = "Sho",
         form = "WG")
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
  mutate(source = "Sho",
         form = "WS")
sho_ws_demo$production = rowSums(sho_ws_wide[,4:714])

sho_ws_demo %>% ggplot(aes(x=jitter(age), y=production, color=sex)) +
  geom_point() + theme_classic() +
  xlab("Age (months)") + ylab("Total Words Produced")


# import Hiromichi's data
hiro1 <- read_xlsx("data/Hiromichi/★se_20200619.xlsx", sheet="data")
hiro2_1 <- read_xlsx("data/Hiromichi/★sp1st_2nd.xlsx", sheet="sp1st")
hiro2_2 <- read_xlsx("data/Hiromichi/★sp1st_2nd.xlsx", sheet="sp2nd")
hiro2 <- hiro2_1 %>% bind_rows(hiro2_2[4:19,])

hiro1_header = hiro1[1:3,]
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

hiro1_proc <- hiro1[4:nrow(hiro1),]
names(hiro1_proc) = c(hiro1_header[3,1:5],
                      hiro1_header[2,6:ncol(hiro1)]) # seq, ID, .., A1, A2, etc. - use row 1 for Hiragana

hiro1_proc <- hiro1_proc %>%
  rename(age = ageM,
         sex = gender) %>%
  mutate(sex = ifelse(sex=="m", "Male", "Female")) %>%
  select(-starts_with("pg"), # remove these (pg1 - pg11)
         -seq, -ageD,
         -starts_with("q", ignore.case=F))


hiro2_proc <- hiro2[4:nrow(hiro2),]
names(hiro2_proc) = c(hiro2_header[3,1:5],
                      hiro2_header[2,6:ncol(hiro1)])

hiro2_proc <- hiro2_proc %>%
  rename(age = ageM,
         sex = gender) %>%
  mutate(sex = ifelse(sex=="m", "Male", "Female")) %>%
  select(-starts_with("pg"), # remove these (pg1 - pg9)
         -starts_with("q", ignore.case=F),
         -seq, -ageD)

length(intersect(names(hiro1_proc), names(hiro2_proc))) # all match: 714/714 - length of WS + 3 demo columns
length(intersect(names(hiro1_proc), ws$questionnaire_id)) # confirm we have all 711 WS items
length(intersect(names(hiro2_proc), ws$questionnaire_id)) # confirm we have all WS items

# combine data
intersect(hiro1_proc$ID, hiro2_proc$ID) # no overlapping IDs

hiro_long <- hiro1_proc %>% bind_rows(hiro2_proc) %>%
  pivot_longer(cols = 4:714) %>%
  mutate(produces = ifelse(value=="1", 1, 0),
         age = as.numeric(age)) %>%
  rename(questionnaire_id = name) %>% select(-value) %>%
  left_join(ws)

nrow(hiro2_proc) # 101
nrow(hiro2_proc %>% distinct(ID, sex, age)) # 101

# several longitudinal subjects
sort(table(hiro2_proc$ID))

hiro_demo <- hiro_long %>% distinct(ID, sex, age) %>%
  rename(id = ID) %>% arrange(id, age) %>%
  mutate(source="Hiromichi", form="WS")

hiro_wide <- hiro_long %>%
  pivot_wider(id_cols = c(ID, sex, age), names_from = item_id, values_from = produces) %>%
  rename(id = ID) %>% arrange(id, age)

hiro_demo$production = rowSums(hiro_wide %>% select(-id, -sex, -age))



# import Yasuyo's data, which are split by sex and each child's data is in a column
yas_m <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019BoysPilot.xlsx") #  847 x 64
# first 7 rows (translated with Google translate):
# Ⅰ Expressive Vocabulary A～X: Say ⇒ "1", Do not say ⇒ "0"
# II How to use words: Say often ⇒ “2”, say occasionally ⇒ “1”, never say ⇒ “0”
# Part 2 Grammar A: Say often ⇒ “2”, say occasionally ⇒ “1”, never say ⇒ “0”
# Part 2 Grammar B to C: Say ⇒ "1", Do not say ⇒ "0"
# Part 2 Grammar D: I speak quite often ⇒ 2, I speak occasionally ⇒ 1, I can't speak yet ⇒ 0
# Part 2 Grammar F: Lower selection ⇒ "1" Upper selection ⇒ "0"
yas_m_proc <- yas_m[10:720,1:63] # final column is all NA
names(yas_m_proc) = c("category","word_id","definition",paste0("m",1:61))

yas_f <- read_xlsx("data/Yasuyo/①日本語CDI語と文法2019GirlsPilot.xlsx") # 847 x 63
# similar to yas_m: data starts on row 10
yas_f_proc <- yas_f[10:720,]
names(yas_f_proc) = c("category","word_id","definition",paste0("f",1:20))
# ToDo: look at categories, fill in (does order of definitions match Sho's forms?)


yas_demo <- read_xlsx("data/Yasuyo/AgeBoys&Girls.xlsx") # 80
yas_demo <- yas_demo %>%
  rename(age = `Age(month)`) %>%
  mutate(sex = ifelse(`...1`=="Boy", "Male", "Female")) %>%
  select(-`...1`) %>%
  mutate(id = ifelse(sex=="Male", paste0("m",ID), paste0("f",ID))) %>% # unique-ify the IDs
  mutate(source = "Yasuyo",
         form = "WS") %>% arrange(id, age)
# age range is 36-42 months..a little old?

# got all 711 WS items!
length(intersect(yas_f_proc$word_id, ws$questionnaire_id))

yas_f_long <- yas_f_proc %>% pivot_longer(cols=4:23, names_to = "id", values_to = "produces") %>%
  select(-category) %>% filter(is.element(word_id, ws$questionnaire_id))
yas_m_long <- yas_m_proc %>% pivot_longer(cols=4:63, names_to = "id", values_to = "produces") %>%
  select(-category) %>% filter(is.element(word_id, ws$questionnaire_id))

yas_long <- yas_f_long %>% bind_rows(yas_m_long) %>%
  left_join(yas_demo %>% select(-ID))

yas_long <- yas_long %>%
  mutate(produces = ifelse(produces=="0", 0, 1)) %>%
  left_join(ws, by=c("word_id"="questionnaire_id"))

yas_wide <- yas_long %>%
  pivot_wider(id_cols = c(id, age, sex), names_from = item_id, values_from = produces) %>% # word_id has form A1, A2, .. B1, ..
  arrange(id, age)

yas_demo$production = rowSums(yas_wide %>% select(-id, -sex, -age))


d_demo <- sho_ws_demo %>% bind_rows(hiro_demo, yas_demo)
d_wide <- sho_ws_wide %>% bind_rows(hiro_wide, yas_wide) %>% select(-age, -sex)
#row.names(d_mat) = d_demo$id
save(d_demo, d_mat, file="Japanese-CDI-WS-data.Rdata")

d_demo %>% ggplot(aes(x=jitter(age), y=production, color=sex)) +
  geom_point(alpha=.5) + theme_classic() + geom_smooth(method = "lm", formula = y ~ 0 + x + I(x^2)) +
  xlab("Age (months)") + ylab("Total Words Produced")

table(d_demo$age)
table(d_demo$sex)
