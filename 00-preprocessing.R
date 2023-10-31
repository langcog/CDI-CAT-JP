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

ws_wg <- ws %>% rename(WS = item_id) %>% left_join(wg %>% rename(WG = item_id))
ws_wg %>% write_csv("WS_WG_items_combined.csv")

# some EN definitions have duplicates/triplicates, need disambiguating:
en_dupes = names(which(sort(table(wg$definition_en))>1))
#View(subset(wg, is.element(definition_en, en_dupes)))

# only fish
ja1_dupes = names(which(sort(table(wg$definition_ja1))>1))
#View(subset(wg, is.element(definition_ja1, ja1_dupes)))

# 14 dupes
ja2_dupes = names(which(sort(table(wg$definition_ja2))>1))
#View(subset(wg, is.element(definition_ja2, ja2_dupes)))

subset(wg, definition_en=="shose")
# クック（靴）translates on Google as "cook (shoes)" (kukku, sounds2) - a shoe noise?

# sounds2 has a lot of the duplicate definitions...let's disambiguate by appending "(sound)"
length(unique(wg$definition_en))
# 416 unique EN defs -> 430 (of 448)
wg <- wg %>% mutate(definition_en = ifelse(category_en=="sounds2", paste(definition_en, "(sound)"), definition_en))
# 662 unique EN defs -> 677 (of 711)
ws <- ws %>% mutate(definition_en = ifelse(category_en=="sounds2", paste(definition_en, "(sound)"), definition_en))

wg[which(wg$definition_en=="shose"),]$definition_en = "shoes"

# update WG form 'fish'...
wg[which(wg$definition_en=="fish" & wg$category_en=="animals"),]$definition_en = "fish (animal)"
wg[which(wg$definition_en=="fish (animal)"),]$definition_ja2 = "sakana (animal)"
wg[which(wg$definition_en=="fish (animal)"),]$definition_ja2 = "さかな (animal)"
wg[which(wg$definition_en=="fish" & wg$category_en=="food_drink"),]$definition_en = "fish (food)"
wg[which(wg$definition_en=="fish (food)"),]$definition_ja2 = "sakana (food)"
wg[which(wg$definition_en=="fish (food)"),]$definition_ja1 = "さかな (food)"
# update WS form 'fish'..
ws[which(ws$definition_en=="fish" & ws$category_en=="animals"),]$definition_en = "fish (animal)"
ws[which(ws$definition_en=="fish (animal)"),]$definition_ja2 = "sakana (animal)"
ws[which(ws$definition_en=="fish (animal)"),]$definition_ja2 = "さかな (animal)"
ws[which(ws$definition_en=="fish" & ws$category_en=="food_drink"),]$definition_en = "fish (food)"
ws[which(ws$definition_en=="fish (food)"),]$definition_ja2 = "sakana (food)"
ws[which(ws$definition_en=="fish (food)"),]$definition_ja1 = "さかな (food)"

# instead of updating definitions in WG and WS data, we'll just match on item_id from forms

wg[which(wg$definition_ja1=="いえ（家）"),]$definition_en = "no (house)" # trans: no (house) ... not home?
wg[which(wg$definition_ja2=="moshimoshi"),]$definition_en = "hello (phone)"
wg[which(wg$definition_en=="bath" & wg$category_en=="furniture_rooms"),]$definition_en = "bath (object)"

# ...

#subset(wg, definition_en=="house")
subset(ws, definition_en=="fish")


# ToDo: save instruments in wordbank format, e.g.
# https://github.com/langcog/wordbank/blob/master/raw_data/Mandarin_Beijing_TC/%5BMandarin_Beijing_TC%5D.csv
# itemID	definition	item	category	type	choices	gloss	uni_lemma
# item_1	白菜	bai2cai4	food_drink	word	produces	cabbage	cabbage
# (and see also _data _fields and _values CSVs)


length(unique(ws$definition_ja1)) # 710 / 711 unique
length(unique(ws$definition_ja2)) # 697
length(unique(ws$definition_en)) # 677


# first 18 are the same
ws$definition_en[1:18] == wg$definition_en[1:18]
ws$definition_ja2[1:18] == wg$definition_ja2[1:18]

# so let's copy over from WS:
wg$definition_ja1[1:18] = ws$definition_ja1[1:18]


intersect(wg$definition_ja1, ws$definition_ja1) # 412 / 448 WG items
intersect(wg$definition_en, ws$definition_en) # 430 / 448 WG items
intersect(wg$definition_ja2, ws$definition_ja2) # 441 / 448 WG items
# match with definition_ja2?

setdiff(wg$definition_ja2, ws$definition_ja2) # none

# on WG, not on WS -- but should have a match
to_match <- setdiff(wg$definition_ja1, ws$definition_ja1)
# 36 items, many with parentheticals:
# [1] "あーあっ（よくないこと）" "アイタ（いたい）"         "オイチィ（美味しい）"     "ガーガー（アヒル）"
# it's actually the parentheses that are different, in some cases:
#subset(ws, definition_ja1=="あーあっ (よくないこと)")
#subset(ws, definition_ja1=="アイタ (いたい)")

#View(subset(wg, is.element(definition_ja1, to_match)))

length(unique(wg$definition_ja2)) # 441
length(unique(wg$definition_en)) # 433

wg %>% filter(definition_ja1=="だいじょうぶ（大丈夫）") # car, kuruma
wg %>% filter(definition_ja1=="きる（着る）")
ws %>% filter(definition_en=="all_right")


# we'll change the WG definition_ja1 to match the WS ones
wg <- wg %>%
  mutate(definition_ja1 = case_when(
    definition_ja1=="車（自動車）" ~ "車 (自動車)",
    definition_ja1=="りす" ~ "リス", # WS has katakana, not hiragana
    definition_ja1=="きる（着る）" ~ "着る", # WG has better disambiguation
    definition_ja1=="だいじょうぶ（大丈夫）" ~ "大丈夫",
    definition_ja1=="かさ" ~ "かさ（傘）",
    definition_ja1=="でんわ" ~ "でんわ（電話）",
    definition_ja1=="はこ" ~ "はこ（箱）",
    definition_ja1=="はし" ~ "はし（箸）",
    definition_ja1=="家" ~ "いえ（家）", # house; 'ie' -- ouchi is also house
    definition_ja1=="こども" ~ "こども（子ども）", # kodomo / child
    definition_ja1=="せんせい" ~ "せんせい（先生）",
    definition_ja1=="ほしい" ~ "ほしい（欲しい）",
    definition_ja1=="あさ" ~ "あさ（朝）",
    definition_ja1=="いま" ~ "いま（今）",
    definition_ja1=="きょう" ~ "きょう（今日）",
    definition_ja1=="ひる" ~ "ひる（昼）",
    definition_ja1=="よる" ~ "よる（夜）",
    definition_ja1=="あつい（暑い、熱い）" ~ "あつい（熱い・暑い）",
    definition_ja1=="いたい" ~ "いたい（痛い）",
    definition_ja1=="きらい" ~ "きらい（嫌い）",
    definition_ja1=="くさい" ~ "くさい（臭い）", # odor (uni_lemma = smell (desc))
    definition_ja1=="しずか" ~ "しずか（静か）",
    definition_ja1=="だいじ" ~ "だいじ（大事）",
    definition_ja1=="高い" ~ "たかい（高い）",
    definition_ja1=="つめたい" ~ "つめたい（冷たい）", # cold -- but there is a second WS cold (samui; 寒い)
    definition_ja1=="ぬれた" ~ "ぬれた（濡れた）",
    definition_ja1=="丸い" ~ "まるい（丸い）",
    definition_ja1=="うえ" ~ "うえ（上）",
    definition_ja1=="うしろ" ~ "うしろ（後ろ）",
    definition_ja1=="した" ~ "した（下）",
    definition_ja1=="そと" ~ "そと（外）",
    definition_ja1=="なか" ~ "なか（中）",
    definition_ja1=="まえ" ~ "まえ（前）", # before (location) - uni: "in front" ?
    definition_ja1=="むこう" ~ "むこう（向こう）",
    definition_ja1=="よこ" ~ "よこ（横）",
    definition_ja1=="いっしょ" ~ "いっしょ（一緒）",
    TRUE ~ definition_ja1
  ))


#wswg <- ws %>% left_join(wg, by=c(definition_ja1==))

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

sho_wg_long <- sho_wg_d %>%
  rename(age = `AgeM.(months)`,
         category = category_en) %>%
  mutate(sex = ifelse(`Sex.(f/m)`=="m", "Male",
                      ifelse(`Sex.(f/m)`=="f", "Female", NA)),
         form = "WG") %>%
  select(form, id, age, sex, item_id, category, produces) %>%
  left_join(wg)
         # definition_en, definition_ja1, definition_ja2) # get these from instrument
# ja1 is kanji/hiragana; ja2 is Latin alphabet


# import Sho's WS data
sho_ws_d <- read_csv("data/Sho/data_wordsandsentences_2023-09-25-15-56-16.csv") %>%
  filter(type=="word") %>%
  select(-`AgeD.(days)`) %>%
  mutate(produces = ifelse(value=="produces", 1, 0),
         produces = ifelse(is.na(value), 0, produces))

sho_ws_long <- sho_ws_d %>%
  rename(age = `AgeM.(months)`,
         category = category_en) %>%
  mutate(sex = ifelse(`Sex.(f/m)`=="m", "Male",
                      ifelse(`Sex.(f/m)`=="f", "Female", NA)),
         form = "WS") %>%
  select(form, id, age, sex, item_id, type, category, produces) %>%
  left_join(ws)
         # definition_en, definition_ja1, definition_ja2) # ja1 is kanji/hiragana; ja2 is Latin alphabet

sho_ws_wg_wide <- sho_wg_long %>%
  bind_rows(sho_ws_long) %>%
  select(form, id, age, sex, definition_ja1, produces) %>%
  pivot_wider(id_cols = c(form,id,age,sex), names_from=definition_ja1, values_from=produces) %>%
  arrange(id,age)

sho_ws_wg_demo <- sho_ws_wg_wide %>% select(form, id, age, sex) %>%
  mutate(source = "Sho")

sho_ws_wg_demo$production = rowSums(sho_ws_wg_wide[,5:714], na.rm=T)

sho_ws_wg_demo %>% ggplot(aes(x=jitter(age), y=production, color=sex)) +
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
         sex = gender,
         id = ID) %>%
  mutate(sex = ifelse(sex=="m", "Male", "Female")) %>%
  select(-starts_with("pg"), # remove these (pg1 - pg11)
         -seq, -ageD,
         -starts_with("q", ignore.case=F))


hiro2_proc <- hiro2[4:nrow(hiro2),]
names(hiro2_proc) = c(hiro2_header[3,1:5],
                      hiro2_header[2,6:ncol(hiro1)])

hiro2_proc <- hiro2_proc %>%
  rename(age = ageM,
         sex = gender,
         id = ID) %>%
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
nrow(hiro2_proc %>% distinct(id, sex, age)) # 101

# several longitudinal subjects
sort(table(hiro2_proc$ID))

hiro_demo <- hiro_long %>% distinct(id, sex, age) %>%
  arrange(id, age) %>%
  mutate(source="Hiromichi", form="WS")

hiro_wide <- hiro_long %>%
  pivot_wider(id_cols = c(id, sex, age), names_from = definition_ja1, values_from = produces) %>%
  mutate(form = "WS") %>%
  arrange(id, age)

hiro_demo$production = rowSums(hiro_wide %>% select(-id, -form, -sex, -age))



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
  pivot_wider(id_cols = c(id, age, sex), names_from = definition_ja1, values_from = produces) %>% # word_id has form A1, A2, .. B1, ..
  mutate(form = "WS") %>%
  arrange(id, age)

yas_demo$production = rowSums(yas_wide %>% select(-id, -form, -sex, -age))



d_demo <- sho_ws_wg_demo %>% bind_rows(hiro_demo, yas_demo)
d_wide <- sho_ws_wg_wide %>% bind_rows(hiro_wide, yas_wide) %>% select(-form, -age, -sex)
#row.names(d_mat) = d_demo$id
save(d_demo, d_wide, file="data/Japanese-CDI-WS-WG-combined-data.Rdata")

d_demo %>% ggplot(aes(x=jitter(age), y=production, color=sex)) +
  geom_point(alpha=.5) + theme_classic() + geom_smooth(method = "lm", formula = y ~ 0 + x + I(x^2)) +
  xlab("Age (months)") + ylab("Total Words Produced")

table(d_demo$age)
table(d_demo$sex)


unique(yas_long$category_en)

all_long <- sho_ws_long %>%
  bind_rows(sho_wg_long) %>%
  bind_rows(yas_long) %>%
  bind_rows(hiro_long)


# action + descriptive
noun_cats = c("body_parts","furniture_rooms","places","people","outside","locations") # "locations"? "others"?
predicate_cats = c("action_words","descriptive_words")
dd <- all_long %>%
  mutate(group = ifelse(is.element(category_en, noun_cats), "nouns",
                        ifelse(is.element(category_en, predicate_cats), "predicates", NA))) %>%
  group_by(form, id, age, group) %>%
    summarise(total = sum(produces)) %>%
  pivot_wider(id_cols =c(form,id,age), names_from=group, values_from=total)

dd %>%
  ggplot(aes(x=nouns+predicates, y=predicates)) +
  geom_point() + geom_abline(intercept = 0, slope = .5) +
  theme_classic()



dd <- all_long %>%
  mutate(group = ifelse(is.element(category_en, noun_cats), "nouns",
                        ifelse(category_en=="action_words", "verbs", NA))) %>%
  group_by(form, id, age, group) %>%
  summarise(total = sum(produces)) %>%
  pivot_wider(id_cols =c(form,id,age), names_from=group, values_from=total)

dd %>%
  ggplot(aes(x=nouns+verbs, y=verbs)) +
  geom_point() + geom_abline(intercept = 0, slope = .5) +
  theme_classic()



# predicates vs. all other (what slope tho?)
dd <- all_long %>%
  mutate(group = ifelse(is.element(category_en, predicate_cats), "predicates", "other")) %>%
  group_by(form, id, group) %>%
  summarise(total = sum(produces)) %>%
  pivot_wider(id_cols =c(form,id), names_from=group, values_from=total)

dd %>%
  ggplot(aes(x=predicates+other, y=predicates)) +
  geom_point() + geom_abline(intercept = 0, slope = .5) +
  theme_classic()
