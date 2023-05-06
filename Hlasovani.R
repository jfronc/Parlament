library(dplyr)
library(magrittr)
library(readr)

hl1993 <- read.table("C:/Users/jarom/Downloads/hl-1993ps/hl1993s.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(hl1993) <- c("id_hlasovani", "id_organ", "schuze", "cislo", "bod", "datum", "cas", "pro", "proti", "zdrzel", "nehlasoval", "prihlaseno", "kvorum", "druh_hlasovani", "vysledek", "nazev_dlouhy", "nazev_kratky")

subset(hl1993, pro == kvorum)

poslanec <- read.table("C:/Users/jarom/Downloads/poslanci/poslanec.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(poslanec) <- c('mp_id', 'id', 'region_id', 'list_id', 'org_id', 'web', 'street', 'municipality', 'postcode', 'email','phone', 'fax', 'psp_phone', 'facebook', 'photo', 'dummy')

osoby <- read.table("C:/Users/jarom/Downloads/poslanci/osoby.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(osoby) <- c('id', 'title_pre', 'family_name', 'given_name', 'title_post', 'birth_date', 'gender', 'updated_on', 'death_date', 'dummy')

organy <- read_delim("C:/Users/jarom/Downloads/poslanci/organy.unl", 
                    delim = "|", 
                    col_names = FALSE, 
                    locale = locale(encoding = "windows-1250"))
# readr::read.table nepřevedla správně některé řádky
colnames(organy) <- c('org_id', 'sup_org_id', 'type_org_id', 'org_abbreviation', 'org_name_cs', 'org_name_en', 'org_since', 'org_until', 'priority', 'members_base', 'dummy')

kluby <- filter(organy, type_org_id == 1)

zarazeni <- read.table("C:/Users/jarom/Downloads/poslanci/zarazeni.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(zarazeni) <- c('id', 'org_id', 'cl_funkce', 'since', 'until', 'since_f', 'until_f', 'dummy')
zarazeni$since %<>% as.Date()
zarazeni$until %<>% as.Date()
# Obsahuje zvlášť řádky pro členství a funkci (cl_funkce = 1) ve stejném orgánu.

kluby_n <- function(date) {
  zarazeni %>% 
  filter(cl_funkce == 0) %>%  # nutné odfiltrovat řádky s fcí v PK; pokud cl_funkce == 1, pak id_of odpovídá funkce:id_funkce.
  filter(org_id %in% kluby$org_id) %>%  left_join(kluby %>% select(org_id, org_abbreviation), by = "org_id") %>%
  filter(since <= as.Date(date, origin = "1970-01-01") & (until > as.Date(date, origin = "1970-01-01") | is.na(until))) %>%
  group_by(org_abbreviation) %>% summarize(count = n()) %>% bind_rows(data.frame(org_abbreviation = "Celkem", count = sum(.$count)))
}

# Vývoj počtu mandátů
dates <- unique(zarazeni$since)
dates <- dates[dates >= as.Date("1992-01-01", origin = "1970-01-01")]

kluby_df <- data.frame() # initialize empty data frame

for (date in dates) {
  kluby_count <- kluby_n(date)
  kluby_count$date <- as.Date(date, origin = "1970-01-01")
  kluby_df <- rbind(kluby_df, kluby_count)
 }

library(tidyr)
kluby_df_pivoted <- pivot_wider(kluby_df, names_from = org_abbreviation, values_from = count, values_fill = 0) %>% 
  filter(Celkem >= 197 & Celkem <= 200) %>%  # odstraní řádky okolo přelomu VO
  arrange(date) %>%
  distinct(across(-date), .keep_all = TRUE) # odstraní řádky, kde nedošlo ke změně počtů

# Kdo byl v klubu ODS ke dni?
zarazeni %>% left_join(osoby %>% select(id, given_name, family_name), , by = "id") %>% filter(org_id == "15", since <= as.Date("1993-01-01"), (until >= as.Date("1993-01-01") | is.na(until))) %>% write.xlsx(file = "C:/Users/jarom/Downloads/ODS.xlsx", sheetName = "Sheet1", row.names = FALSE)
