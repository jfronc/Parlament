hl1993 <- read.table("C:/Users/jarom/Downloads/hl-1993ps/hl1993s.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(hl1993) <- c("id_hlasovani", "id_organ", "schuze", "cislo", "bod", "datum", "cas", "pro", "proti", "zdrzel", "nehlasoval", "prihlaseno", "kvorum", "druh_hlasovani", "vysledek", "nazev_dlouhy", "nazev_kratky")

subset(hl1993, pro == kvorum)

poslanec <- read.table("C:/Users/jarom/Downloads/poslanci/poslanec.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(poslanec) <- c('mp_id', 'id', 'region_id', 'list_id', 'org_id', 'web', 'street', 'municipality', 'postcode', 'email','phone', 'fax', 'psp_phone', 'facebook', 'photo', 'dummy')

osoby <- read.table("C:/Users/jarom/Downloads/poslanci/osoby.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(osoby) <- c('id', 'title_pre', 'family_name', 'given_name', 'title_post', 'birth_date', 'gender', 'updated_on', 'death_date', 'dummy')

organy <- read.table("C:/Users/jarom/Downloads/poslanci/organy.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(organy) <- c('org_id', 'sup_org_id', 'type_org_id', 'org_abbreviation', 'org_name_cs', 'org_name_en', 'org_since', 'org_until', 'priority', 'members_base', 'dummy')
