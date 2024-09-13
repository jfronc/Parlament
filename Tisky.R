library(tidyverse)
library(magrittr)

#  stáhneme a extrahujeme data do složky
temp <- tempfile()
download.file("https://www.psp.cz/eknih/cdrom/opendata/tisky.zip", temp)
file.path(getwd(), "tisky") %>% unzip(temp, exdir = .)
file.remove(temp)

tisky <- readr::read_delim("tisky/tisky.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X5,X7,X10,X15,X17,X18,X23,X24,X25)) #  každá tabulka se převádí s plonkovým posledním sloupcem
colnames(tisky) <- c('id_tisk', 'id_druh', 'id_stav', 'ct', 'id_org', 'vo', 'id_osoba', 'shortname', 'predlozeno', 'roz1', 'dalsi', 'name', '90', 'url', 'eu', 'roz2')
tisky %<>% dplyr::filter(id_druh == 1 | id_druh == 2) #  vytřídíme návrhy zákonů
tisky$predlozeno %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()
tisky$roz1 %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()

hist <- readr::read_delim("tisky/hist.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X6,X7,X8,X9,X10,X11,X13,X16))
colnames(hist) <- c('id_hist', 'id_tisk', 'datum', 'id_hlas', 'id_prechod', 'sb_publ', 'sb_c', 'pozn')
hist$datum %<>% as.Date()
hist$sb_publ %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()

typ_stavu <- readr::read_delim("tisky/typ_stavu.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X3))
colnames(typ_stavu) <- c('id_typ', 'popis')

stavy <- readr::read_delim("tisky/stavy.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X7))
colnames(stavy) <- c('id_stav', 'id_typ', 'id_druh', 'pozn', 'lhuta', 'lhuta_w')
stavy %<>% left_join(typ_stavu, by = "id_typ") %>% dplyr::filter(id_druh == 1 | id_druh == 2)

typ_akce <- readr::read_delim("tisky/typ_akce.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X3))
colnames(typ_akce) <- c('id_akce', 'popis')

prechody <- readr::read_delim("tisky/prechody.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X6))
colnames(prechody) <- c('id_prechod', 'odkud', 'kam', 'id_akce', 'typ')
prechody %<>% left_join(typ_akce, by = "id_akce") %>%
  left_join(stavy %>% select(id_stav, popis), by = join_by(odkud == id_stav)) %>%
  left_join(stavy %>% select(id_stav, popis), by = join_by(kam == id_stav))
colnames(prechody) <- c('id_prechod', 'id_odkud', 'id_kam', 'id_akce', 'typ', 'popis', 'odkud', 'kam')
prechody$cesta <- paste0(prechody$odkud, " / ", prechody$popis, " / ", prechody$kam)

hist %<>% left_join(tisky %>% select(id_tisk, ct, shortname), by = "id_tisk")
hist %<>% left_join(prechody %>% select(id_prechod, cesta), by = "id_prechod")

# spočítáme, kolik dní trvá odeslat zákon ze Sněmovny do Sbírky
hist2 <- hist %>% dplyr::filter(id_prechod %in% c(57, 151, 1140, 2078, 2080, 2099))
hist3 <- subset(hist2, select = c(id_tisk, ct)) %>% distinct()
hist3 %<>% left_join(hist2 %>% dplyr::filter(id_prechod %in% c(2078, 2080, 2099)) %>% select(id_tisk, datum), by = "id_tisk") # přidá column s datem příchodu do PS
hist3 %<>% left_join(hist2 %>% dplyr::filter(id_prechod %in% c(57, 151, 1140)) %>% select(id_tisk, datum), by = "id_tisk") # přidá column s datem odchodu z PS do Sb.
colnames(hist3) <- c('id_tisk', 'ct', 'PS', 'Sb')
hist3 %<>% dplyr::filter(PS != "1900-01-01") # odfiltruje tisky s chybějícím datem, jakož i další NA hodnoty způsobené nestandardním procesem (veto prezidenta apod.)
hist3$days <- hist3$Sb - hist3$PS
as.numeric(hist3$days) %>% summary()

# spočítáme, kolik dní trvá odeslat zákon ze Sněmovny do Senátu
hist2 <- hist %>% dplyr::filter(id_prechod %in% c(33, 81, 127, 171, 1116, 38, 132, 1121))
hist3 <- subset(hist2, select = c(id_tisk, ct)) %>% distinct()
hist3 %<>% left_join(hist2 %>% dplyr::filter(id_prechod %in% c(33, 81, 127, 171, 1116)) %>% select(id_tisk, datum), by = "id_tisk") # přidá column s datem schválení
hist3 %<>% left_join(hist2 %>% dplyr::filter(id_prechod %in% c(38, 132, 1121)) %>% select(id_tisk, datum), by = "id_tisk") # přidá column s datem odchodu z PS do Senátu
colnames(hist3) <- c('id_tisk', 'ct', 'PS', 'Sen')
hist3 %<>% dplyr::filter(PS != "1900-01-01") # odfiltruje tisky s chybějícím datem, jakož i další NA hodnoty způsobené nestandardním procesem (veto prezidenta apod.)
hist3$days <- hist3$Sen - hist3$PS
as.numeric(hist3$days) %>% summary()
