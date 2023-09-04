library(tidyverse)
library(magrittr)

tisky <- readr::read_delim("tisky/tisky.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X5,X7,X10,X15,X17,X18,X23,X24,X25))
colnames(tisky) <- c('id_tisk', 'id_druh', 'id_stav', 'ct', 'id_org', 'vo', 'id_osoba', 'shortname', 'predlozeno', 'roz1', 'dalsi', 'name', '90', 'url', 'eu', 'roz2')
tisky %<>% dplyr::filter(id_druh == 1 | id_druh == 2) #  vytřídíme návrhy zákonů
tisky$predlozeno %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()
tisky$roz1 %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()

hist <- readr::read_delim("tisky/hist.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X6,X7,X8,X9,X10,X11,X13,X16))
colnames(hist) <- c('id_hist', 'id_tisk', 'datum', 'id_hlas', 'id_prechod', 'sb_publ', 'sb_c', 'pozn')
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
prechody %<>% left_join(typ_akce, by = "id_akce")
prechody %<>% left_join(stavy %>% select(id_stav, popis), by = join_by(odkud == id_stav))
prechody %<>% left_join(stavy %>% select(id_stav, popis), by = join_by(kam == id_stav))
colnames(prechody) <- c('id_prechod', 'id_odkud', 'id_kam', 'id_akce', 'typ', 'popis', 'odkud', 'kam')
prechody$cesta <- paste0(prechody$odkud, " / ", prechody$popis, " / ", prechody$kam)
