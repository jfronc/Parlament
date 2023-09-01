library(tidyverse)
library(magrittr)

tisky <- readr::read_delim("tisky/tisky.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X5,X7,X10,X15,X17,X18,X23,X24,X25)) %>%
  dplyr::filter(tisky, id_druh == 1 | id_druh == 2) #  vytřídíme návrhy zákonů
colnames(tisky) <- c('id_tisk', 'id_druh', 'id_stav', 'ct', 'id_org', 'vo', 'id_osoba', 'shortname', 'predlozeno', 'roz1', 'dalsi', 'name', '90', 'url', 'eu', 'roz2')
tisky$predlozeno %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()
tisky$roz1 %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()

hist <- readr::read_delim("tisky/hist.unl", delim = "|", col_names = FALSE, locale = locale(encoding = "windows-1250")) %>% subset(select = -c(X6,X7,X8,X9,X10,X11,X13,X16))
colnames(hist) <- c('id_hist', 'id_tisk', 'datum', 'id_hlas', 'id_prechod', 'sb_publ', 'sb_c', 'pozn')
hist$sb_publ %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()
