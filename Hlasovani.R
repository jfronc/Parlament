hl1993 <- read.table("C:/Users/jarom/Downloads/hl-1993ps/hl1993s.unl", sep="|", header=FALSE, fileEncoding = "windows-1250")
colnames(hl1993) <- c("id_hlasovani", "id_organ", "schuze", "cislo", "bod", "datum", "cas", "pro", "proti", "zdrzel", "nehlasoval", "prihlaseno", "kvorum", "druh_hlasovani", "vysledek", "nazev_dlouhy", "nazev_kratky")
