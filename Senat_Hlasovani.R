# visualizuje konkrétní hlasování podle senátorských klubů

xfun::pkg_attach("tidyverse", "magrittr", "xml2")

division = function (nr) {
  base_url = 'https://www.senat.cz/xqw/xervlet/pssenat/hlasy?G='
  page = paste0(base_url, nr) %>% read_html()
  pro = list()
  proti = list()
  tables = xml_find_all(page, "//table[@class='PE_zebra outlineRows']")
  for (i in seq_along(tables))  {
    table = tables[i] %>% xml_find_all(".//tr") %>% xml_text()
    pro[i] = str_count(table, "A\\s") %>% sum()
    proti[i] = str_count(table, "(N|X)\\s") %>% sum() # zdržení se je efektivně hlas proti
  }
  df = data.frame(unlist(pro), unlist(proti))
  colnames(df) = c("pro", "proti")
  df$klub <- c("ODS+TOP", "STAN", "KDU", "ANO+SOCDEM", "SEN21+Pir", "Nez")
  
  df %<>% pivot_longer(cols = c(pro, proti), names_to = "Position", values_to = "Votes")
  
  ggplot(df, aes(x = Position, y = Votes, fill = klub)) +
    geom_bar(position="stack", stat = "identity", width = 0.5) +
    labs(title = "Pro a proti podle klubů", x = "Hlasování", y = "Hlasy") +
    geom_text(aes(label = ifelse(Votes >= 2, Votes, "")), size = 5, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    theme_minimal()
}

# division(22488)

# TODO: zobecnit pro různá funkční období, nastavit barvy
