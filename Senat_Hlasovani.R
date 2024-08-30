# visualizuje konkrétní hlasování podle senátorských klubů

xfun::pkg_attach("tidyverse", "magrittr", "xml2")

division = function (nr) {
  base_url = 'https://www.senat.cz/xqw/xervlet/pssenat/hlasy?G='
  page = paste0(base_url, nr) %>% read_html()
  pro = list()
  proti = list()
  for (i in (1:6))  {
    table = xml_find_all(page, "//table[@class='PE_zebra outlineRows']")[i] %>% xml_find_all(".//tr") %>% xml_text()
    pro[i] = str_count(table, "A\\s") %>% sum()
    proti[i] = str_count(table, "(N|X)\\s") %>% sum() # zdržení se je efektivně hlas proti
  }
  df = data.frame(unlist(pro), unlist(proti))
  colnames(df) = c("pro", "proti")
  df$klub <- c("ODS+TOP", "STAN", "KDU", "ANO+SOCDEM", "SEN21+Pir", "Nez")
  
  df %<>% pivot_longer(cols = c(pro, proti), names_to = "Position", values_to = "Votes")
  
  ggplot(df, aes(x = Position, y = Votes, fill = klub)) +
    geom_bar(position="stack", stat = "identity") +
    labs(title = "Votes by Party and Position", x = "Party", y = "Hlasy") +
    geom_text(aes(label = Votes), size = 6, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
    theme_minimal()
}
    
# division(22440)

# TODO: zobecnit pro různá funkční období, nastavit barvy, z geom_text vyhodit malá čísla
