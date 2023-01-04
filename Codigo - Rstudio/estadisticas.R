

lines_anios <- datos %>% 
  count(anios = year(FECHA_UTC)) %>% 
  ggplot(aes(anios,n)) + geom_line(color = "cyan4", size = 1) +
  labs(title = "Sismos detectadatos en el Perú",
       subtitle = "Sismos por año (1960-2021)",
       x = "Años", y = "Cantidad de sismos")

ggplotly(lines_anios)

barras_meses <- datos %>% 
  count(meses = month(FECHA_UTC)) %>% 
  ggplot(aes(reorder(month.name, meses),n)) + geom_bar(stat = "identity") +
  geom_col(aes(fill = n)) +
  geom_label(aes(reorder(month.name, meses), y= n,
                 label=n)) +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title = "Sismos detectadatos en el Perú",
       subtitle = "Sismos por meses (1960-2021)",
       x = "Meses", y = "Cantidad de sismos")



datos %>% 
  filter(MAGNITUD > 6) %>%
  count(anios = year(FECHA_UTC)) %>% 
  ggplot(aes(anios,n)) + geom_line(color = "cyan4", size = 1) +
  labs(title = "Sismos detectadatos en el Perú",
       subtitle = "Sismos por año (Magnitud > 6, años 1960-2021)",
       x = "Años", y = "Cantidad de sismos")

datos %>% 
  filter(MAGNITUD > 6) %>% 
  count(meses = month(FECHA_UTC)) %>% 
  ggplot(aes(reorder(month.name[meses], meses),n)) + 
  geom_bar(stat = "identity", fill = "darkcyan") +
  geom_label(aes(reorder(month.name[meses], meses), y= n,
                 label=n), fill = "darkcyan") +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title = "Sismos detectadatos en el Perú",
       subtitle = "Sismos por meses (Magnitud > 6, años 1960-2021)",
       x = "Meses", y = "Cantidad de sismos")
