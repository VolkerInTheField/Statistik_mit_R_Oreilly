# # Versuch, einen Korrelationskoeffizienten grafisch darzustellen


werte <- tibble(x = c(1:10), y = 2 * x) %>% 
  mutate(x_mean = mean(x),
         y_mean = mean(y),
         xv = x - mean(x),
         yv = y - mean(y),
         kv = sum(xv * yv),
         zaehler = kv,
         nenner = sqrt(sum(xv * xv) * sum(yv * yv)),
         r = zaehler / nenner)
         
werte
w_df <- as.data.frame(werte)
gp_01 <- ggplot(werte, aes(x = x, y = y)) +
  geom_point() + 
  geom_point(aes(x = xv, y = yv)) + 
  geom_point(aes(x = x, y = kv), color = "red") +
  geom_point(aes(x = y, y = kv), color = "green")
gp_01

gp_02 <- ggplot(werte, aes(x = x, y = r)) +
  geom_line()
gp_02


mosaic::cor(AntragsVolumen[!is.na(AntragsVolumen)] ~ Antragsteller1BeschaeftigungsArt[Antragsteller1BeschaeftigungsArt == "ANGESTELLTER"],
    data = df_data, use = "pairwise.complete.obs", method = "spearman")
