# # Korrealtionskoeffizient anhand des Datasets

df_ds <- as.data.frame(df_data %>% 
  filter(ist_sale == 1,
         Antragsteller1BeschaeftigungsArt != "RENTNER",
         Antragsteller2BeschaeftigungsArt != "RENTNER",
         !is.na(Antragsteller1Geburtsdatum)) %>% 
  transmute(VorgangsNummer,
            Antragsteller1BeschaeftigungsArt,
            kredit = AntragsVolumen,
            gebDatum = ymd(as.Date(Antragsteller1Geburtsdatum)),
            einkommen = Antragsteller1EinkommenMonatlich + Antragsteller2EinkommenMonatlich) %>% 
  mutate(alter = as.integer(year(Sys.Date()) - year(gebDatum)),
         alter = ifelse(is.na(alter), 0, alter)))
         
test_data <- data.frame(x = c(1:100), y = x * 2)

lin_korrkoeff <- function(x, col_1 = 1, col_2 = 1){
  vector_1 <- grep(paste0("^", col_1, "$"), names(x))
  
  return(abs(x[, vector_1] - mean(x[, vector_1])))
}

lin_korrkoeff(df_ds, "kredit", "alter")
lin_korrkoeff(werte, "x", "y")
werte
