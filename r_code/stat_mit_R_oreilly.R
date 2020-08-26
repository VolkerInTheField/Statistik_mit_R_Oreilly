# # Daten einlesen
packages <- c("dplyr", "mosaic", "data.table")
lapply(packages, require, character.only = TRUE)

df <- read.table(paste0(getwd(),"/data/daten_final.csv"),
                 header = TRUE, sep = ";", quote = "\"", dec = ",",
                 na.strings = "")
names(df)

df <- df %>%
  mutate(INCOMEGROUP = as.factor(INCOMEGROUP))

df[df$INCOMEGROUP == "High income: nonOECD" | 
     df$INCOMEGROUP == "High income: OECD"] <- "High income"

df2 <- df %>% 
  mutate(INCOMEGROUP = ifelse(INCOMEGROUP == "High income: nonOECD" | 
                              INCOMEGROUP == "High income: OECD", "High income",
                              INCOMEGROUP),
         INCOMEGROUP = as.factor(INCOMEGROUP),
         WP_F = cut(WOMENPARL, breaks = c(0,10,25,50,100),
                    labels = c("sehr niedrig", "niedrig", "mittel", "hoch")))

df2$INCOMEGROUP
df2$INCOMEGROUP <- as.factor(df2$INCOMEGROUP)
levels(df2$INCOMEGROUP)[3]

df2$WOMENPARL

# # Kapitel 5: Daten deskriptiv analysieren
summary(df$EXPEND)

# # Funktion zur Berechnung der mittleren absolute Abweichung
maa <- function(wert){
  z <- (sum(abs(wert[!is.na(wert)] - mean(wert, na.rm = TRUE))) / length(wert[!is.na(wert)]))
  return(z)
  
}




