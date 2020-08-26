# # Modeling

df_data <- fread(paste0(getwd(),"/data/ds_rk_gmbh_gesamt.txt"))
df_data = as.data.frame(df_data)


# # Income to Volume
df_mod <- df_data %>% 
  select(AntragsVolumen, loan = Antragsteller1EinkommenMonatlich, Antragsteller1Geburtsdatum) %>% 
  filter(AntragsVolumen < 100000, loan < 10000,
         !is.na(Antragsteller1Geburtsdatum)) %>% 
  mutate(GebDat =  ymd(as.Date(Antragsteller1Geburtsdatum)),
         Alter = year(Sys.Date()) - year(GebDat)) %>% 
  filter(Alter < 80)
         
df_mod$training_case <- rnorm(nrow(df_mod)) > 0  
  
mod_in_vol <- lm(AntragsVolumen ~ loan, data = df_mod)

gp_01 <- ggplot(mod_in_vol, aes(x = loan, y = AntragsVolumen)) + 
  geom_line()

gp_01

gp_02 <- ggplot(mod_in_vol, aes(AntragsVolumen)) +
  geom_histogram()

gp_02

mod_age_vol <- lm(AntragsVolumen ~ Alter + loan, data = df_mod)
gp_03 <- ggplot(mod_age_vol, aes(x = Alter, y = AntragsVolumen)) +
  geom_line()
gp_03

df_pred <- data.frame(Alter = 25, loan = 1500)
predict(mod_age_vol, df_pred)

test_df <- df_mod %>% 
  filter(Alter == 25, loan == 1500)
mean(test_df$AntragsVolumen)

gp_04 <- ggplot(test_df, aes(AntragsVolumen)) + 
  geom_histogram(binwidth = 2500)
gp_04


mod_1 <- lm(AntragsVolumen ~ loan, data = subset(df_mod, training_case))
mod_2 <- lm(AntragsVolumen ~ loan + Alter, data = subset(df_mod, training_case))

preds_1 <- predict(mod_1, newdata = subset(df_mod, !training_case))
preds_2 <- predict(mod_2, newdata = subset(df_mod, !training_case))

errors_1 <- df_mod$AntragsVolumen - preds_1
errors_2 <- df_mod$AntragsVolumen - preds_2

head(errors_1)
head(errors_2)
