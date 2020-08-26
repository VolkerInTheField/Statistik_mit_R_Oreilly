# # Selbstgemachtes

library("data.table")
library("mosaic")

data <- fread(paste0(getwd(), "/data/ds_rk_gmbh_gesamt.txt"))

x <- mosaic::mean(AntragsVolumen ~ ProduktAnbieterId, data = data)
x

var1 <- c(1,2,3,4,5)
var2 <- c(1,2,3)

cross_over <- expand.grid(var1,var2)
cross_over
