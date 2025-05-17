# instala usethis si no lo tienes
# install.packages("usethis")
library(usethis)
library(readxl)

# 1) Lee los Excel desde inst/extdata
carreras_peruanas <- read_excel("carreras_peruanas.xlsx")


# 2. Rename in your workspace
carreras_peruanas  <- carreras_peruanas


# 3. Save them into data/ with the correct names
usethis::use_data(carreras_peruanas, overwrite = TRUE)
