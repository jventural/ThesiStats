# instala usethis si no lo tienes
# install.packages("usethis")
library(usethis)
library(readxl)

# 1) Lee los Excel desde inst/extdata
universidades_peruanas <- read_excel("universidades_peruanas.xlsx")


# 2. Rename in your workspace
universidades_peruanas  <- universidades_peruanas


# 3. Save them into data/ with the correct names
usethis::use_data(universidades_peruanas, overwrite = TRUE)
