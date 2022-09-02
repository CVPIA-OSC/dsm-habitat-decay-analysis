library(tidyverse)

d <- read_tsv("data-raw/CrossSectionFluxes/SedimentRatingCurves/SedRatingCurve_RiverMile_283.33.txt",
              skip = 1, col_names = c("flow", "parker_qs", "wilcock_qs", "gaeuman_qs"))

s <- str_match("SedRatingCurve_RiverMile_291.98.txt", "[0-9]+\\.?[0-9]+")[,1]
files_to_read <- list.files("data-raw/CrossSectionFluxes/SedimentRatingCurves/",
                            pattern = ".txt",
                            full.names = TRUE)




res <- map_df(files_to_read, function(x) {
  river_mile <- str_match(x, "[0-9]+\\.?[0-9]+")[,1]
  read_tsv(x, skip = 1, col_names = c("flow", "parker_qs", "wilcock_qs", "gaeuman_qs")) %>%
    mutate(river_mile = as.numeric(river_mile))
})

write_csv(res, "data/rating-curves-compiled.csv")
