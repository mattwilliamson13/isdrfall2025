library(FedData)
library(sf)
library(elevatr)

protected_areas <- get_padus(template = "Morley Nelson Snake River Birds Of Prey National Conservation Area",
                             label = "MNBOP")
z <- 14
zelev <- get_elev_raster(protected_areas[[1]], z = z, clip = "location")
mat <- raster_to_matrix(zelev)
