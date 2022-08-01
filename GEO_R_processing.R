library(terra)

all_pts <- read.csv("ANALYSIS/ALL_SOILC_7_11_22_samp.csv")
levels(as.factor(all_pts$AGE_LITH2))

WA_GEO <- vect("SPATIAL LAYERS/WA_GEO/WA_geologic_unit_poly_100k.shp")

COL_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/COL/COL_poly.shp")
HOH_poly_unag <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/HOH/HOH_poly/Hoh_outline.shp")
HOH_poly <- aggregate(HOH_poly_unag, "Id", dissolve = TRUE)
MAS_poly <- vect("SPATIAL LAYERS/SPATIAL_LAYERS_7_11_22/MAS/MAS_poly/mashel_poly.shp")


COL_CROP <- crop(WA_GEO, COL_poly)

terra::plot(COL_CROP, "AGE_LITHOL", col = rainbow(16))


values(COL_CROP)["AGE_LITHOL"] <- c("Tertiary intrusive rocks",
                                    "Tertiary intrusive rocks",
                                    "pre-Tertiary heterogeneous metamorphic rocks",
                                    "pre-Tertiary metasedimentary rocks",
                                    "pre-Tertiary metamorphic and igneous rocks",
                                    "pre-Tertiary heterogeneous metamorphic rocks",
                                    "pre-Tertiary heterogeneous metamorphic rocks",
                                    "pre-Tertiary heterogeneous metamorphic rocks",
                                    "pre-Tertiary metasedimentary rocks",
                                    "pre-Tertiary metasedimentary rocks",
                                    "pre-Tertiary heterogeneous metamorphic rocks",
                                    "Quaternary alluvium",
                                    "Pleistocene bog, marsh, swamp, or lake deposits",
                                    "Pleistocene continental glacial drift",
                                    "Pleistocene continental glacial till",
                                    "Water")

#What to do:
# Recode the geology codes in the dataframe to consolidate some layers.
    # Some of the Colville geology does not fit into catagories when we go to model the area