#!/usr/bin/env Rscript
# ----------------------------------------------------------------------
# Vanity viewer: postal polygons (2-digit, 3-digit, districts)
#
# WHAT
#   - Loads output/*.geojson polygons and output/*_labels.gpkg points
#   - Renders quick static previews with ggplot2
#   - Builds an interactive Leaflet map with layer toggles & labels
#
# INPUTS (repo-relative; created by your build scripts)
#   output/postal_2d.geojson
#   output/postal_3d.geojson
#   output/postal_districts.geojson
#   output/postal_2d_labels.gpkg       (layer = "labels")
#   output/postal_3d_labels.gpkg       (layer = "labels_3d")
#   output/postal_districts_labels.gpkg (layer = "labels_district")
#
# OUTPUTS
#   output/preview_postal_layers.png        # ggplot montage (optional)
#   output/postal_layers_map.html           # Leaflet widget (optional)
# ----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(htmlwidgets)
})


# --- paths --------------------------------------------------------------
f2d <- "output/postal_2d.geojson"
f3d <- "output/postal_3d.geojson"
fd  <- "output/postal_districts.geojson"

l2d <- list(path = "output/postal_2d_labels.gpkg", layer = "labels")
l3d <- list(path = "output/postal_3d_labels.gpkg", layer = "labels_3d")
ld  <- list(path = "output/postal_districts_labels.gpkg", layer = "labels_district")

# --- load ---------------------------------------------------------------
message("Reading polygons …")
g2d <- st_read(f2d, quiet = TRUE)
g3d <- st_read(f3d, quiet = TRUE)
gd  <- st_read(fd,  quiet = TRUE)

message("Reading labels …")
labs2d <- st_read(l2d$path, layer = l2d$layer, quiet = TRUE)
labs3d <- st_read(l3d$path, layer = l3d$layer, quiet = TRUE)
labsd  <- st_read(ld$path,  layer = ld$layer,  quiet = TRUE)

# --- quick ggplot previews ----------------------------------------------
# Note: keeps fills simple; labels come from precomputed label points.
p2 <- ggplot() +
  geom_sf(data = g2d, aes(fill = postal_2d), color = "white", linewidth = 0.1) +
  geom_sf_text(data = labs2d, aes(label = postal_2d), size = 3, fontface = "bold") +
  guides(fill = "none") +
  ggtitle("Postal 2-digit") +
  theme_void()

p3 <- ggplot() +
  geom_sf(data = g3d, aes(fill = postal_3d), color = "white", linewidth = 0.05) +
  geom_sf_text(data = labs3d, aes(label = postal_3d), size = 2.6, fontface = "bold") +
  guides(fill = "none") +
  ggtitle("Postal 3-digit") +
  theme_void()

pd <- ggplot() +
  geom_sf(data = gd, aes(fill = postal_district), color = "white", linewidth = 0.1) +
  geom_sf_text(data = labsd, aes(label = postal_district), size = 3, fontface = "bold") +
  guides(fill = "none") +
  ggtitle("Postal Districts (01–28)") +
  theme_void()

# Save a simple montage (stacked)
pngfile <- "output/preview_postal_layers.png"
png(pngfile, width = 1400, height = 2100, res = 140)
gridExtra::grid.arrange(p2, p3, pd, ncol = 1)
dev.off()
message("Wrote preview: ", pngfile)