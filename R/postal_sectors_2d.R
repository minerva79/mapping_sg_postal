#!/usr/bin/env Rscript
# ----------------------------------------------------------------------
# Build 2-digit postal sector polygons for Singapore
#
# WHAT
#   - Repairs subzone geometries (SVY21), canonicalizes one row per subzone,
#   - Computes majority 2-digit sector (top2d) per subzone from point data,
#   - Dissolves subzones to 2-digit polygons, generates label points,
#   - Exports GeoJSON (RFC7946) + labels and a crosswalk with purity.
#
# INPUTS (repo-relative)
#   data/geo_subzones_clean.rds   # sf: subzone polygons, must contain SUBZONE_KEY
#   data/sg_postal.rds            # sf: geocoded postal points with 'postal_code' and 'subzone'
#
# OUTPUTS
#   output/postal_2d.geojson                # dissolved 2-digit polygons (EPSG:4326)
#   output/postal_2d_labels.gpkg (labels)   # label points for plotting
#   output/subzone_to_top2d_with_purity.csv # subzone->2d crosswalk with counts/purity
#
# ASSUMPTIONS
#   - Subzone key column is SUBZONE_NO (change SUBZONE_KEY if needed).
#   - 'postal_code' is convertible to character and keeps leading zeros.
#   - 'subzone' in points matches the subzone key values.
# ----------------------------------------------------------------------

# --- deps ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(lwgeom)    # st_make_valid(), st_snap_to_grid()
  library(ggplot2)
})
options(sf_max_plot = 1) # keep plots light

# --- config -------------------------------------------------------------
SUBZONE_KEY <- "SUBZONE_NO"   # change if your subzone name differs
CRS_M <- 3414                 # SVY21 (meters)
CRS_LL <- 4326                # WGS84 lon/lat
SNAP_M <- 1                   # snap-to-grid size in meters for de-slivering

# --- helpers ------------------------------------------------------------
# Normalize/fix polygons in a projected CRS for robust dissolves
fix_geoms <- function(x, crs_proj = CRS_M, snap_m = SNAP_M) {
  x %>%
    st_transform(crs_proj) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON") %>%
    lwgeom::st_snap_to_grid(size = snap_m) %>%
    st_buffer(0) %>%                  # close micro gaps / re-node
    st_make_valid()
}

# Thin wrapper around sf::summarise(do_union=TRUE)
dissolve_by <- function(x, group_col) {
  x %>%
    group_by({{ group_col }}) %>%
    summarise(do_union = TRUE, .groups = "drop")
}

# Majority sector per subzone + purity (share of winning sector)
make_top2d <- function(points_sf) {
  points_sf %>%
    st_drop_geometry() %>%
    count(subzone, postal_2d, name = "n") %>%
    group_by(subzone) %>%
    mutate(total = sum(n), share = n / total) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(subzone, postal_2d, purity = share, n = n, total = total)
}

# --- paths --------------------------------------------------------------
basemap    <- readRDS("data/geo_subzones_clean.rds")
postal_agg <- readRDS("data/sg_postal.rds")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# --- prep ---------------------------------------------------------------
stopifnot(inherits(basemap, "sf"), SUBZONE_KEY %in% names(basemap))

# Keep postal codes as character (preserve leading zeros) and derive 2d/3d
postal_agg <- postal_agg %>%
  mutate(
    postal_code = as.character(postal_code),
    postal_2d   = substr(postal_code, 1, 2),
    postal_3d   = substr(postal_code, 1, 3)
  )

# Use GEOS (s2 off) for repair/union; we’ll restore afterwards
old_s2 <- sf_use_s2(FALSE)

# 1) Clean polygons in projected CRS (SVY21)
message("Repairing subzone geometries …")
bm_fix <- fix_geoms(basemap)

# 2) Canonicalize: one row per subzone
message("Collapsing to unique subzones …")
basemap0 <- dissolve_by(bm_fix, !!sym(SUBZONE_KEY))

# 3) Majority 2-digit sector per subzone + purity
message("Computing majority (top2d) per subzone …")
subzone_top2d <- make_top2d(postal_agg)

# 3a) Minimal QA: subzones without any mapping (informational)
missing_map <- anti_join(
  basemap0 %>% st_drop_geometry() %>% select(!!sym(SUBZONE_KEY)),
  subzone_top2d,
  by = setNames("subzone", SUBZONE_KEY)
)
if (nrow(missing_map)) {
  message("Note: ", nrow(missing_map), " subzone(s) have no top2d mapping.")
}

# 3b) Minimal QA: list low-purity subzones (tweak threshold as needed)
low_purity <- subzone_top2d %>% filter(purity < 0.75)
message("Low-purity subzones (<0.75): ", nrow(low_purity))

# 4) Dissolve subzones to 2-digit sector polygons (EPSG:4326)
message("Dissolving to 2-digit sector polygons …")
geom_2d <- basemap0 %>%
  inner_join(subzone_top2d, by = setNames("subzone", SUBZONE_KEY)) %>%
  dissolve_by(postal_2d) %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(CRS_LL)

# Restore s2 setting
sf_use_s2(old_s2)

# --- quick QA -----------------------------------------------------------
cat("Invalid (subzones_clean): ", sum(!st_is_valid(basemap0)), "\n")
cat("Invalid (postal_2d)     : ", sum(!st_is_valid(geom_2d)),  "\n")
print(table(st_geometry_type(geom_2d)))

# Non-contiguity: count parts per sector (informational)
geom_2d_parts <- geom_2d %>% st_transform(CRS_M) %>% st_cast("POLYGON")
geom_2d_parts$area_m2 <- as.numeric(st_area(geom_2d_parts))
parts_per_2d <- geom_2d_parts %>%
  st_drop_geometry() %>%
  count(postal_2d, name = "n_parts") %>%
  arrange(desc(n_parts))
message("Sectors with multiple disjoint parts: ",
        sum(parts_per_2d$n_parts > 1))

# --- label points (largest piece per sector) ----------------------------
geom_2d_labels <- geom_2d_parts %>%
  group_by(postal_2d) %>%
  slice_max(area_m2, n = 1, with_ties = FALSE) %>%
  st_point_on_surface() %>%
  st_transform(CRS_LL) %>%
  ungroup()

# --- minimal preview plot (optional) ------------------------------------
ggplot() +
  geom_sf(data = geom_2d, fill = NA, color = "grey30", linewidth = 0.2) +
  geom_sf_text(data = geom_2d_labels, aes(label = postal_2d),
               size = 3.5, fontface = "bold") +
  theme_minimal()

# --- exports ------------------------------------------------------------
# GeoJSON (RFC 7946) for web/Leaflet/Mapbox
st_write(geom_2d, "output/postal_2d.geojson",
         delete_dsn = TRUE, quiet = TRUE,
         layer_options = c("RFC7946=YES","COORDINATE_PRECISION=6"))

# Label points (GeoPackage), and the crosswalk for downstream joins
st_write(geom_2d_labels, "output/postal_2d_labels.gpkg",
         layer = "labels", delete_dsn = TRUE, quiet = TRUE)

write.csv(subzone_top2d, "output/subzone_to_top2d_with_purity.csv", row.names = FALSE)

message("Done. Outputs written to ./output/")
