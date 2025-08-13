#!/usr/bin/env Rscript
# ----------------------------------------------------------------------
# Build 3-digit postal sector polygons for Singapore
#
# WHAT
#   - Repairs subzone geometries (SVY21), canonicalizes one row per subzone,
#   - Computes majority 3-digit sector (top3d) per subzone from point data,
#   - Dissolves subzones to 3-digit polygons, generates label points,
#   - Exports GeoJSON (RFC7946) + labels and a crosswalk with purity.
#
# INPUTS (repo-relative)
#   data/geo_subzones_clean.rds   # sf: subzone polygons, must contain SUBZONE_KEY
#   data/sg_postal.rds            # sf: geocoded postal points with 'postal_code' and 'subzone'
#
# OUTPUTS
#   output/postal_3d.geojson                 # dissolved 3-digit polygons (EPSG:4326)
#   output/postal_3d_labels.gpkg (labels)    # label points for plotting
#   output/subzone_to_top3d_with_purity.csv  # subzone->3d crosswalk with counts/purity
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

# --- config -------------------------------------------------------------
SUBZONE_KEY <- "SUBZONE_NO"   # change if your subzone key differs
CRS_M <- 3414                 # SVY21 (meters)
CRS_LL <- 4326                # WGS84 lon/lat
SNAP_M <- 1                   # snap-to-grid size (m)
PURITY_MIN <- 0.95            # info flag for low-majority subzones

# --- helpers (reused) ---------------------------------------------------
if (!exists("fix_geoms")) {
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
}
if (!exists("dissolve_by")) {
  dissolve_by <- function(x, group_col) {
    x %>%
      group_by({{ group_col }}) %>%
      summarise(do_union = TRUE, .groups = "drop")
  }
}

make_top3d <- function(points_sf) {
  points_sf %>%
    st_drop_geometry() %>%
    count(subzone, postal_3d, name = "n") %>%
    group_by(subzone) %>%
    mutate(total = sum(n), purity = n / total) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(subzone, postal_3d, purity, n, total)
}

# --- inputs -------------------------------------------------------------
basemap    <- readRDS("data/geo_subzones_clean.rds")
postal_agg <- readRDS("data/sg_postal.rds")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

stopifnot(inherits(basemap, "sf"), SUBZONE_KEY %in% names(basemap))

# Ensure 3-digit codes exist (preserve leading zeroes)
postal_agg <- postal_agg %>%
  mutate(
    postal_code = as.character(postal_code),
    postal_2d   = substr(postal_code, 1, 2),  # kept for symmetry with 2d pipelines
    postal_3d   = substr(postal_code, 1, 3)
  )

# --- main pipeline (3-digit) -------------------------------------------
old_s2 <- sf_use_s2(FALSE)   # GEOS for repair/union

# 1) Clean subzone polygons (reuse basemap0 if already in session)
if (!exists("basemap0")) {
  message("Repairing subzone geometries …")
  bm_fix   <- fix_geoms(basemap)
  message("Collapsing to unique subzones …")
  basemap0 <- dissolve_by(bm_fix, !!sym(SUBZONE_KEY))
}

# 2) Majority 3-digit mapping (+ purity)
message("Computing majority (top3d) per subzone …")
subzone_top3d <- make_top3d(postal_agg)

# Minimal QA (informational only)
missing_map3d <- anti_join(
  basemap0 %>% st_drop_geometry() %>% select(!!sym(SUBZONE_KEY)),
  subzone_top3d,
  by = setNames("subzone", SUBZONE_KEY)
)
message("Subzones without 3-digit mapping: ", nrow(missing_map3d))

low_purity_3d <- subzone_top3d %>% filter(purity < PURITY_MIN)
message("Low-purity 3-digit subzones (<", PURITY_MIN, "): ", nrow(low_purity_3d))

# 3) Dissolve to 3-digit polygons (EPSG:4326)
message("Dissolving to 3-digit polygons …")
geom_3d <- basemap0 %>%
  inner_join(subzone_top3d, by = setNames("subzone", SUBZONE_KEY)) %>%
  dissolve_by(postal_3d) %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(CRS_LL)

sf_use_s2(old_s2)

# --- QA & labels --------------------------------------------------------
cat("Invalid (postal_3d): ", sum(!st_is_valid(geom_3d)), "\n")
print(table(st_geometry_type(geom_3d)))

# Count disjoint parts per 3-digit (informational)
geom_3d_parts <- geom_3d %>% st_transform(CRS_M) %>% st_cast("POLYGON")
geom_3d_parts$area_m2 <- as.numeric(st_area(geom_3d_parts))
parts_per_3d <- geom_3d_parts %>%
  st_drop_geometry() %>%
  count(postal_3d, name = "n_parts") %>%
  arrange(desc(n_parts))
message("3-digit codes with multiple disjoint parts: ",
        sum(parts_per_3d$n_parts > 1))

# Label point: largest piece of each 3-digit
geom_3d_labels <- geom_3d_parts %>%
  group_by(postal_3d) %>%
  slice_max(area_m2, n = 1, with_ties = FALSE) %>%
  st_point_on_surface() %>%
  st_transform(CRS_LL) %>%
  ungroup()

# --- quick plot (optional) ----------------------------------------------
ggplot() +
  geom_sf(data = geom_3d, fill = NA, color = "grey30", linewidth = 0.2) +
  geom_sf_text(data = geom_3d_labels, aes(label = postal_3d),
               size = 3, fontface = "bold") +
  theme_minimal()

# --- exports ------------------------------------------------------------
st_write(geom_3d, "output/postal_3d.geojson",
         delete_dsn = TRUE, quiet = TRUE,
         layer_options = c("RFC7946=YES","COORDINATE_PRECISION=6"))

st_write(geom_3d_labels, "output/postal_3d_labels.gpkg",
         layer = "labels_3d", delete_dsn = TRUE, quiet = TRUE)

write.csv(subzone_top3d, "output/subzone_to_top3d_with_purity.csv", row.names = FALSE)

message("Done. 3-digit outputs written to ./output/")
