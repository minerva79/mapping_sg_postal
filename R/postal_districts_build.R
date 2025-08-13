#!/usr/bin/env Rscript
# ----------------------------------------------------------------------
# Build postal DISTRICT (01–28) polygons for Singapore
#
# WHAT
#   - Repairs subzone geometries (SVY21) and collapses to one row per subzone
#   - Computes majority 2-digit sector (top2d) per subzone from point data
#   - Maps 2-digit sectors → URA postal districts via parsed PDF table
#   - Dissolves subzones to district polygons, generates label points
#   - Exports RFC7946 GeoJSON + labels and the crosswalk used
#
# INPUTS (repo-relative)
#   data/geo_subzones_clean.rds   # sf: subzone polygons, contains SUBZONE_KEY
#   data/sg_postal.rds            # sf: geocoded postal points; cols: postal_code, subzone
#   data/postal_districts.rds     # data.frame from PDF parse; cols: Postal_District, Postal_Sector, ...
#
# OUTPUTS
#   output/postal_districts.geojson                 # district polygons (EPSG:4326)
#   output/postal_districts_labels.gpkg (labels)    # label points for plotting
#   output/subzone_to_postal_district_crosswalk.csv # subzone → district (via top2d) with purity
#
# ASSUMPTIONS
#   - Subzone key column is SUBZONE_NO (change SUBZONE_KEY if needed)
#   - postal_code keeps leading zeros (treated as character)
#   - subzone values in points match the subzone key in polygons
# ----------------------------------------------------------------------

# --- deps ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(lwgeom)     # st_make_valid(), st_snap_to_grid()
  library(stringr)
  library(tidyr)
  library(ggplot2)
})
options(sf_max_plot = 1)

# --- config -------------------------------------------------------------
SUBZONE_KEY <- "SUBZONE_NO"   # change if your subzone name differs
CRS_M <- 3414                 # SVY21 (meters)
CRS_LL <- 4326                # WGS84 lon/lat
SNAP_M <- 1                   # snap-to-grid size in meters for de-slivering
PURITY_MIN <- 0.75            # info flag for weak top2d (adjust as needed)

# --- helpers ------------------------------------------------------------
fix_geoms <- function(x, crs_proj = CRS_M, snap_m = SNAP_M) {
  x %>%
    st_transform(crs_proj) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON") %>%
    lwgeom::st_snap_to_grid(size = snap_m) %>%
    st_buffer(0) %>%
    st_make_valid()
}

dissolve_by <- function(x, group_col) {
  x %>%
    group_by({{ group_col }}) %>%
    summarise(do_union = TRUE, .groups = "drop")
}

make_top2d <- function(points_sf) {
  points_sf %>%
    st_drop_geometry() %>%
    count(subzone, postal_2d, name = "n") %>%
    group_by(subzone) %>%
    mutate(total = sum(n), purity = n / total) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(subzone, postal_2d, purity, n, total)
}

# --- inputs -------------------------------------------------------------
basemap          <- readRDS("data/geo_subzones_clean.rds")
postal_agg       <- readRDS("data/sg_postal.rds")
postal_districts <- readRDS("data/postal_districts.rds")
dir.create("output", showWarnings = FALSE, recursive = TRUE)

stopifnot(inherits(basemap, "sf"), SUBZONE_KEY %in% names(basemap))

# ensure 2/3-digit strings (preserve leading zeros)
postal_agg <- postal_agg %>%
  mutate(
    postal_code = as.character(postal_code),
    postal_2d   = substr(postal_code, 1, 2),
    postal_3d   = substr(postal_code, 1, 3)
  )

# sector (2d) -> district lookup from parsed PDF
sector_to_district <- postal_districts %>%
  transmute(
    Postal_District = str_pad(as.character(Postal_District), 2, pad = "0"),
    Postal_Sector   = str_replace_all(Postal_Sector, "\\s", "")
  ) %>%
  separate_rows(Postal_Sector, sep = ",") %>%
  rename(postal_district = Postal_District, postal_2d = Postal_Sector) %>%
  mutate(postal_2d = str_pad(postal_2d, 2, pad = "0")) %>%
  distinct(postal_2d, postal_district)

# --- main pipeline ------------------------------------------------------
old_s2 <- sf_use_s2(FALSE)  # use GEOS for repairs/union

# 1) fix polygons + collapse to unique subzones (reuse basemap0 if exists)
if (!exists("basemap0")) {
  message("Repairing subzone geometries …")
  bm_fix   <- fix_geoms(basemap)
  message("Collapsing to unique subzones …")
  basemap0 <- dissolve_by(bm_fix, !!sym(SUBZONE_KEY))
}

# 2) majority 2-digit per subzone + purity (reuse if exists)
if (!exists("subzone_top2d")) {
  message("Computing majority (top2d) per subzone …")
  subzone_top2d <- make_top2d(postal_agg)
}

# info-only QA
message("Low-purity subzones (<", PURITY_MIN, "): ",
        nrow(filter(subzone_top2d, purity < PURITY_MIN)))

# 3) subzone -> top2d -> district
subzone_to_district <- subzone_top2d %>%
  mutate(postal_2d = str_pad(postal_2d, 2, pad = "0")) %>%
  left_join(sector_to_district, by = "postal_2d")

# 4) dissolve to district polygons (EPSG:4326)
message("Dissolving subzones to postal districts …")
geom_district <- basemap0 %>%
  inner_join(subzone_to_district, by = setNames("subzone", SUBZONE_KEY)) %>%
  dissolve_by(postal_district) %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(CRS_LL)

sf_use_s2(old_s2)  # restore s2

# --- minimal QA + quick repair pass ------------------------------------
cat("Invalid (geom_district): ", sum(!st_is_valid(geom_district)), "\n")
if (sum(!st_is_valid(geom_district)) > 0) {
  message("Running quick make-valid pass …")
  geom_district <- geom_district %>%
    st_transform(CRS_M) %>%
    st_make_valid() %>%
    lwgeom::st_snap_to_grid(size = 1) %>%
    st_buffer(0) %>%
    st_cast("MULTIPOLYGON") %>%
    st_transform(CRS_LL)
  cat("Invalid (geom_district) after fix: ",
      sum(!st_is_valid(geom_district)), "\n")
}

# count disjoint parts (informational)
geom_district_parts <- geom_district %>% st_transform(CRS_M) %>% st_cast("POLYGON")
geom_district_parts$area_m2 <- as.numeric(st_area(geom_district_parts))
parts_per_district <- geom_district_parts %>%
  st_drop_geometry() %>%
  count(postal_district, name = "n_parts") %>%
  arrange(desc(n_parts))
message("Districts with multiple disjoint parts: ",
        sum(parts_per_district$n_parts > 1))

# --- labels (largest piece per district) --------------------------------
district_labels <- geom_district_parts %>%
  group_by(postal_district) %>%
  slice_max(area_m2, n = 1, with_ties = FALSE) %>%
  st_point_on_surface() %>%
  st_transform(CRS_LL) %>%
  ungroup()

# --- minimal preview (optional) ----------------------------------------
ggplot() +
  geom_sf(data = geom_district, fill = NA, color = "grey30", linewidth = 0.2) +
  geom_sf_text(data = district_labels, aes(label = postal_district),
               size = 3.5, fontface = "bold") +
  theme_minimal()

# --- exports ------------------------------------------------------------
st_write(geom_district, "output/postal_districts.geojson",
         delete_dsn = TRUE, quiet = TRUE,
         layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=6"))

st_write(district_labels, "output/postal_districts_labels.gpkg",
         layer = "labels_district", delete_dsn = TRUE, quiet = TRUE)

write.csv(subzone_to_district,
          "output/subzone_to_postal_district_crosswalk.csv",
          row.names = FALSE)

message("Done. District outputs written to ./output/")
