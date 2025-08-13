# SG Postal Sectors & Districts (R)

Reproducible scripts to build Singapore **postal sector polygons** (2-digit & 3-digit) and **postal districts (01–28)** from subzone geometries plus geocoded postal points. Outputs are saved as GeoJSON (RFC 7946) and label layers; a vanity script renders a **PNG montage** with `ggplot2`.
**Note:** This repo intentionally stops at PNG output (no Leaflet/HTML).

---

## Repository layout

```
Project
├─ data/
│  ├─ geo_subzones_clean.rds
│  ├─ List_of_Postal_Districts.pdf
│  ├─ postal_districts.rds
│  └─ sg_postal.rds
├─ output/
│  ├─ postal_2d.geojson
│  ├─ postal_2d_labels.gpkg
│  ├─ postal_3d.geojson
│  ├─ postal_3d_labels.gpkg
│  ├─ postal_districts.geojson
│  ├─ postal_districts_labels.gpkg
│  ├─ preview_postal_layers.png
│  ├─ subzone_to_postal_district_crosswalk.csv
│  ├─ subzone_to_top2d_with_purity.csv
│  └─ subzone_to_top3d_with_purity.csv
├─ R/
│  ├─ postal_districts.R
│  ├─ postal_districts_build.R
│  ├─ postal_layers_vanity.R
│  ├─ postal_sectors_2d.R
│  └─ postal_sectors_3d.R
└─ sg_mapping.Rproj
```

---

## Data sources

* **Subzones** — `data/geo_subzones_clean.rds` (sf): cleaned subzone polygons.
* **Postal points** — `data/sg_postal.rds` (sf): geocoded SG postal codes with `postal_code` and `subzone`.
* **URA postal districts PDF** — `data/List_of_Postal_Districts.pdf`, parsed by `R/postal_districts.R` into `data/postal_districts.rds`.

> Tip: keep `postal_code` as **character** to preserve leading zeros.

---

## Requirements

* R ≥ 4.2
* System libs: **GDAL**, **GEOS**, **PROJ** (for `sf`/`lwgeom`)
* R packages: `sf`, `lwgeom`, `dplyr`, `tidyr`, `stringr`, `readr`, `ggplot2`, `pdftools`, `gridExtra`

Install packages:

```r
install.packages(c(
  "sf","lwgeom","dplyr","tidyr","stringr","readr","ggplot2","pdftools","gridExtra"
))
```

---

## How to run (end-to-end)

From the repo root:

1. **Parse URA postal districts PDF** → RDS

   ```bash
   Rscript R/postal_districts.R
   ```

2. **Build 2-digit sector polygons** (+ labels, crosswalk)

   ```bash
   Rscript R/postal_sectors_2d.R
   ```

3. **Build 3-digit sector polygons** (+ labels, crosswalk)

   ```bash
   Rscript R/postal_sectors_3d.R
   ```

4. **Build district polygons** (sector → district mapping)

   ```bash
   Rscript R/postal_districts_build.R
   ```

5. **Render PNG montage (ggplot only)**

   ```bash
   Rscript R/postal_layers_vanity.R
   ```

   Produces `output/preview_postal_layers.png`.

---

## What each script does

* `R/postal_districts.R`
  Parses `data/List_of_Postal_Districts.pdf` into `data/postal_districts.rds` and optional CSVs (one row per district; optional **expanded** one row per 2-digit sector).

* `R/postal_sectors_2d.R`
  Repairs subzone geometry in **SVY21 (EPSG:3414)**, reduces to one row per subzone, computes majority **2-digit** per subzone (with **purity**), dissolves to **2-digit** polygons, creates label points.
  Outputs: `postal_2d.geojson`, `postal_2d_labels.gpkg` (layer `labels`), `subzone_to_top2d_with_purity.csv`.

* `R/postal_sectors_3d.R`
  Same as above for **3-digit** sectors.
  Outputs: `postal_3d.geojson`, `postal_3d_labels.gpkg` (layer `labels_3d`), `subzone_to_top3d_with_purity.csv`.

* `R/postal_districts_build.R`
  Maps **2-digit sectors → postal districts (01–28)** using the parsed URA table; dissolves to district polygons; builds labels.
  Outputs: `postal_districts.geojson`, `postal_districts_labels.gpkg` (layer `labels_district`), `subzone_to_postal_district_crosswalk.csv`.

* `R/postal_layers_vanity.R`
  Loads the three polygon layers + labels and writes **`output/preview_postal_layers.png`** via `ggplot2`. (Leaflet code is intentionally omitted.)

---

## CRS & geometry notes

* Geometry repair & dissolves are done in **SVY21 / EPSG:3414** (meters).
  Final GeoJSON exports are **EPSG:4326** (WGS84) for web compatibility.
* If dissolves produce invalid geometries, scripts include a minimal **make-valid** pass (`st_make_valid()` → `st_snap_to_grid()` → `st_buffer(0)`).

---

## Purity & QA

Majority assignments record a **purity** share (e.g., 0.92 ⇒ 92% of postal points in that subzone belong to the chosen sector). Use:

* `output/subzone_to_top2d_with_purity.csv`
* `output/subzone_to_top3d_with_purity.csv`

to flag edge subzones. For analytics, consider **weighted** aggregation by sector shares instead of hard assignment.

---

## Repro tips

* All paths are repo-relative; outputs go to `output/`.
* To regenerate everything, delete `output/` and re-run the scripts in order.
* For cleaner diffs, commit the CSV crosswalks and the PNG preview; GeoJSONs can be large.

---

## License & attribution

* Code: GPL3.
* Postal district definitions: URA “List of Postal Districts”.
* Underlying postal points/subzones: see `data/` provenance in your environment.

---

## Contact

Questions, issues, or suggestions — please open a GitHub issue in this repo.
