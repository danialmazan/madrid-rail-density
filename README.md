# Madrid rail-station population density

This repository now contains **Phase 1 Catastro validation**, the approved
**Phase 2 regional building-component population model**, and the approved
**Phase 3 canonical urban-rail station dataset**, the approved **Phase 4
entrance-first catchment and coverage model**, plus the approved **Phase 5 QA
and sensitivity audit**, and the approved **Phase 6 bilingual website and
publication-data build**. Phase 7 adds entrance/platform analysis, district and
station comparisons, deterministic nearest-station hinterlands, self-hosted
analytical PMTiles, and a typed Vite/TypeScript explorer while retaining the
audited Phase 1–6 outputs and gates.

Phase 1b adds a dwelling-based station-distance sensitivity test solely to
validate the spatial support. It is not the final population or station model.
Phase 2 allocates the official 1 January 2025 INE population of every census
section to residential cadastral components in EPSG:25830.

## Current methodological contract

- A Spanish Catastro INSPIRE `Building` is treated as a parcel-level feature. Its geometry may be multipart.
- `numberOfDwellings`, `numberOfBuildingUnits`, `currentUse`, and `officialArea` are never treated as independently measured attributes of individual polygon components.
- Multipart geometries may only be exploded after the parcel-level weight has been allocated explicitly.
- Residential composition and candidate dwelling counts come from CAT Types 14 and 15, not from `currentUse` alone.
- CAT Type 13 unidades constructivas are not joined directly to INSPIRE `BuildingPart` geometries.

## Requirements

R 4.5 or later with `sf`, `terra`, `data.table`, `yaml`, `jsonlite`, `digest`,
and `ggplot2`; Node.js/npm; Python 3; and `tippecanoe`. Install the Phase 7 frontend
and Parquet dependencies once with:

```bash
npm install --prefix frontend
python3 -m pip install -r frontend/requirements-phase7.txt
```

The older R pipeline writes optional Parquet files when the R `arrow` package
is present. Phase 7's five public Parquet downloads are always produced by the
Python requirement above. Raw and intermediate data are excluded from version
control.

## Data acquisition

Public INSPIRE municipality packages are downloaded reproducibly:

```bash
make download-inspire
```

The independent INE 2021 housing controls and matching section geography are cached with:

```bash
make download-ine-validation
```

The configured Catastro host currently presents a certificate chain that is not accepted by the local command-line trust store. `config.yml` records the deliberate `curl --insecure` workaround; downloads are restricted to the official HTTPS host and immediately checksummed.

CAT alphanumeric files are free and exclude protected ownership and valuation fields, but Catastro requires certificate/Cl@ve authentication and licence acceptance. Download the urban CAT files for Madrid (`28900`), Pozuelo de Alarcón (`28115`), and Parla (`28106`) and place the untouched ZIP or CAT files under:

```text
data/raw/catastro/cat/
```

The parser understands the portal's nested provincial delivery, extracts only the three configured municipal `.CAT.gz` members, and records hashes for both delivery archives and extracted members. It does not retain or request ownership or valuation data.

## Run Phase 1

```bash
make phase1
```

To reproduce the approved Phase 1b extension after Phase 1 inputs are present:

```bash
make phase1b
```

To reproduce Phase 2 after placing the authenticated province-wide urban and
rural CAT deliveries under `data/raw/catastro/cat/`:

```bash
make phase2
```

To download the official CRTM station layers, build one record per connected
physical station/interchange, and run the Phase 3 gates:

```bash
make phase3
```

To calculate overlapping station catchments, non-duplicated regional/network
coverage, mode overlaps, densities and the station-point sensitivity scenario:

```bash
make phase4
```

To reproduce the population-weight, component-allocation, section-boundary,
entrance/reference-point and ±50 m threshold sensitivity audit:

```bash
make phase5
```

To build and validate the bilingual static article and its compact public
downloads from the audited outputs:

```bash
make phase6
```

To refresh the nine official CRTM Estaciones/Accesos/Andenes layers, rebuild
the normalized entrance/platform origins, calculate all Phase 7 analytics,
generate PMTiles and public CSV/JSON/Parquet downloads, build the frontend, run
unit and Playwright journeys, validate every analytical and regression gate,
and promote only a green build into `site/`:

```bash
make phase7
```

The production candidate is written to `site_phase7/`. Promotion is guarded by
`validation/current_phase7_decision.json`; the first successful local promotion
preserves the previous Phase 6 build in `site_phase6_archive/`.

## Website and deployment

Run the complete local site with one command:

```bash
cd frontend
npm start
```

The public site is deployed to
`https://danielalmazan.com/madrid-rail-density/` through GitHub Pages. The
building-component PMTiles archive is larger than GitHub's normal per-file Git
limit, so it is stored as the `components.pmtiles` asset on the `data-v1`
release. The Pages workflow downloads that pinned asset before running the Vite
production build. Replacing the analytical tile therefore requires uploading a
new verified release asset and updating the pinned release tag in the workflow.

Phase 7 exposes only `Entrance` and `Platform` as public distance methods.
Curves use 25 m steps through 1,500 m, histograms use 100 m bands plus an
overflow band, and means and medians use unsummarized component distances.
Resident geography filters do not prevent selection of a station outside the
boundary; station rankings filter stations by their central-point location but
retain all modelled Comunidad residents in each catchment. Catchments may
overlap, whereas regional coverage and hinterlands assign each resident once.

The generated article is in `site/`. Its radius control links the regional
coverage curve, station map, station profile and ranking across every 100 m
increment from 100 to 1,500 m. The interface defaults to nearest official
entrances, exposes the official-station-point sensitivity comparison, and uses
straight-line distance language throughout.

Phase 3 includes Metro de Madrid, Metro Ligero, Cercanías stations inside the
Comunidad de Madrid, and Tranvía de Parla under the declared `urban_rail`
scope. Modes remain separately filterable. Its preferred geometry is
entrance-first: later distances must use the nearest official entrance, with
an actual official CRTM station/stop point only where no entrance is published.
The official point is retained for every station as the fallback and the
station-point-only sensitivity scenario. Neither metric is a walking distance.

The Phase 2 downloader caches and checksums all 179 official municipal INSPIRE
archives plus the matching 2025 INE geography and population table. The urban
and rural CAT deliveries remain authenticated manual inputs. Positive INSPIRE
dwelling counts are used only as an explicitly flagged residual fallback for
parcels missing from both CAT domains.

Outputs include:

- `validation/tables/inspire_summary.csv`
- `validation/tables/use_dwelling_crosscheck.csv`
- `validation/tables/top_dwelling_outliers.csv`
- `validation/tables/cat_inspire_comparison.csv` when CAT is available
- `validation/tables/cat_rule_summary.csv` when CAT is available
- `validation/tables/ine_section_dwelling_comparison.csv`
- `validation/tables/disconnected_component_summary.csv`
- `validation/case_studies/case_parcels.csv`
- `validation/case_studies/*_components.png` and component-allocation CSV
- `validation/maps/*_diagnostic.png`
- `validation/phase1_report.md`
- `validation/phase1_status.json`
- `validation/case_studies/sec_visual_checks.csv`
- `data/raw/source_manifest.csv`
- `validation/phase1b/phase1b_report.md`
- `validation/phase1b/phase1b_status.json`
- `validation/phase1b/sensitivity_metrics.csv`
- `validation/phase1b/sensitivity_outlier_cells.csv`
- `validation/current_method_decision.json`
- `data/derived/building_population/component_population_2025.rds`
- `data/derived/building_population/section_reconciliation.csv`
- `validation/phase2/phase2_report.md`
- `validation/phase2/phase2_metrics.csv`
- `validation/phase2/high_residents_per_dwelling_sections.csv`
- `validation/current_phase2_decision.json`
- `data/derived/stations/canonical_stations.rds`
- `data/derived/stations/canonical_stations.csv`
- `data/derived/stations/station_services.csv`
- `data/derived/stations/station_entrances.rds`
- `data/derived/stations/station_entrances.csv`
- `data/derived/stations/station_distance_origins.rds`
- `data/derived/stations/station_distance_origins.csv`
- `data/derived/stations/station_reference_points.rds`
- `data/derived/stations/station_reference_points.csv`
- `data/interim/phase3/station_source_records.csv`
- `validation/phase3/phase3_report.md`
- `validation/phase3/phase3_gate_checks.csv`
- `validation/phase3/entrance_coverage_by_mode.csv`
- `validation/phase3/surface_stop_fallbacks.csv`
- `validation/phase3/entrance_reference_distance_diagnostics.csv`
- `validation/phase3/entrance_coverage_map.png`
- `validation/phase3/canonical_station_map.png`
- `validation/current_phase3_decision.json`
- `data/derived/catchments/station_catchments_2025.csv`
- `data/derived/catchments/station_catchment_geometries_2025.rds`
- `data/derived/catchments/regional_network_coverage_2025.csv`
- `data/derived/catchments/network_overlap_2025.csv`
- `data/derived/catchments/station_catchment_sensitivity_2025.csv`
- `data/derived/catchments/regional_coverage_sensitivity_2025.csv`
- `validation/phase4/phase4_report.md`
- `validation/phase4/phase4_gate_checks.csv`
- `validation/phase4/regional_coverage_curve.png`
- `validation/phase4/station_800m_sensitivity.png`
- `validation/current_phase4_decision.json`
- `data/derived/sensitivity/station_population_method_sensitivity_2025.csv`
- `data/derived/sensitivity/regional_population_method_sensitivity_2025.csv`
- `data/derived/sensitivity/station_threshold_band_comparison_2025.csv`
- `data/derived/sensitivity/entrance_reference_sensitivity_summary_2025.csv`
- `validation/phase5/phase5_report.md`
- `validation/phase5/methodology_ledger.md`
- `validation/phase5/phase5_gate_checks.csv`
- `validation/phase5/population_method_rank_sensitivity.png`
- `validation/phase5/threshold_band_sensitivity.png`
- `validation/current_phase5_decision.json`
- `data/derived/stations/station_platforms.csv`
- `data/derived/stations/station_origins_phase7.csv`
- `data/derived/phase7/coverage_curves_2025.csv`
- `data/derived/phase7/distance_histograms_2025.csv`
- `data/derived/phase7/area_distance_metrics_2025.csv`
- `data/derived/phase7/station_catchment_curves_2025.csv`
- `data/derived/phase7/station_hinterlands_2025.csv`
- `frontend/public/tiles/components.pmtiles`
- `frontend/public/tiles/surface.pmtiles`
- `frontend/public/tiles/stations.pmtiles`
- `validation/phase7/phase7_report.md`
- `validation/phase7/pmtiles_validation.json`
- `validation/current_phase7_decision.json`

Phase 1 initially concluded **NO_GO_REDESIGN** because 17.93% of prototype
dwellings belong to parcels with separated physical footprints. Phase 1b has
now resolved that gate as **GO_MODIFIED_COMPONENT_ALLOCATION**: distribute each
CAT-derived parcel dwelling total across connected footprints according to
BuildingPart footprint area × floors above ground, conservatively excluding
only tiny ancillary components. Footprint and preferred volume allocations are
within 2% for 99.32% of complete eligible station-radius cells. This remains a
modeled allocation—not an observed building-level dwelling count—and the 23
outlier cells are retained for later QA.

Phase 2 concludes **GO_PHASE2_COMPLETE**. It allocates 7,113,886 residents over
4,518 sections to 644,613 residential components on 502,598 cadastral parcels.
All section totals reconcile; no section-level fallback is needed. All
fallback-derived dwelling equivalents account for 0.065% of total weight, and a true
boundary-area split changes 0.0399% of regional population allocation in total
variation terms. Six sections with more than five residents per derived
dwelling remain explicitly flagged for later sensitivity analysis rather than
being hidden by the reconciliation.

Phase 3 concludes **GO_PHASE3_ENTRANCE_FIRST_WITH_SURFACE_FALLBACK**. The station universe contains 357
physical stations: 241 Metro, 37 Metro Ligero, 92 in-region Cercanías, and 19
Tranvía de Parla stops, with 32 cross-mode stations merged. Nineteen
out-of-region Cercanías records and two unnamed incomplete Metro records are
excluded. An explicit source-data exception keeps Callao and Santo Domingo
separate despite their shared CRTM interchange code.

The official access layers provide 1,020 in-scope entrance points. Coverage is
complete for Metro and in-region Cercanías, and every underground Metro Ligero
complex has access geometry. Literal access records cover only 14 of 37 Metro
Ligero stations and none of the 19 Tranvía de Parla stops; all uncovered cases
are surface stops. Consequently, 317 stations use nearest-entrance geometry
and 40 use explicitly flagged official surface-stop point fallbacks. All 30
revised Phase 3 gates pass.

Phase 4 concludes **GO_PHASE4_COMPLETE**. Catchments are calculated every 100 m
from 100–1,500 m in EPSG:25830. Station catchments may overlap; regional and
network coverage use each component's nearest eligible origin and therefore do
not double-count residents. The preferred method uses nearest official entrance
with the approved surface-stop fallback, while an official-station-point result
is retained throughout as sensitivity analysis. All 24 Phase 4 gates pass,
including direct recomputation checks for five contrasting stations.

Phase 5 concludes **GO_PHASE5_PUBLICATION_READY_WITH_CAVEATS**. Dwelling,
residential-area, component-placement and census-boundary alternatives leave
station rankings and regional coverage highly stable. Entrance geometry and
the exact radius remain materially consequential, especially below 400 m;
the website must therefore use entrance-first results, retain the station-point
sensitivity scenario, show cumulative curves, and avoid presenting close
threshold rankings as exact. All 20 Phase 5 gates pass, and the methodology
ledger records every retained caveat and publication requirement.

Without CAT inputs, the run completes the public INSPIRE checks but returns `BLOCKED_CAT_REQUIRED`; this is intentional and prevents a false go decision.

## Sources

- Dirección General del Catastro, [INSPIRE dataset specification](https://www.catastro.hacienda.gob.es/webinspire/documentos/Conjuntos%20de%20datos.pdf)
- Dirección General del Catastro, [INSPIRE services](https://www.catastro.hacienda.gob.es/webinspire/index.html)
- Dirección General del Catastro, [CAT fixed-width specification](https://www.catastro.hacienda.gob.es/documentos/formatos_intercambio/catastro_fin_cat_2006.pdf)
- Dirección General del Catastro, [CAT FAQ](https://www.catastro.hacienda.gob.es/documentos/preguntas_frecuentes_formato_CAT.pdf)
- Regueira et al., [Identification of dwellings in Catastro, Andalucía case](https://www.catastro.hacienda.gob.es/documentos/publicaciones/ct/ct99/catastro%2099%20accesible.PDF)
- Consorcio Regional de Transportes de Madrid, [official open-data portal](https://datos.crtm.es/)
- CRTM, [open-data licence](https://www.crtm.es/licencia-de-uso)

## Browser analytics opt-out

The HTML entry loads Cloudflare Web Analytics only when `localStorage.getItem("exclude_cf_analytics") !== "1"`. To exclude this browser profile, run `localStorage.setItem("exclude_cf_analytics", "1"); location.reload();` in the site console. To restore tracking, run `localStorage.removeItem("exclude_cf_analytics"); location.reload();`. The preference persists for the exact origin and is shared by all project paths under `https://danielalmazan.com`. Other browsers and profiles retain normal tracking. Reload existing tabs after changing it. No visible settings or other analytics integrations are changed.

The preference check runs before any beacon request. If browser storage is unavailable, normal analytics continues without a storage exception. Cloudflare must use manual snippet installation; automatically injected scripts operate outside this browser preference check.
