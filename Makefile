.PHONY: phase1 phase1b phase2 phase3 phase4 phase5 phase6 phase7 download-inspire download-ine-validation download-prototype-stations download-phase2-sources download-phase3-stations parse-catastro parse-regional-catastro parse-regional-inspire prepare-ine-2025 build-regional-components allocate-population validate-phase2 build-stations validate-phase3 calculate-catchments validate-phase4 calculate-sensitivity validate-phase5 build-site validate-site calculate-phase7 build-phase7-tiles build-phase7-site validate-phase7 promote-phase7 extract-case-parts prepare-prototype-components case-studies validate validate-phase1b test clean-derived

R := Rscript

phase1: download-inspire download-ine-validation parse-catastro extract-case-parts case-studies validate test

phase1b: phase1 download-prototype-stations prepare-prototype-components validate-phase1b test

phase2: download-phase2-sources parse-regional-catastro parse-regional-inspire prepare-ine-2025 build-regional-components allocate-population validate-phase2 test

phase3: download-phase3-stations build-stations validate-phase3 test

phase4: phase3 calculate-catchments validate-phase4 test

phase5: phase4 calculate-sensitivity validate-phase5 test

phase6: phase5 build-site validate-site test

phase7: download-phase3-stations build-stations validate-phase3
	$(R) scripts/22_calculate_phase7.R
	$(R) scripts/23_build_phase7_payload.R
	python3 scripts/23b_write_phase7_parquet.py
	$(R) scripts/24_build_phase7_tiles.R
	npm run build --prefix frontend
	npm run test --prefix frontend
	npm run test:e2e --prefix frontend
	$(R) scripts/25_validate_phase7.R
	$(R) tests/run_tests.R
	$(R) scripts/26_promote_phase7.R

calculate-phase7:
	$(R) scripts/22_calculate_phase7.R

build-phase7-tiles:
	$(R) scripts/24_build_phase7_tiles.R

build-phase7-site:
	$(R) scripts/23_build_phase7_payload.R
	python3 scripts/23b_write_phase7_parquet.py
	npm run build --prefix frontend

validate-phase7:
	$(R) scripts/25_validate_phase7.R

promote-phase7:
	$(R) scripts/26_promote_phase7.R

download-inspire:
	$(R) scripts/01_download_inspire.R

download-ine-validation:
	$(R) scripts/01b_download_ine_validation.R

download-prototype-stations:
	$(R) scripts/01c_download_prototype_stations.R

download-phase2-sources:
	$(R) scripts/06_download_phase2_sources.R

download-phase3-stations:
	$(R) scripts/13_download_phase3_stations.R

parse-catastro:
	$(R) scripts/02_parse_catastro.R

parse-regional-catastro:
	$(R) scripts/07_parse_regional_catastro.R

parse-regional-inspire:
	$(R) scripts/08_parse_regional_inspire.R

prepare-ine-2025:
	$(R) scripts/09_prepare_ine_2025.R

build-regional-components:
	$(R) scripts/10_build_regional_components.R

allocate-population:
	$(R) scripts/11_allocate_population.R

validate-phase2:
	$(R) scripts/12_validate_phase2.R

build-stations:
	$(R) scripts/14_build_station_dataset.R

validate-phase3:
	$(R) scripts/15_validate_phase3.R

calculate-catchments:
	$(R) scripts/16_calculate_catchments.R

validate-phase4:
	$(R) scripts/17_validate_phase4.R

calculate-sensitivity:
	$(R) scripts/18_calculate_phase5_sensitivity.R

validate-phase5:
	$(R) scripts/19_validate_phase5.R

build-site:
	$(R) scripts/20_build_site_assets.R

validate-site:
	$(R) scripts/21_validate_phase6.R

extract-case-parts:
	$(R) scripts/02b_extract_case_building_parts.R

prepare-prototype-components:
	$(R) scripts/02c_prepare_prototype_components.R

case-studies:
	$(R) scripts/03_prepare_case_studies.R

validate:
	$(R) scripts/04_validate_catastro.R

validate-phase1b:
	$(R) scripts/05_validate_component_sensitivity.R

test:
	$(R) tests/run_tests.R

clean-derived:
	$(R) -e 'unlink(c("data/interim/catastro_rds", "validation/tables", "validation/case_studies"), recursive = TRUE); dir.create("data/interim/catastro_rds", recursive = TRUE); dir.create("validation/tables", recursive = TRUE); dir.create("validation/case_studies", recursive = TRUE)'
