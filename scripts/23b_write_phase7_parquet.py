#!/usr/bin/env python3
"""Write the compact public Phase 7 tables as Parquet with Zstandard compression."""
from pathlib import Path

import pyarrow.csv as csv
import pyarrow.parquet as parquet

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "data" / "derived" / "phase7"
DEST = ROOT / "frontend" / "public" / "downloads"
TABLES = {
    "coverage_curves_2025.csv": "coverage-curves-2025.parquet",
    "distance_histograms_2025.csv": "distance-histograms-2025.parquet",
    "area_distance_metrics_2025.csv": "area-distance-metrics-2025.parquet",
    "station_catchment_curves_2025.csv": "station-catchment-curves-2025.parquet",
    "station_hinterlands_2025.csv": "station-hinterlands-2025.parquet",
}

DEST.mkdir(parents=True, exist_ok=True)
for source_name, destination_name in TABLES.items():
    table = csv.read_csv(SOURCE / source_name)
    parquet.write_table(table, DEST / destination_name, compression="zstd")
    print(f"wrote {destination_name}: {table.num_rows:,} rows")
