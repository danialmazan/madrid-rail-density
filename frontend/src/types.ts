export const PointMethod = { Entrance:'entrance', Platform:'platform' } as const;
export type PointMethod = typeof PointMethod[keyof typeof PointMethod];
export type CoverageMethod = PointMethod | 'compare';
export const Geography = { Region:'region', City:'city' } as const;
export type Geography = typeof Geography[keyof typeof Geography];
export const RailView = { All:'all', Nested:'nested' } as const;
export type RailView = typeof RailView[keyof typeof RailView];
export const ComparisonEntity = { Station:'station', District:'district' } as const;
export type ComparisonEntity = typeof ComparisonEntity[keyof typeof ComparisonEntity];
export const StationMetric = { Radius:'radius', Hinterland:'hinterland' } as const;
export type StationMetric = typeof StationMetric[keyof typeof StationMetric];
export type Measure = 'share' | 'population';

export interface Meta { version:string; population_reference_date:string; catastro_snapshot:string; station_snapshot:string; population:number; station_count:number; entrance_count:number; platform_count:number; fallback_station_count:number; curve_step_m:number; histogram_step_m:number; maximum_distance_m:number; default_radius_m:number; distance_type:string; crs:string }
export interface Station { station_id:string; station_name:string; modes:string; lines:string; municipality_code:string; municipality_name:string; longitude:number; latitude:number; official_entrance_count:number; official_platform_count:number; has_official_entrance:boolean; entrance_fallback:boolean }
export interface District { district_code:string; district_id:string; district_name:string }
export interface CoverageRow { geography_type:string; geography_id:string; geography_name:string; point_method:PointMethod; rail_network:string; radius_m:number; population:number; population_share:number; geography_population:number }
export interface HistogramRow { geography_type:string; geography_id:string; geography_name:string; point_method:PointMethod; rail_network:string; band_start_m:number; band_end_m:number|null; overflow:boolean; population:number; population_share:number; geography_population:number }
export interface AreaMetric { geography_type:string; geography_id:string; geography_name:string; point_method:PointMethod; rail_network:string; population:number; median_distance_m:number; mean_distance_m:number }
export interface StationCurve { station_id:string; radius_m:number; point_method:PointMethod; population:number; residential_components:number; catchment_area_km2:number; population_density_per_km2:number|null; station_name:string; modes:string; lines:string; municipality_code:string; municipality_name:string; official_entrance_count:number; entrance_fallback:boolean; longitude:number; latitude:number; station_geography:string; rank_region:number; rank_city:number|null }
export interface Hinterland { station_id:string; point_method:PointMethod; nearest_station_population:number; component_count:number; median_distance_m:number; mean_distance_m:number; station_name:string; modes:string; lines:string; municipality_code:string; municipality_name:string; official_entrance_count:number; entrance_fallback:boolean; longitude:number; latitude:number; rank_region:number; station_geography:string; rank_city:number|null }
export interface RailData { meta:Meta; map:{region_bbox:number[];city_bbox:number[];basemap_style:string}; districts:District[]; stations:Station[]; coverage_curves:CoverageRow[]; distance_histograms:HistogramRow[]; area_metrics:AreaMetric[]; station_curves:StationCurve[]; hinterlands:Hinterland[] }

export interface AppState {
  language:'en'|'es'; geography:Geography; coverageMethod:CoverageMethod; areaMeasure:Measure; railView:RailView;
  mapMethod:PointMethod; districtMethod:PointMethod; districtMeasure:Measure; districtRail:'all'|'mode'; districtIds:string[];
  stationMethod:PointMethod; stationMeasure:'population'|'density'; stationNames:string[];
  rankingMetric:StationMetric; radius:number; modeFilter:string; direction:'top'|'bottom'; selectedStationId:string;
}
