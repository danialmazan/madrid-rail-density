component_station_distances <- function(points, origins, maximum_distance_m, progress_label = "distance join") {
  hits <- sf::st_is_within_distance(origins, points, dist = maximum_distance_m, sparse = TRUE)
  groups <- split(seq_len(nrow(origins)), origins$station_id)
  result <- vector("list", length(groups))
  station_ids <- names(groups)
  for (i in seq_along(groups)) {
    origin_index <- groups[[i]]
    lengths_i <- lengths(hits[origin_index])
    if (!sum(lengths_i)) next
    pair_origin <- rep(origin_index, lengths_i)
    pair_component <- unlist(hits[origin_index], use.names = FALSE)
    distance_m <- as.numeric(sf::st_distance(
      origins[pair_origin, ], points[pair_component, ], by_element = TRUE
    ))
    x <- data.table::data.table(component_index = pair_component, distance_m = distance_m)
    x <- x[distance_m <= maximum_distance_m + 1e-7, .(distance_m = min(distance_m)), by = component_index]
    x[, station_id := station_ids[i]]
    result[[i]] <- x
    if (i %% 50L == 0L) message(progress_label, ": ", i, "/", length(groups), " stations")
  }
  out <- data.table::rbindlist(result, use.names = TRUE)
  data.table::setcolorder(out, c("station_id", "component_index", "distance_m"))
  out[]
}

aggregate_station_catchments <- function(distances, population_values, stations, radii_m, scenario) {
  x <- data.table::copy(distances)
  x[, estimated_population := population_values[component_index]]
  x[, first_radius_m := pmax(min(radii_m), ceiling(distance_m / min(radii_m)) * min(radii_m))]
  x[first_radius_m < min(radii_m), first_radius_m := min(radii_m)]
  incremental <- x[first_radius_m <= max(radii_m), .(
    incremental_population = sum(estimated_population),
    incremental_components = .N
  ), by = .(station_id, radius_m = first_radius_m)]
  grid <- data.table::CJ(station_id = stations$station_id, radius_m = radii_m, unique = TRUE)
  grid <- merge(grid, incremental, by = c("station_id", "radius_m"), all.x = TRUE)
  grid[is.na(incremental_population), incremental_population := 0]
  grid[is.na(incremental_components), incremental_components := 0L]
  data.table::setorder(grid, station_id, radius_m)
  grid[, `:=`(
    population = cumsum(incremental_population),
    residential_components = cumsum(incremental_components)
  ), by = station_id]
  grid[, scenario := scenario]
  grid[]
}

catchment_geometries <- function(origins, radii_m, region_geometry, scenario) {
  station_ids <- unique(origins$station_id)
  rows <- lapply(radii_m, function(radius) {
    buffered <- sf::st_buffer(origins, radius)
    geometries <- sf::st_sfc(lapply(station_ids, function(id) {
      sf::st_union(sf::st_geometry(buffered[buffered$station_id == id, ]))[[1]]
    }), crs = sf::st_crs(origins))
    x <- sf::st_sf(station_id = station_ids, radius_m = radius, scenario = scenario, geometry = geometries)
    sf::st_geometry(x) <- sf::st_intersection(sf::st_geometry(x), region_geometry)
    x$catchment_area_km2 <- as.numeric(sf::st_area(x)) / 1e6
    x
  })
  do.call(rbind, rows)
}

nearest_network_distance <- function(points, origins) {
  index <- sf::st_nearest_feature(points, origins)
  data.table::data.table(
    nearest_station_id = origins$station_id[index],
    distance_m = as.numeric(sf::st_distance(points, origins[index, ], by_element = TRUE))
  )
}

regional_coverage_table <- function(nearest, radii_m, networks, scenarios, regional_population) {
  rows <- list()
  k <- 0L
  for (scenario in scenarios) {
    for (network in networks) {
      distance_column <- paste(scenario, network, "distance_m", sep = "_")
      for (radius in radii_m) {
        k <- k + 1L
        population <- sum(nearest$estimated_population[nearest[[distance_column]] <= radius])
        rows[[k]] <- data.table::data.table(
          scenario = scenario, network = network, radius_m = radius,
          population = population, population_share = population / regional_population
        )
      }
    }
  }
  data.table::rbindlist(rows)
}

network_overlap_table <- function(nearest, radii_m, networks, scenarios, regional_population) {
  pairs <- utils::combn(networks, 2, simplify = FALSE)
  rows <- list()
  k <- 0L
  population <- nearest$estimated_population
  for (scenario in scenarios) {
    for (pair in pairs) {
      da <- nearest[[paste(scenario, pair[1], "distance_m", sep = "_")]]
      db <- nearest[[paste(scenario, pair[2], "distance_m", sep = "_")]]
      for (radius in radii_m) {
        a <- da <= radius
        b <- db <= radius
        k <- k + 1L
        rows[[k]] <- data.table::data.table(
          scenario = scenario, network_a = pair[1], network_b = pair[2], radius_m = radius,
          population_a_only = sum(population[a & !b]),
          population_b_only = sum(population[!a & b]),
          population_both = sum(population[a & b]),
          population_union = sum(population[a | b]),
          population_neither = sum(population[!a & !b]),
          regional_population = regional_population
        )
      }
    }
  }
  data.table::rbindlist(rows)
}
