library(sf)
library(dplyr)

# ── settings ──────────────────────────────────────────────────────────────
shp_path      <- "C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/SALBIA/GIS/River_Gambia_Basse2Ocean_manually_fixed.shp"
start_dumb_id <- 1
buffer_dist   <- 0.1
# ──────────────────────────────────────────────────────────────────────────

river <- st_read(shp_path)
cat("Loaded", nrow(river), "segments\n")

# ── topology and seg_id (unchanged from before) ───────────────────────────
river_buf  <- st_buffer(river, buffer_dist)
neighbours <- st_intersects(river_buf, river)
dumb_ids   <- river$dumb_id
pos_of     <- setNames(seq_len(nrow(river)), dumb_ids)

get_neighbours <- function(pos) {
  nb_positions <- neighbours[[pos]]
  nb_dumb_ids  <- dumb_ids[nb_positions]
  nb_dumb_ids[nb_dumb_ids != dumb_ids[pos]]
}

cat("Walking downstream...\n")
order   <- integer(nrow(river))
visited <- logical(nrow(river))
seg_id  <- 0L
current <- pos_of[as.character(start_dumb_id)]

while (!is.na(current)) {
  visited[current] <- TRUE
  order[current]   <- seg_id
  seg_id           <- seg_id + 1L
  
  nbs           <- get_neighbours(current)
  unvisited_nbs <- nbs[!visited[pos_of[as.character(nbs)]]]
  
  if (length(unvisited_nbs) == 0) {
    cat("Reached end at dumb_id =", dumb_ids[current],
        "(seg_id =", seg_id - 1, ")\n")
    current <- NA
  } else if (length(unvisited_nbs) == 1) {
    current <- pos_of[as.character(unvisited_nbs)]
  } else {
    cat("WARNING: junction at dumb_id =", dumb_ids[current],
        "| following", max(unvisited_nbs), "\n")
    current <- pos_of[as.character(max(unvisited_nbs))]
  }
}

unreached <- dumb_ids[!visited]
if (length(unreached) > 0) {
  cat("WARNING:", length(unreached), "segments not reached:",
      head(unreached, 20), "\n")
}

river$seg_id <- order

# ── cumulative distance at start and end nodes ────────────────────────────
# Reproject to a metric CRS for accurate length calculation if needed.
# If your layer is already in meters (UTM), skip this step.
# EPSG:32628 = UTM zone 28N, appropriate for the Gambia.
river_m <- st_transform(river, crs = 32628)

# Compute each segment's length in km
river_m$seg_length_km <- as.numeric(st_length(river_m)) / 1000

# Sort by seg_id to compute cumulative distances in order
river_m <- river_m[order(river_m$seg_id), ]

# dist_start_km: cumulative distance from upstream at the segment's start node
# dist_end_km:   cumulative distance from upstream at the segment's end node
river_m$dist_start_km <- cumsum(c(0, river_m$seg_length_km[-nrow(river_m)]))
river_m$dist_end_km   <- cumsum(river_m$seg_length_km)

# ── optional: distance from ocean instead of from upstream ────────────────
# Uncomment these two lines if you prefer ocean as origin (x=0 at ocean end)
total_length <- max(river_m$dist_end_km)
river_m$from_ocean_start_km <- total_length - river_m$dist_start_km
river_m$from_ocean_end_km   <- total_length - river_m$dist_end_km

# Transform back to original CRS before writing
river_out <- st_transform(river_m, crs = st_crs(river))

cat("\nFirst few segments:\n")
print(st_drop_geometry(river_out) |>
        head(10))



# Keep only what you need
river_out_clean <- river_out |>
  select(seg_id, 
         seg_length_km, 
         dist_start_km, dist_end_km,
         from_ocean_start_km, from_ocean_end_km)

# Then write — to a fresh filename to avoid lock issues
st_write(river_out_clean, 
         paste0(dirname(shp_path), "/", "basse2ocean_riverKM_v2.gpkg"))


