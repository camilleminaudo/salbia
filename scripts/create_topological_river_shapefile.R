library(sf)
library(dplyr)

# ── settings ──────────────────────────────────────────────────────────────
shp_path      <- "C:/Users/Camille Minaudo/OneDrive - Universitat de Barcelona/Documentos/PROJECTS/SALBIA/GIS/River_Gambia_fixedgeom.shp"
start_dumb_id <- 2        # dumb_id of the most upstream segment
buffer_dist   <- 0.1   # in CRS units; use 0.1 or similar if CRS is in meters
# ──────────────────────────────────────────────────────────────────────────

# Load shapefile
river <- st_read(shp_path)
cat("Loaded", nrow(river), "segments\n")

# Build neighbour list using spatial index
# st_intersects does this efficiently with an internal spatial index —
# no manual index construction needed in sf
cat("Building topology...\n")
river_buf <- st_buffer(river, buffer_dist)
neighbours <- st_intersects(river_buf, river)  # returns a list, one entry per segment

# Convert to dumb_id-based lookup
# st_intersects returns positional indices, so we map back to dumb_id
dumb_ids <- river$dumb_id

get_neighbours <- function(pos) {
  nb_positions <- neighbours[[pos]]
  nb_dumb_ids  <- dumb_ids[nb_positions]
  # exclude self
  nb_dumb_ids[nb_dumb_ids != dumb_ids[pos]]
}

# Position lookup: dumb_id -> row index in river
pos_of <- setNames(seq_len(nrow(river)), dumb_ids)

cat("Topology ready. Walking downstream...\n")

# Walk downstream from start
order     <- integer(nrow(river))   # will hold seg_id for each row
visited   <- logical(nrow(river))   # TRUE once a segment is assigned
seg_id    <- 0L
current   <- pos_of[as.character(start_dumb_id)]

while (!is.na(current)) {
  visited[current]  <- TRUE
  order[current]    <- seg_id
  seg_id            <- seg_id + 1L
  
  nbs <- get_neighbours(current)
  unvisited_nbs <- nbs[!visited[pos_of[as.character(nbs)]]]
  
  if (length(unvisited_nbs) == 0) {
    cat("Reached end at dumb_id =", dumb_ids[current],
        " (seg_id =", seg_id - 1, ")\n")
    current <- NA
    
  } else if (length(unvisited_nbs) == 1) {
    current <- pos_of[as.character(unvisited_nbs)]
    
  } else {
    # Junction — follow the neighbour with the largest dumb_id as tiebreaker
    cat("WARNING: junction at dumb_id =", dumb_ids[current],
        "| unvisited neighbours:", unvisited_nbs,
        "| following", max(unvisited_nbs), "\n")
    current <- pos_of[as.character(max(unvisited_nbs))]
  }
}

# Report unreached segments
unreached <- dumb_ids[!visited]
if (length(unreached) > 0) {
  cat("WARNING:", length(unreached), "segments not reached:",
      head(unreached, 20), "\n")
  cat("Try increasing buffer_dist\n")
} else {
  cat("All segments reached successfully\n")
}

# Write seg_id back and export
river$seg_id <- order

st_write(river, shp_path, delete_layer = TRUE)
cat("Done. seg_id written to", shp_path, "\n")