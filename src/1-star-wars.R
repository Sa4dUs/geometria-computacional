# ============================================================
# File: 1-star-wars.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-02-09
# Last modified: 2026-03-1
# License: MIT
# ============================================================

library(ggplot2)
library(dplyr)
library(sf)

v_df <- readLines("assets/millennium-falcon.obj") %>%
  grep("^v\\s+", ., value = TRUE) %>%
  read.table(text = ., header = FALSE) %>%
  select(x = 2, y = 3, z = 4) %>%
  mutate(id = row_number())

f_lines <- readLines("assets/millennium-falcon.obj") %>%
  grep("^f\\s+", ., value = TRUE) %>%
  gsub("/[0-9/]*", "", .) %>%
  gsub("^f\\s+", "", .)

process_visible_facets <- function(f_lines, v_df) {
  lapply(f_lines, function(line) {
    idx <- as.numeric(strsplit(line, "\\s+")[[1]])
    if (length(idx) < 3) return(NULL)
    
    p <- v_df[idx[1:3], 1:3]
    v1 <- as.numeric(p[2,] - p[1,])
    v2 <- as.numeric(p[3,] - p[1,])
    normal_y <- v1[3] * v2[1] - v1[1] * v2[3]
    
    if (normal_y < 0) return(NULL)
    
    mat <- as.matrix(v_df[c(idx, idx[1]), c("x", "z")])
    list(st_polygon(list(mat)), idx)
  })
}

facets_data <- process_visible_facets(f_lines, v_df)
facets_data <- facets_data[!sapply(facets_data, is.null)]

polygons_sfc <- st_sfc(lapply(facets_data, `[[`, 1))

ship_clean_union <- polygons_sfc %>%
  st_buffer(dist = 1e-9) %>% 
  st_union() %>%
  st_make_valid()

ship_outline <- st_union(ship_clean_union) %>% 
  st_buffer(dist = 0)

ship_simplified <- st_simplify(ship_clean_union, dTolerance = 0.005)

df_points <- v_df %>% filter(id %in% unique(unlist(lapply(facets_data, `[[`, 2))))
df_edges <- do.call(rbind, lapply(facets_data, function(x) {
  idx <- x[[2]]
  data.frame(x1 = v_df$x[idx], z1 = v_df$z[idx], 
             x2 = v_df$x[c(idx[-1], idx[1])], z2 = v_df$z[c(idx[-1], idx[1])])
}))

efficient_tri <- st_intersection(st_collection_extract(st_triangulate(ship_simplified), "POLYGON"), ship_simplified)

plot_theme <- theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(df_points, aes(x, z)) + geom_point(size = 0.1) + coord_fixed() + labs(title = "2D Projection: Points") + plot_theme
ggplot(df_edges) + geom_segment(aes(x=x1, y=z1, xend=x2, yend=z2), linewidth = 0.1) + coord_fixed() + labs(title = "2D Projection: Points") + plot_theme
ggplot() + geom_sf(data = ship_outline, fill = "gray90", color = "blue") + labs(title = "2D Projection: Concave Hull") + plot_theme
ggplot() + geom_sf(data = efficient_tri, fill = "gray95", color = "black", linewidth = 0.1) + labs(title = "2D Projection: Triangulation") + plot_theme

area <- as.numeric(st_area(ship_simplified))

ggplot() +
  geom_sf(data = efficient_tri, fill = "gray95", color = "black", linewidth = 0.1) +
  geom_sf(data = ship_simplified, fill = NA, color = "steelblue", linewidth = 0.5) +
  annotate("text", x = Inf, y = -Inf, label = paste("Area:", round(area, 2)), 
           hjust = 1.1, vjust = -1.1, fontface = "bold", color = "darkred") +
  labs(title = "Final Geometry & Area Verification") +
  plot_theme