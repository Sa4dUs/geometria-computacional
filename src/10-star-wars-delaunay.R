# ============================================================
# File: 10-star-wars-delaunay.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-04-26
# Last modified: 2026-04-26
# License: MIT
# ============================================================

library(deldir)
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

df_points_clean <- df_points %>% 
  distinct(x, z, .keep_all = TRUE)

vt_deldir <- deldir(df_points$x, df_points$z)

tri_list <- deldir::triang.list(vt_deldir)
tri_sf <- st_as_sf(do.call(rbind, lapply(tri_list, function(t) {
  st_sf(geometry = st_sfc(st_polygon(list(as.matrix(rbind(t[,c("x","y")], t[1,c("x","y")]))))))
}))) %>% st_intersection(ship_outline)

area_total <- sum(st_area(tri_sf))

teselas_list <- tile.list(vt_deldir)
teselas_sf <- st_as_sf(do.call(rbind, lapply(teselas_list, function(tile) {
  st_sf(geometry = st_sfc(st_polygon(list(as.matrix(rbind(cbind(tile$x, tile$y), c(tile$x[1], tile$y[1])))))))
}))) %>% st_intersection(ship_outline)

plot_theme <- theme_void() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot() +
  geom_sf(data = tri_sf, fill = "gray95", color = "black", linewidth = 0.1) +
  annotate("text", x = Inf, y = -Inf, label = paste("Área:", round(area_total, 4)), 
           hjust = 1.1, vjust = -1.1, fontface = "bold", color = "darkred") +
  plot_theme

ggplot() +
  geom_sf(data = teselas_sf, fill = "lightblue", color = "white", alpha = 0.6) +
  geom_point(data = df_points, aes(x, z), size = 0.1, color = "black") +
  plot_theme