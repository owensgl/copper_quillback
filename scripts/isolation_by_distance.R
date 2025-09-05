library(geosphere)

sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/sample_info.txt") %>%
  mutate(lat = round(latitude), long=round(longitude)) %>%
  mutate(location = paste0(lat, "_",long)) %>%
  mutate(type = case_when(population == "Salish Sea" | population == "Puget Sound" ~ "Inside",
                          TRUE ~ "Outside")) %>% 
  select(location, species, type) %>%
  mutate(type = case_when(location == "49_-125" & species == "Quillback" ~ "Outside",
                           TRUE ~ type)) %>%
  unique()



copper_fst <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/fst/copper/coords/copper_coords_fst_summary.txt")

copper_fst_summary <- copper_fst %>%
  inner_join(sample_info %>% filter(species == "Copper") %>% rename(pop1 = location, loc1 = type)) %>%
  inner_join(sample_info %>% filter(species == "Copper") %>% rename(pop2 = location, loc2 = type)) %>%
  mutate(comparison = paste0(loc1,"-",loc2)) %>%
  mutate(comparison = case_when(comparison == "Outside-Inside" ~ "Inside-Outside",
                                TRUE ~ comparison)) %>%
  separate(pop1, c("lat1","long1"),"_", convert=T) %>%
  separate(pop2, c("lat2","long2"),"_", convert=T)  %>%
  mutate(
    distance_km = distHaversine(cbind(long1, lat1), cbind(long2, lat2)) / 1000
  ) %>%
  mutate(species = "Copper")
  
  

copper_fst_summary %>%
  ggplot(.,aes(x=distance_km,y=fst,color=comparison)) +
  geom_point() +
  geom_smooth(method="lm")


###
quillback_fst <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/fst/quillback/coords/quillback_coords_fst_summary.txt")

quillback_fst_summary <- quillback_fst %>%
  inner_join(sample_info %>% filter(species == "Quillback") %>% rename(pop1 = location, loc1 = type)) %>%
  inner_join(sample_info %>% filter(species == "Quillback") %>% rename(pop2 = location, loc2 = type)) %>%
  mutate(comparison = paste0(loc1,"-",loc2)) %>%
  mutate(comparison = case_when(comparison == "Outside-Inside" ~ "Inside-Outside",
                                TRUE ~ comparison)) %>%
  separate(pop1, c("lat1","long1"),"_", convert=T) %>%
  separate(pop2, c("lat2","long2"),"_", convert=T)  %>%
  mutate(
    distance_km = distHaversine(cbind(long1, lat1), cbind(long2, lat2)) / 1000
  ) %>%
  mutate(species = "Quillback")



quillback_fst_summary %>%
  ggplot(.,aes(x=distance_km,y=fst,color=comparison)) +
  geom_point() +
  geom_smooth(method="lm")

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/isolation_by_distance.v1.pdf",
    height=3.5,width=9)
rbind(quillback_fst_summary, copper_fst_summary) %>%
  ggplot(.,aes(x=distance_km,y=fst,color=comparison)) +
  geom_point(alpha=0.9) +
  geom_smooth(method="lm",se=F) +
  facet_wrap(~species) +
  theme_cowplot() +
  ylab(expression(F[ST])) +
  xlab("Distance (km)") +
  scale_color_viridis_d(name="Comparison") +
  geom_hline(yintercept = 0,linetype="dotted")
dev.off()
anova(lm(fst~comparison + distance_km,data=copper_fst_summary))
summary(lm(fst~comparison + distance_km,data=copper_fst_summary))
anova(lm(fst~comparison + distance_km,data=quillback_fst_summary))
summary(lm(fst~comparison + distance_km,data=quillback_fst_summary))

library(vegan)

# First, create a unique identifier for each location
df <- quillback_fst_summary %>%
  filter(comparison == "Outside-Outside") %>%
  mutate(
    loc1_id = paste(lat1, long1, sep = "_"),
    loc2_id = paste(lat2, long2, sep = "_")
  )


# Get all unique locations
all_locs <- unique(c(df$loc1_id, df$loc2_id))
n_locs <- length(all_locs)

# Create empty matrices
fst_matrix <- matrix(0, nrow = n_locs, ncol = n_locs)
dist_matrix <- matrix(0, nrow = n_locs, ncol = n_locs)
comp_matrix <- matrix(NA, nrow = n_locs, ncol = n_locs)

# Set row and column names
rownames(fst_matrix) <- colnames(fst_matrix) <- all_locs
rownames(dist_matrix) <- colnames(dist_matrix) <- all_locs
rownames(comp_matrix) <- colnames(comp_matrix) <- all_locs

# Convert comparison to numeric (you may need to adjust this based on your categories)
df$comparison_numeric <- as.numeric(as.factor(df$comparison))

# Fill in the matrices
for(i in 1:nrow(df)) {
  loc1 <- df$loc1_id[i]
  loc2 <- df$loc2_id[i]
  
  # Fill both triangles of the matrix (symmetric)
  fst_matrix[loc1, loc2] <- df$fst[i]
  fst_matrix[loc2, loc1] <- df$fst[i]
  
  dist_matrix[loc1, loc2] <- df$distance_km[i]
  dist_matrix[loc2, loc1] <- df$distance_km[i]
  
  comp_matrix[loc1, loc2] <- df$comparison_numeric[i]
  comp_matrix[loc2, loc1] <- df$comparison_numeric[i]
}

# Set diagonal to 0 for distance matrices
diag(fst_matrix) <- 0
diag(dist_matrix) <- 0
diag(comp_matrix) <- 0

# Convert to distance objects
fst_dist <- as.dist(fst_matrix)
geo_dist <- as.dist(dist_matrix)
comp_dist <- as.dist(comp_matrix)

# Perform partial Mantel test
# This tests correlation between fst and geographic distance, controlling for comparison type
partial_mantel_result <- mantel.partial(fst_dist, geo_dist, comp_dist, 
                                        method = "pearson", permutations = 10000)
print(partial_mantel_result)

# Also run regular Mantel test for comparison
regular_mantel <- mantel(fst_dist, geo_dist, method = "pearson", permutations = 10000)
print("Regular Mantel test (without controlling for comparison):")
print(regular_mantel)

