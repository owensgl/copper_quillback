# install required packages
library(tidyverse)
library(cowplot)
library(PNWColors)
library(clipr)

# set working directory

# set colour palette for visualization
sp_palette <- c("Copper" = "#daa038", 
                "Quillback" = "#0A369D")

# load sample info
sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/sample_info.txt")

# for both species combined ----------------------------------------------------
samples <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/structure/combined.nosex", 
                    col_names = c("sample", "trash")) %>%
  subset(select = 1)

# read in the admixture data for K = 2,5
all_admix <- tibble()

# Loop up the reading for each k
for (k in 2:5){
  data <- read.delim(paste0("/project/ctb-grego/ntbsykes/copper_quillback/structure/combined.",k,".Q"),
                     header = FALSE,
                     col.names = paste0("Q",seq(1:k)),
                     sep = " ")
  data$sample <- samples$sample
  data$k <- k
  
  # Convert to longer
  data %>% gather(Q, value, -sample, -k) -> data
  all_admix <- rbind(all_admix, data)
}

# Load in the location info for sorting the plot
all_admix <- inner_join(all_admix, sample_info)

assign_groups_to_species <- function(admix_data, k_val) {
  species_affinity <- admix_data %>%
    filter(k == k_val) %>%
    group_by(species, Q) %>%
    summarise(mean_ancestry = mean(value), .groups = 'drop') %>%
    group_by(Q) %>%
    slice_max(mean_ancestry, n = 1) %>%
    select(Q, species) %>%
    rename(assigned_species = species)
  
  return(species_affinity)
}
# Get assignments for all K values
k_values <- unique(all_admix$k)
group_assignments <- map_dfr(k_values, ~assign_groups_to_species(all_admix, .x) %>% 
                               mutate(k = .x))
####
create_color_palette <- function(group_assignments) {
  # Count groups per species per K
  groups_per_species <- group_assignments %>%
    group_by(k, assigned_species) %>%
    summarise(n_groups = n(), .groups = 'drop')
  
  # Get maximum number of groups needed for each species
  max_copper_groups <- max(groups_per_species$n_groups[groups_per_species$assigned_species == "Copper"])
  max_quillback_groups <- max(groups_per_species$n_groups[groups_per_species$assigned_species == "Quillback"])
  
  # Create color palettes starting with your specified colors
  copper_colors <- colorRampPalette(c("#daa038", "#f4c465", "#f7d791", "#faeabc"))(max_copper_groups)
  quillback_colors <- colorRampPalette(c("#0A369D", "#2d5cb8", "#5082d3", "#73a8ee"))(max_quillback_groups)
  
  # If you need more colors, extend the palette
  if(max_copper_groups > 4) {
    copper_colors <- colorRampPalette(c("#daa038", "#f4c465", "#f7d791", "#faeabc", "#fdf6e3"))(max_copper_groups)
  }
  
  if(max_quillback_groups > 4) {
    quillback_colors <- colorRampPalette(c("#0A369D", "#2d5cb8", "#5082d3", "#73a8ee", "#a5ceff"))(max_quillback_groups)
  }
  
  # Assign colors to groups
  color_assignments <- group_assignments %>%
    group_by(k, assigned_species) %>%
    arrange(Q) %>%
    mutate(color_index = row_number()) %>%
    ungroup() %>%
    mutate(color = case_when(
      assigned_species == "Copper" ~ copper_colors[color_index],
      assigned_species == "Quillback" ~ quillback_colors[color_index]
    ))
  
  return(color_assignments)
}

color_assignments <- create_color_palette(group_assignments)

plot_data <- all_admix %>%
  left_join(color_assignments, by = c("k", "Q"))






# plot the admixture bars
inside_pops <- c("Salish Sea", "Puget Sound")
plot_order <- c("Copper-Outside","Copper-Inside",
                "Quillback-Inside","Quillback-Outside")

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/admixture_bars_both_kplus.pdf",
    height=6,width=12)
plot_data %>%
  mutate(location = case_when(population %in% inside_pops ~ "Inside",
                              TRUE ~ "Outside")) %>% 
  mutate(location_species = paste0(species,"-",location)) %>%
  mutate(location_species = fct_relevel(location_species, plot_order)) %>%
  
  filter(k != 1, k < 9) %>%
  arrange(latitude) %>%
  ggplot(., aes(x = reorder(sample, latitude), y = value, fill = factor(color))) +
  geom_bar(stat = "identity", position = "stack") +
  #scale_fill_manual(values = c("grey80", "#daa038")) +
  xlab("Samples sorted by latitude") +
  ylab("Ancestry") +
  facet_grid(k~location_species, scales = "free", space = "free") +
  theme_cowplot() +
  theme(
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.text = element_text(size = 12),
    strip.background.y = element_blank(),
    #strip.text.y = element_blank(),
    legend.position = "none") +
  scale_fill_identity()
dev.off()

