# load required packages
library(tidyverse)
library(cowplot)
library(patchwork)
library(SNPRelate)
library(zoo)
library(ape)
library(ggtree)
is.waive <- function(x) {
  inherits(x, "waiver")
}

# create colour palettes
pop_palette <- c("Alaska" = "#29335C",
                 "Hecate Strait" = "#6A679E",
                 "Queen Charlotte Sound" = "#009FFD",
                 "Georgia Basin" = "#058C42",
                 "Vancouver Island" = "#FFB710",
                 "Puget Sound" = "#F75524",
                 "Washington & Oregon" = "#CB002D",
                 "California" = "#7B1919")


sp_palette <- c("Copper" = "#daa038",
                "Quillback" = "#0A369D")

sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/copper_quillback_summary.txt") %>%
  select(sample, species, latitude, longitude, population, minor_parent_ancestry) %>%
  mutate(population = case_when(population == "Salish Sea" ~ "Georgia Basin",
                                T ~ population))



location_renamed <- tibble(population=c("Alaska","Hecate Strait","Queen Charlotte Sound",
                                        "Georgia Basin","Vancouver Island","Puget Sound",
                                        "Washington & Oregon","California"),
                           pop_short = c("AK","HS","QCS","GB","VI","PS","WA/OR","CA")
)
# Define your desired order as a vector
pop_order <- c("AK", "HS", "QCS", "VI", "GB", "PS", "WA/OR", "CA")

location_renamed <- location_renamed %>%
  mutate(pop_short = factor(pop_short, levels = pop_order))
sample_info <- sample_info %>% 
  inner_join(location_renamed)


###Emu plotting
missing_data <- read_table("/project/ctb-grego/ntbsykes/copper_quillback/vcf/outside_quillback/combined.quillback.outside.imiss") %>%
  select(INDV, F_MISS) %>%
  rename(sample = INDV)
emu_pca <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/vcf/outside_quillback/combined.quillback.outside.emu.eigvecs") %>%
  select(-`#FID`) %>%
  rename(sample = IID) %>%
  inner_join(sample_info)

eigenvalues <- scan("/project/ctb-grego/ntbsykes/copper_quillback/vcf/outside_quillback/combined.quillback.outside.emu.eigvals")

# 2. Calculate the Percent Variance Explained (PVE)
# Formula: (individual eigenvalue / sum of all eigenvalues) * 100
pve_values <- (eigenvalues / sum(eigenvalues)) * 100

# 3. Create the data frame with the single column "PVE"
pve_table <- data.frame(PVE = pve_values)

pca_1 <- emu_pca %>%
  inner_join(missing_data) %>%
  ggplot(., aes(PC1, PC2, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  xlab(paste0("PC1 (", round(pve_table$PVE[1], 2), "%)")) +
  ylab(paste0("PC2 (", round(pve_table$PVE[2], 2), "%)")) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm")      # Shrinks the actual color boxes
  )

pca_2 <- emu_pca %>%
  inner_join(missing_data) %>%
  ggplot(., aes(PC1, PC2, colour = F_MISS)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_color_viridis_c(
    option = "viridis",
    # Set the specific numeric positions for the labels
    breaks = c(0, 0.2, 0.4, 0.6),
    # Set the text to be displayed at those positions
    labels = c("0", "0.2", "0.4", "0.6"),
    # Ensure the scale limits don't cut off your requested breaks
    limits = c(0, 0.6) 
  ) +
  xlab(paste0("PC1 (", round(pve_table$PVE[1], 2), "%)")) +
  ylab(paste0("PC2 (", round(pve_table$PVE[2], 2), "%)")) +
  theme_cowplot() +
  labs(colour = "Proportion\nmissing") +
  guides(colour = guide_colourbar(position = "bottom")) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm")      # Shrinks the actual color boxes
  )


####Ohana plots


samples <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback_outside/combined.quillback.outside.nosex",
                    col_names = c("sample", "trash")) %>%
  subset(select = 1)

# Make an empty data frame to slap all this stuff into
quillback_admix <- tibble(sample = character(),
                       k = numeric(),
                       Q = character(),
                       value = numeric())

# Loop up the reading for each k
for (k in 2:4){
  data <- read.delim(paste0("/project/ctb-grego/ntbsykes/copper_quillback/vcf/outside_quillback/combined.quillback.outside.",k,".q.matrix"),
                     header = FALSE, skip=1,
                     col.names = paste0("Q",seq(1:k)),
                     sep = "\t")
  data$sample <- samples$sample
  data$k <- k
  
  # Convert to longer
  data %>% gather(Q, value, -sample, -k) -> data
  quillback_admix <- rbind(quillback_admix, data)
}

# Load in the location info for sorting the plot
quillback_admix <- inner_join(quillback_admix, sample_info)




#####
# 1. Function for the Admixture Plot
make_admix_plot <- function(df, k_val) {
  df %>%
    filter(k == k_val) %>%
    ggplot(aes(x = reorder(sample, latitude), y = value, fill = factor(Q))) +
    geom_bar(stat = "identity", position = "stack", width = 1) +
    scale_fill_viridis_d() +
    facet_grid(. ~ pop_short, scales = "free", space = "free") +
    theme_cowplot() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      strip.text.x = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5)
    )
}

# 2. Function for the Phylogeny Plot
make_phylo_plot <- function(k_val) {
  if (k_val == 2) {
    return(plot_spacer()) # Leaves a blank space for k=2
  }
  
  file_path <- paste0("/project/ctb-grego/ntbsykes/copper_quillback/vcf/outside_quillback/combined.quillback.outside.", k_val, ".tree.nwk")
  
  if (!file.exists(file_path)) return(plot_spacer(theme(plot.margin = margin(15, 15, 15, 15))))
  
  tree <- read.tree(file_path)
  tree_data <- data.frame(label = tree$tip.label, value = as.numeric(tree$tip.label))
  
  p <- suppressMessages(
    ggtree(tree, layout = "daylight") %<+% tree_data + 
      geom_tippoint(aes(color = as.factor(value)), size = 3) +
      scale_color_viridis_d(option = "viridis") + 
      # 1. Fixed: Removed side = "both". Just provide the ratio.
      hexpand(.2) + 
      vexpand(.2) +
      # 2. Keep clip = "off" to prevent the cut-off circles
      coord_cartesian(clip = "off") + 
      theme_tree() +
      theme(
        legend.position = "none",
        # 3. Use generous margins so the circles don't hit the edge of the patchwork
        plot.margin = margin(15, 15, 15, 15) 
      )
  )
  return(p)
}

# 3. Generate and Combine Rows
# Create a list of combined plots (Admix + Phylo) for each k
all_rows <- lapply(2:4, function(k) {
  p_admix <- make_admix_plot(quillback_admix, k)
  p_phylo <- make_phylo_plot(k)
  
  # Return the horizontal pair
  # widths = c(2, 1) makes the admixture plot wider than the phylogeny
  return(p_admix + p_phylo + plot_layout(widths = c(3, 1)))
})

# 4. Final Patchwork Assembly
# wrap_plots with ncol = 1 stacks the rows vertically
final_plot <- wrap_plots(all_rows, ncol = 1) +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(final_plot)


# 1. Define a global margin to ensure every plot 'claims' the same amount of space
shared_margin <- margin(t = 5, r = 5, b = 5, l = 5)

# 2. Updated Admixture Function
make_admix_plot <- function(df, k_val) {
  df %>%
    filter(k == k_val) %>%
    ggplot(aes(x = reorder(sample, -latitude), y = value, fill = factor(Q))) +
    geom_bar(stat = "identity", position = "stack", width = 1) +
    scale_fill_viridis_d() +
    facet_grid(. ~ pop_short, scales = "free", space = "free") +
    theme_cowplot() +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      strip.text.x = element_text(size = 10),
      strip.background = element_blank(),
      legend.position = "none",
      # Apply the shared margin
      plot.margin = shared_margin
    ) +
    ylab("Ancestry") + xlab("Sample (North to South)")
}

# 3. Updated Phylogeny Function
make_phylo_plot <- function(k_val) {
  # If k=2, return a blank plot with the exact same margin and theme
  if (k_val == 2) {
    return(ggplot() + theme_void() + theme(plot.margin = shared_margin))
  }
  
  file_path <- paste0("/project/ctb-grego/ntbsykes/copper_quillback/vcf/outside_quillback/combined.quillback.outside.", k_val, ".tree.nwk")
  if (!file.exists(file_path)) return(ggplot() + theme_void() + theme(plot.margin = shared_margin))
  
  tree <- read.tree(file_path)
  tree_data <- data.frame(label = tree$tip.label, value = as.numeric(tree$tip.label))
  
  p <- suppressMessages(
    ggtree(tree, layout = "daylight") %<+% tree_data + 
      geom_tippoint(aes(color = as.factor(value)), size = 3) +
      scale_color_viridis_d(option = "viridis") + 
      hexpand(.2) + 
      vexpand(.2) +
      coord_cartesian(clip = "off") + 
      theme_tree() +
      theme(
        legend.position = "none",
        # Apply the shared margin
        plot.margin = shared_margin
      )
  )
  return(p)
}

# 4. Combine into rows with locked widths
all_rows <- lapply(2:4, function(k) {
  p_admix <- make_admix_plot(quillback_admix, k)
  p_phylo <- make_phylo_plot(k)
  
  # patchwork will now align the actual plot panels because the 
  # outer dimensions (margins) are identical.
  return(p_admix + p_phylo + plot_layout(widths = c(3, 1)))
})

final_plot <- (pca_1 + pca_2) / wrap_plots(all_rows, ncol = 1)  + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(1, 2))
pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/quillback_outside.emu.ohana.pdf",
    height=8,width=6)
print(final_plot)
dev.off()
