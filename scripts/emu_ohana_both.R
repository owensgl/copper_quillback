# load required packages
library(tidyverse)
library(cowplot)
library(patchwork)
library(SNPRelate)
library(zoo)
library(ape)
library(ggtree)
library(clue)
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


###Emu plotting Copper
missing_data_copper <- read_table("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.imiss") %>%
  select(INDV, F_MISS) %>%
  rename(sample = INDV)
emu_pca_copper <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.emu.eigvecs") %>%
  select(-`#FID`) %>%
  rename(sample = IID) %>%
  inner_join(sample_info)

eigenvalues_copper <- scan("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.emu.eigvals")

# 2. Calculate the Percent Variance Explained (PVE)
# Formula: (individual eigenvalue / sum of all eigenvalues) * 100
pve_values_copper <- (eigenvalues_copper / sum(eigenvalues_copper)) * 100

# 3. Create the data frame with the single column "PVE"
pve_table_copper <- data.frame(PVE = pve_values_copper)

pca_1_copper <- emu_pca_copper %>%
  inner_join(missing_data_copper) %>%
  ggplot(., aes(PC1, PC2, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  scale_shape_manual(name="Species", values=c(1,2)) +
  xlab(paste0("PC1 (", round(pve_table$PVE[1], 2), "%)")) +
  ylab(paste0("PC2 (", round(pve_table$PVE[2], 2), "%)")) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm"),      # Shrinks the actual color boxes
    legend.position = "none"
  )
pca_1_copper
pca_2_copper <- emu_pca_copper %>%
  inner_join(missing_data_copper) %>%
  ggplot(., aes(PC3, PC4, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  scale_shape_manual(name="Species", values=c(1,2)) +
  xlab(paste0("PC3 (", round(pve_table$PVE[3], 2), "%)")) +
  ylab(paste0("PC4 (", round(pve_table$PVE[4], 2), "%)")) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm"),      # Shrinks the actual color boxes
    legend.position = "none"
  )

pca_1_copper | pca_2_copper

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/pca_copper_greg.v3.pdf",
    height=4,width=8)
pca_1_copper | pca_2_copper
dev.off()

###Emu plotting quillback
missing_data_quillback <- read_table("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/quillback_combined.imiss") %>%
  select(INDV, F_MISS) %>%
  rename(sample = INDV)
emu_pca_quillback <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/quillback_combined.emu.eigvecs") %>%
  select(-`#FID`) %>%
  rename(sample = IID) %>%
  inner_join(sample_info)

eigenvalues_quillback <- scan("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/quillback_combined.emu.eigvals")

# 2. Calculate the Percent Variance Explained (PVE)
# Formula: (individual eigenvalue / sum of all eigenvalues) * 100
pve_values_quillback <- (eigenvalues_quillback / sum(eigenvalues_quillback)) * 100

# 3. Create the data frame with the single column "PVE"
pve_table_quillback <- data.frame(PVE = pve_values_quillback)

pca_1_quillback <- emu_pca_quillback %>%
  inner_join(missing_data_quillback) %>%
  ggplot(., aes(PC1, PC2, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  scale_shape_manual(name="Species", values=c(1,2)) +
  xlab(paste0("PC1 (", round(pve_table$PVE[1], 2), "%)")) +
  ylab(paste0("PC2 (", round(pve_table$PVE[2], 2), "%)")) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm"),      # Shrinks the actual color boxes
    legend.position = "none"
  )
pca_1_quillback
pca_2_quillback <- emu_pca_quillback %>%
  inner_join(missing_data_quillback) %>%
  ggplot(., aes(PC3, PC4, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  scale_shape_manual(name="Species", values=c(1,2)) +
  xlab(paste0("PC3 (", round(pve_table$PVE[3], 2), "%)")) +
  ylab(paste0("PC4 (", round(pve_table$PVE[4], 2), "%)")) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm"),      # Shrinks the actual color boxes
    legend.position = "none"
  )

pca_1_quillback | pca_2_quillback

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/pca_quillback_greg.v3.pdf",
    height=4,width=8)
pca_1_quillback | pca_2_quillback
dev.off()


####Ohana plots Copper


samples_copper <- read_delim("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.fam",
                      col_names = c("sample", "trash"), delim=" ") %>%
  subset(select = 1)

# Make an empty data frame to slap all this stuff into
admix_copper <- tibble(sample = character(),
                     k = numeric(),
                     Q = character(),
                     value = numeric())

# Loop up the reading for each k
for (k in 2:7){
  data <- read.delim(paste0("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.",k,".q.matrix"),
                     header = FALSE, skip=1,
                     col.names = paste0("Q",seq(1:k)),
                     sep = "\t")
  data$sample <- samples_copper$sample
  data$k <- k
  
  # Convert to longer
  data %>% gather(Q, value, -sample, -k) -> data
  admix_copper <- rbind(admix_copper, data)
}

# Load in the location info for sorting the plot
admix_copper <- inner_join(admix_copper, sample_info)





ohana_plot_copper <- admix_copper %>%
  filter(k == 2) %>%
  ggplot(aes(x = reorder(sample, -latitude), y = value, fill = factor(Q))) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  scale_fill_manual(values = c("#daa038","#73541c")) +
  facet_grid(. ~ pop_short, scales = "free", space = "free") +
  theme_cowplot() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.text.x = element_text(size = 10),
    #strip.background = element_blank(),
    legend.position = "none"
  ) +
  ylab("Ancestry") + xlab("Sample (North to South)")


####Ohana supmat plots



# 1. Define a global margin to ensure every plot 'claims' the same amount of space
shared_margin <- margin(t = 5, r = 5, b = 5, l = 5)

# 2. Updated Admixture Function
make_admix_plot <- function(df, k_val) {
  df %>%
    filter(k == k_val) %>%
    inner_join(remapped_Q) %>%
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
  
  file_path <- paste0("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.", k_val, ".tree.nwk")
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
all_rows_copper <- lapply(2:6, function(k) {
  p_admix <- make_admix_plot(admix_copper, k)
  p_phylo <- make_phylo_plot(k)
  
  # patchwork will now align the actual plot panels because the 
  # outer dimensions (margins) are identical.
  return(p_admix + p_phylo + plot_layout(widths = c(3, 1)))
})
pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/copper_ohana.pdf",
    height=8,width=8)
wrap_plots(all_rows_copper, ncol = 1)  
dev.off()


####Ohana plots quillback


samples_quillback <- read_delim("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/quillback_combined.fam",
                             col_names = c("sample", "trash"), delim=" ") %>%
  subset(select = 1)

# Make an empty data frame to slap all this stuff into
admix_quillback <- tibble(sample = character(),
                       k = numeric(),
                       Q = character(),
                       value = numeric())

# Loop up the reading for each k
for (k in 2:6){
  data <- read.delim(paste0("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/quillback_combined.",k,".q.matrix"),
                     header = FALSE, skip=1,
                     col.names = paste0("Q",seq(1:k)),
                     sep = "\t")
  data$sample <- samples_quillback$sample
  data$k <- k
  
  # Convert to longer
  data %>% gather(Q, value, -sample, -k) -> data
  admix_quillback <- rbind(admix_quillback, data)
}

# Load in the location info for sorting the plot
admix_quillback <- inner_join(admix_quillback, sample_info)






# 2. Updated Admixture Function

ohana_plot_quillback <- admix_quillback %>%
  filter(k == 2) %>%
  ggplot(aes(x = reorder(sample, -latitude), y = value, fill = factor(Q))) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  scale_fill_manual(values = c("#0A369D","#89ACF3")) +
  facet_grid(. ~ pop_short, scales = "free", space = "free") +
  theme_cowplot() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.text.x = element_text(size = 10),
    #strip.background = element_blank(),
    legend.position = "none"
  ) +
  ylab("Ancestry") + xlab("Sample (North to South)")


ohana_plot_quillback




pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/ohana_copper_quill_greg.pdf",
    height=3,width=16)
ohana_plot_copper | ohana_plot_quillback
dev.off()


###Ohana quillback supplement


# 1. Define a global margin to ensure every plot 'claims' the same amount of space
shared_margin <- margin(t = 5, r = 5, b = 5, l = 5)

# 2. Updated Admixture Function
make_admix_plot <- function(df, k_val) {
  df %>%
    filter(k == k_val) %>%
    inner_join(remapped_Q) %>%
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
  
  file_path <- paste0("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/quillback_combined.", k_val, ".tree.nwk")
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
all_rows_quillback <- lapply(2:6, function(k) {
  p_admix <- make_admix_plot(admix_quillback, k)
  p_phylo <- make_phylo_plot(k)
  
  # patchwork will now align the actual plot panels because the 
  # outer dimensions (margins) are identical.
  return(p_admix + p_phylo + plot_layout(widths = c(3, 1)))
})

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/quillback_ohana.pdf",
    height=8,width=8)
wrap_plots(all_rows_quillback, ncol = 1)  
dev.off()

