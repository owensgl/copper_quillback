#Process individual sample Dtrios into heatmap

library(tidyverse)
library(patchwork)
library(yarrr)
library(stringr)
library(ape)
library(aplot)
library(ggtree)
# create colour palettes
pop_palette <- c("Alaska" = "#29335C",
                 "Hecate Strait" = "#6A679E",
                 "Queen Charlotte Sound" = "#009FFD",
                 "Salish Sea" = "#058C42",
                 "Vancouver Island" = "#FFB710",
                 "Puget Sound" = "#F75524",
                 "Washington & Oregon" = "#CB002D",
                 "California" = "#7B1919")
palette_1 <- unname(piratepal("basel"))
palette_2 <- unname(piratepal("pony"))
location_renamed <- tibble(population=c("Alaska","Hecate Strait","Queen Charlotte Sound",
                                        "Salish Sea","Vancouver Island","Puget Sound",
                                        "Washington & Oregon","California"),
                           pop_short = c("AK","HS","QCS","GB","VI","PS","WA/OR","CA")
)
# Define your desired order as a vector
pop_order <- c("AK", "HS", "QCS", "VI", "GB", "PS", "WA/OR", "CA")

location_renamed <- location_renamed %>%
  mutate(pop_short = factor(pop_short, levels = pop_order))



sp_palette <- c("Copper" = "#daa038",
                "Quillback" = "#0A369D")
all_files <-  list.files("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/", pattern="BBAA")
non_empty_files <- all_files[file.info(file.path("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/", all_files))$size > 0]
sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/copper_quillback_summary.txt") %>%
  select(sample, species, latitude, longitude, population, minor_parent_ancestry)



all_stats <- tibble()
test_set <- tibble()
for (file in non_empty_files){
  sample_name <- sub("_BBAA\\.txt$", "", file)
  data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/",file)) %>%
    select(P1, P2, P3, Dstatistic, `p-value`,`f4-ratio`)
  flipped_data <- data %>%
    mutate(P2_backup = P1,
           P1_backup = P2,
           D_backup = Dstatistic*-1) %>%
    mutate(P1 = P1_backup,
           P2 = P2_backup,
           Dstatistic = D_backup) %>%
    select(-P1_backup, -P2_backup, -D_backup) %>%
    filter(P1 == "copper") %>%
    filter(grepl("ABL",P2)) 
  data <- rbind(data, flipped_data)
  extra_data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/",file)) %>%
    select(P1, P2, P3, Dstatistic, `p-value`,`f4-ratio`, BBAA, ABBA, BABA) %>%
    filter(((grepl("copper",P1) | grepl("copper",P2)) & grepl("ABL",P3) | 
              (grepl("ABL",P1) | grepl("ABL",P2)) & grepl("copper",P3))) %>%
    mutate(Dstat1 = (ABBA- BBAA)/(ABBA+BBAA),
           Dstat2 = (BBAA - ABBA)/(BBAA +ABBA),
           Dstat3 = (BBAA- BABA)/(BBAA+BABA),
           Dstat4 = (BABA - BBAA)/(BABA +BBAA)) %>%
    mutate(Dstat = case_when((P1 == "copper" & grepl("ABL",P3)) ~ Dstat1,
                             (P3 == "copper" & grepl("ABL",P1)) ~ Dstat2,
                             (P2 == "copper" & grepl("ABL",P3)) ~ Dstat4,
                             (P3 == "copper" & grepl("ABL",P2)) ~ Dstat3
    )) %>%
    mutate(other_spe = case_when((P1 == "copper" & grepl("ABL",P3)) ~ P2,
                                 (P3 == "copper" & grepl("ABL",P1)) ~ P2,
                                 (P2 == "copper" & grepl("ABL",P3)) ~ P1,
                                 (P3 == "copper" & grepl("ABL",P2)) ~ P1
    )) %>%
    mutate(
      P1 = "copper",
      P2 = sample_name,
      P3 = other_spe,
      Dstatistic = Dstat,
      `p-value` = NA,
      `f4-ratio` = NA
    ) %>%
    select(P1, P2, P3, Dstatistic, `p-value`,`f4-ratio`)
  data <- rbind(data, extra_data)
  
  all_stats <- rbind(all_stats, data)
}

plot_data <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) 

#Add in empty copper row for phylogeny
empty_extra <- plot_data %>%
  group_by(P1, P2, species, latitude, longitude, population, minor_parent_ancestry, pop_short) %>%
  summarize() %>%
  mutate(P3 = "copper", Dstatistic = NA, `p-value` = NA, `f4-ratio` = NA)

plot_data <- rbind(plot_data, empty_extra)

# Heatmap plot
# 1. Define a helper function to get cluster order for each group
get_cluster_order <- function(data) {
  # We need a matrix where rows are P2 and columns are P3 (or vice versa)
  matrix_data <- data %>%
    select(P2, P3, Dstatistic) %>%
    pivot_wider(names_from = P3, values_from = Dstatistic, values_fill = 0) %>%
    column_to_rownames("P2")
  
  # Calculate distance and perform hierarchical clustering
  clustering <- hclust(dist(matrix_data))
  
  # Return a dataframe with the P2 names and their cluster rank
  tibble(
    P2 = rownames(matrix_data)[clustering$order],
    cluster_rank = 1:nrow(matrix_data)
  )
}

# 2. Apply clustering per population and plot
plot_data_clustered <- plot_data %>%
  filter(!is.na(P1)) %>%
  #group_by(population) %>%
  nest() %>%
  # Generate the order for each population
  mutate(order_df = map(data, get_cluster_order)) %>%
  # JOIN the order back into the nested data frames
  mutate(data = map2(data, order_df, ~left_join(.x, .y, by = "P2"))) %>%
  select(-order_df) %>% 
  unnest(data) %>%
  # Apply the facet-specific ordering
  mutate(P2_ordered = reorder_within(P2, cluster_rank, population))




##########

#Trying kolora tree
kolora_tree <- read.nexus("/project/ctb-grego/ntbsykes/copper_quillback/meta/allspecies_FigTree.20210311.tre")
common_names <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/broad_samples/common_names.txt",
                         col_names=c("science_name","common_name")) %>%
  mutate(science_name = case_when(science_name == "Reference_sample" ~ "schlegelii",
                                  science_name == "dallii" ~ "dalli",
                                  science_name == "chlorstictus" ~ "chlorostictus",
                                  T ~ science_name))
tree_species <- sub("^[^_]*_", "", kolora_tree$tip.label)
missing_from_tree <- common_names %>%
  filter(!(science_name %in% tree_species)) %>%
  select(science_name, common_name)

# Clean labels: "Genus_species" becomes "species"
cleaned_labels <- sub("^[^_]*_", "", kolora_tree$tip.label)

name_map <- setNames(common_names$common_name, common_names$science_name)

# Replace the cleaned labels with common names
# If a name isn't found in the map, it will return NA; 
# ifelse() ensures we keep the cleaned name if no match exists
new_labels <- ifelse(cleaned_labels %in% names(name_map), 
                     name_map[cleaned_labels], 
                     cleaned_labels)
kolora_tree$tip.label <- new_labels

tips_to_keep <- kolora_tree$tip.label[
  sub("^[^_]*_", "", kolora_tree$tip.label) %in% common_names$common_name & 
    kolora_tree$tip.label != "Outgroup"
]

# 2. Prune the tree
filtered_tree <- keep.tip(kolora_tree, tips_to_keep)

# # 1. Prepare Tree and Labels
# tree <- read.tree("/project/ctb-grego/ntbsykes/copper_quillback/broad_samples/broad.maf2.ref.phylogeny.subset.fa.speciescommon.treefile")
# tree_subset <- drop.tip(tree, "Outgroup")
# tree_subset$tip.label <- str_to_sentence(tree_subset$tip.label)
tree_subset <- filtered_tree
tree_subset$tip.label <- str_to_sentence(tree_subset$tip.label)

###Isolating individual introgression by testing groups
group_1 <- c("rougheye","baramenuke","redstripe","harlequin","pygmy","sharpchin",
             "darkblotched","yellowmouth","pop","northern","dusky",
             "light_dusky","yellowtail","black","widow","deacon","blue")

group_2 <- c("whitespeckled",
             "rosy","swordspine","greenspotted","greenblotched","rosethorn",
             "starry","buccaneer","patagonian","yelloweye","shortbelly",
             "chilipepper","boccaccio","cowcod", "redbanded","splitnose","tiger",
             "flag","treefish","blackgill","vermillion","canary")
group_3 <- c("greenstriped","stripetail","halfbanded","grass","brown",
             "calico","china","quillback","gopher","kelp","aurora",
             "copper")

threshold_D <- 0.05
outliers <- plot_data %>%
  mutate(group = case_when(P3 %in% group_1 ~ "group_1",
                           P3 %in% group_2 ~ "group_2",
                           P3 %in% group_3 ~ "group_3",
                           T ~ "other")) %>% 
  group_by(P2, group) %>%
  mutate(median_D_group = median(Dstatistic, na.rm=T),
         deviation = Dstatistic - median_D_group) %>%
  mutate(`p-value` = case_when(is.na(`p-value`) ~ 0,
                               T ~ `p-value`)) %>%
  mutate(outlier = case_when(deviation >= threshold_D & `p-value` < 0.0001 & Dstatistic > 0 & `f4-ratio` > 0.01~ "outlier",
                             T ~ NA)) %>%
  filter(outlier == "outlier") %>%
  filter(P3 != "aurora") %>%
  mutate(P3 = str_to_sentence(as.character(P3))) %>% 
  mutate(P3 = factor(P3, levels = tree_subset$tip.label)) %>%
  group_by(P2, group) %>%
  mutate( max_dev = max(deviation))


# Manual fix for the is.waive error
is.waive <- function(x) inherits(x, "waiver")



# 1. Prepare Heatmap Data (P3 on X-axis)
plot_data_heatmap <- plot_data_clustered %>%
  mutate(P3 = str_to_sentence(as.character(P3))) %>% 
  mutate(P3 = factor(P3, levels = tree_subset$tip.label))

# 2. Create the Heatmap (Labels at bottom)
p_main <- ggplot(plot_data_heatmap, aes(x = P3, y = P2_ordered, fill = Dstatistic)) +
  geom_tile() +
  geom_tile(data = inner_join(plot_data_heatmap, outliers), color="black") +
  geom_point(data = inner_join(plot_data_heatmap, outliers) %>%
               filter(max_dev == deviation),
             shape="o") +
  
  scale_y_reordered() + 
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-0.3, 0.3), oob = scales::squish,
    na.value = "grey90",
    name="D"
  ) +
  #facet_grid(population ~ ., scales = "free_y", space = "free_y") +
  theme(
    # Re-enable X axis text and rotate 90 degrees for the bottom
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,size=6),
    axis.title.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(y = "P2")

# 3. Create the Tree Plot (Dotted lines ONLY)
p_tree <- ggtree(tree_subset, layout = "dendrogram") + 
  geom_tiplab(
    align = TRUE, 
    linetype = "dotted", 
    # We set the label to be empty/invisible so only the line remains
    label = "", 
    size=0,
    offset = 0 
  ) +
  # Small ylim adjustment so the lines reach the edge of the plot
  ylim(0, 1.5) 



side_bar_data <- plot_data_heatmap %>%
  select(P2_ordered, population) %>%
  distinct()
p_side <- ggplot(side_bar_data, aes(x = 1, y = P2_ordered, fill = population)) +
  geom_tile() +
  scale_y_reordered() + 
  # Use a discrete color scale for populations
  scale_fill_manual(values = pop_palette, name = "Population") + 
  theme_void() + # Removes all background and axis text
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0, "lines"),
    legend.position = "none"
  )

heatmap_plot <- p_main %>% insert_top(p_tree, height = 0.2)

final_plot <- p_main %>% 
  insert_top(p_tree, height = 0.2) %>% 
  insert_right(p_side, width = 0.05) # Adjust width as needed


pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/broad_copper.heatmap.v2.pdf",
    height=12,width=5)
final_plot
dev.off()

#####Bar plots of introgression amounts

bar_plot <- plot_data_heatmap %>%
  full_join(outliers) %>%
  mutate(chosen_outlier = case_when(deviation == max_dev | 
                                      (P3 == "Quillback" & outlier == outlier) ~ "chosen",
                                    T ~ "not_chosen")) %>%
  mutate(intro_amount = case_when(chosen_outlier == "chosen" ~ `f4-ratio` * 100,
                                  T ~ 0))   %>%
  mutate(P3 = case_when(intro_amount == 0 ~ NA,
                        T ~ P3)) %>%
  group_by(P2) %>%
  mutate(max_intro = sum(intro_amount)) %>% 
  ungroup() %>%
  ggplot(.,aes(y=fct_reorder(P2, max_intro),x=intro_amount)) +
  geom_vline(xintercept = c(5,10),linetype="dotted") + 
  geom_col(position="stack",aes(fill = P3)) +
  facet_grid(pop_short~., scales = "free_y",space="free_y") +
  scale_fill_manual(values = c("Brown" = palette_1[10],
                               "Quillback" = "#0A369D",
                               "Black" = "black",
                               "Vermillion" = palette_1[2],
                               "Canary" = palette_2[1], 
                               "Yelloweye" = palette_2[9],
                               "Yellowtail" = palette_2[2]
                               ),
                    name="Donor Species",
                    breaks = c("Black","Brown","Canary","Quillback","Vermillion","Yelloweye","Yellowtail"),
                    na.translate = FALSE) +
  theme_cowplot() +
  theme(axis.text.y = element_blank()) +
  ylab("Sample") +
  xlab("Admixture amount (%)") +
  theme(
    axis.text.y = element_blank(),
    # c(x, y) coordinates: 0.5 is middle, 0.1 is near the bottom
    legend.position = c(0.5, 0.1), 
    # Anchors the legend at its bottom-center point to the coordinates above
    legend.justification = c(0.5, 0),
    legend.direction = "vertical",
    # Optional: Add a background so the bars don't make the text unreadable
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.box.background = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 10)) +
  geom_vline(xintercept=0,linetype="dotted")

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/broad_copper.proportions.v1.pdf",
    height=12,width=3)
bar_plot
dev.off()

####T-test for difference in admixture proportion
test_data <- plot_data_heatmap %>%
  full_join(outliers) %>%
  mutate(chosen_outlier = case_when(deviation == max_dev | 
                                      (P3 == "Quillback" & outlier == outlier) ~ "chosen",
                                    T ~ "not_chosen")) %>%
  mutate(intro_amount = case_when(chosen_outlier == "chosen" ~ `f4-ratio` * 100,
                                  T ~ 0))   %>%
  mutate(P3 = case_when(intro_amount == 0 ~ NA,
                        T ~ P3)) %>%
  group_by(P2, population) %>%
  summarize(max_intro = sum(intro_amount)) %>%
  mutate(admixed = case_when(max_intro > 0 ~ 1,
                             T ~ 0) ) %>%
  mutate(location = case_when(population %in% c("Salish Sea","Puget Sound") ~ "inside",
                              T ~ "outside")) 

t.test(admixed ~ location, data=test_data)
  
