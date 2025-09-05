library(tidyverse)
library(cowplot)

# set working directory
setwd("/project/ctb-grego/ntbsykes/copper_quillback")

species <- "copper"

# create the function ----------------------------------------------------------
#plot_fst_heatmap <- function(species){
  # read in the list of pairs
  coord_pairs <- read_tsv(paste0("fst/list_of_",species,"_pairs.txt"), col_names = "pair")
  # open up an empty frame
  fst <- tibble()
  # loop over the pairs
  for (coord_pair in coord_pairs$pair){
    pops <- str_split(coord_pair, "_")[[1]]
    pop1 <- pops[1]
    pop2 <- pops[2]
    # read in and summarise
    data <- read_tsv(paste0("fst/",species,"/",coord_pair,".fst")) %>%
      mutate(FstNum = ifelse(is.na(FstNum), 0, FstNum),
             FstDenom = ifelse(is.na(FstDenom), 0, FstDenom)) %>%
      summarise(fst = sum(FstNum) / sum(FstDenom)) %>%
      mutate(pop1 = pop1, pop2 = pop2) %>%
      dplyr::select(pop1, pop2, fst)
    # slap the new data onto the overall data frame
    fst <- rbind(fst, data)
  }
  # replace NA's with zero
  fst <- fst %>% mutate(fst = replace(fst, is.na(fst), 0))
  # create a mirrored data frame
  tsf <- fst %>% 
    mutate(tmp = pop2, pop2 = pop1, pop1 = tmp) %>%
    dplyr::select(-tmp)
  # now bind those together
  fst <- rbind(tsf, fst) %>%
    mutate(fst = ifelse(fst < 0, 0, fst))
  # plot the thing
  pdf(file = paste0("plots/fst_heatmap_",species,".pdf"), width = 10, height = 8)
  plt <- fst %>% 
    ggplot(., aes(pop1, pop2, fill = fst)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_cowplot() +
    labs(fill = "Fst",
         x = "Population 1 (sorted by latitude)",
         y = "Population 2 (sorted by latitude)") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())
  print(plt)
  dev.off()
}

plot_fst_heatmap("copper")
plot_fst_heatmap("quillback")

# testing here for grouping by population
species <- "copper"

pop_map <- read_tsv("meta/copper_quillback_sample_info.txt") %>%
  mutate(pop1 = paste0(round(latitude, 1), round(longitude, 1))) %>%
  dplyr::select(population, pop1) %>%
  unique()

coord_pairs <- read_tsv(paste0("fst/list_of_",species,"_pairs.txt"), col_names = "pair")
# open up an empty frame
fst <- tibble()
# loop over the pairs
for (coord_pair in coord_pairs$pair){
  pops <- str_split(coord_pair, "_")[[1]]
  pop1 <- pops[1]
  pop2 <- pops[2]
  # read in and summarise
  data <- read_tsv(paste0("fst/",species,"/",coord_pair,".fst")) %>%
    mutate(FstNum = ifelse(is.na(FstNum), 0, FstNum),
           FstDenom = ifelse(is.na(FstDenom), 0, FstDenom)) %>%
    summarise(fst = sum(FstNum) / sum(FstDenom)) %>%
    mutate(pop1 = pop1, pop2 = pop2) %>%
    dplyr::select(pop1, pop2, fst)
  # slap the new data onto the overall data frame
  fst <- rbind(fst, data)
}
# join with the population map for sorting later
fst <- inner_join(fst, pop_map)
# replace NA's with zero
fst <- fst %>% mutate(fst = replace(fst, is.na(fst), 0))
# create a mirrored data frame
tsf <- fst %>% 
  mutate(tmp = pop2, pop2 = pop1, pop1 = tmp) %>%
  dplyr::select(-tmp)
# now bind those together
fst <- rbind(tsf, fst) %>%
  mutate(fst = ifelse(fst < 0, 0, fst))
# plot the thing
pdf(file = paste0("plots/fst_heatmap_",species,".pdf"), width = 10, height = 8)
plt <- fst %>%
  arrange(pop1, pop2) %>%
  ggplot(., aes(pop1, pop2, fill = fst)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_cowplot() +
  labs(fill = "Fst",
       x = "Population 1 (sorted by latitude)",
       y = "Population 2 (sorted by latitude)") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
print(plt)
dev.off()
