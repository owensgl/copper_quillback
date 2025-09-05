library(tidyverse)
library(cowplot)

# set working directory
setwd("/project/ctb-grego/ntbsykes/copper_quillback")

species <- "quillback"


coord_pairs <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/fst/quillback/locale/list_of_pairfiles.txt"), col_names = "pair") %>%
  mutate(pair = str_replace(pair,".txt","" ))
# open up an empty frame
fst <- tibble()
# loop over the pairs
for (coord_pair in coord_pairs$pair){
  pops <- str_split(coord_pair, "_")[[1]]
  pop1 <- pops[1]
  pop2 <- pops[2]
  # read in and summarise
  data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/fst/",species,"/locale/",coord_pair,".fst")) %>%
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
#pdf(file = paste0("plots/fst_heatmap_",species,".pdf"), width = 10, height = 8)
plt <- fst %>% 
  filter(pop1 > pop2) %>%
  mutate(across(c(pop1, pop2), ~ case_when(
    .x == "WO" ~ "WA/OR",
    .x == "VI" ~ "WVI",
    .x == "QC" ~ "QCS",
    TRUE ~ .x
  ))) %>%
  ggplot(., aes(fct_relevel(pop1,"WA/OR", after=Inf),
                            pop2, fill = fst)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_cowplot() +
  labs(fill = expression(F[ST]),
       x = "Population 1 (sorted by latitude)",
       y = "Population 2 (sorted by latitude)") +
  scale_y_discrete(position = "right") +
  theme(axis.title = element_blank()) 

print(plt)
sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/sample_info.txt") %>%
  mutate(lat = round(latitude), long=round(longitude)) %>%
  mutate(location = paste0(lat, "_",long)) %>%
  mutate(type = case_when(population == "Salish Sea" | population == "Puget Sound" ~ "Inside",
                          TRUE ~ "Outside")) %>% 
  select(location, species, type) %>%
  mutate(type = case_when(location == "49_-125" & species == "Quillback" ~ "Outside",
                          TRUE ~ type)) %>%
  unique()

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





#pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/isolation_by_distance.v1.pdf",
    height=3.5,width=9)
plt2 <- quillback_fst_summary %>%
  ggplot(.,aes(x=distance_km,y=fst,color=comparison)) +
  geom_point(alpha=0.9) +
  geom_smooth(method="lm",se=F) +
  #facet_wrap(~species) +
  theme_cowplot() +
  ylab(expression(F[ST])) +
  xlab("Distance (km)") +
  scale_color_viridis_d(name="Comparison") +
  geom_hline(yintercept = 0,linetype="dotted")
dev.off()

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/quillback_fst_alone.pdf",
  height=5,width=12)
plt + plt2 +
  plot_annotation(
    title = 'Quillback')
dev.off()
