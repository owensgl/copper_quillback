#Process individual sample Dtrios from copper

library(tidyverse)
library(patchwork)
library(yarrr)
library(stringr)
library(cowplot)
# create colour palettes
pop_palette <- c("Alaska" = "#29335C",
                 "Hecate Strait" = "#6A679E",
                 "Queen Charlotte Sound" = "#009FFD",
                 "Salish Sea" = "#058C42",
                 "Vancouver Island" = "#FFB710",
                 "Puget Sound" = "#F75524",
                 "Washington & Oregon" = "#CB002D",
                 "California" = "#7B1919")
location_renamed <- tibble(population=c("Alaska","Hecate Strait","Queen Charlotte Sound",
                                        "Salish Sea","Vancouver Island","Puget Sound",
                                        "Washington & Oregon","California"),
                           pop_short = c("AK","HS","QCS","SS","VI","PS","WA/OR","CA")
)


sp_palette <- c("Copper" = "#daa038",
                "Quillback" = "#0A369D")
all_files <-  list.files("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/", pattern="Dmin")
non_empty_files <- all_files[file.info(file.path("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/", all_files))$size > 0]
sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/copper_quillback_summary.txt") %>%
  select(sample, species, latitude, longitude, population, minor_parent_ancestry)



all_stats <- tibble()
for (file in non_empty_files){
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
  
  all_stats <- rbind(all_stats, data)
}

plot_data <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  filter(population == "Hecate Strait") 

# Identify the top point for each P2 sample
top_bottom_points <- plot_data %>%
  group_by(P2) %>%
  {bind_rows(
    slice_min(., Dstatistic, n = 1),
    slice_max(., Dstatistic, n = 1)
  )} %>%
  ungroup()
#color_species <- calc_pal()(10)
# Create the plot with labels
p1 <- plot_data %>%
  filter(!is.na(P1)) %>%
  ggplot(aes(x = P2, y = Dstatistic, color = P3)) +
  geom_jitter(width = 0.1) +
  facet_grid(.~population, scales = "free_x",space="free_x") +
  theme_cowplot() +
  #theme(axis.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_hline(yintercept = 0,linetype="dashed")  +
  geom_text(data = top_bottom_points, 
            aes(label = P3), 
            vjust = -0.5, 
            hjust = 0.5,
            size = 3,
            show.legend = FALSE) 

p1

all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  filter(P3 == "quillback") %>%
  ggplot(.,aes(x=fct_reorder(P2, Dstatistic),y=Dstatistic)) +
  geom_point(aes(color=population)) +
  facet_grid(~pop_short,drop=T,scales="free_x",space="free_x") +
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0),
        strip.background = element_rect(fill = "lightblue", color = "black"),
        panel.spacing.x = unit(0.1, "lines"),
        legend.position = "none") +
  xlab("Copper Sample") +
  ylab("D") +
  geom_hline(yintercept = 0,linetype="dashed") +
  #ylim(-0.12,0.12) +
  coord_cartesian(clip = 'off') +
  scale_color_manual(values=pop_palette)








result <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  group_by(P2) %>%
  arrange(desc(Dstatistic)) %>%
  summarise(
    # Get top 3 Dstatistic values and corresponding P3 values
    top1_D = nth(Dstatistic, 1),
    top2_D = nth(Dstatistic, 2),
    top3_D = nth(Dstatistic, 3),
    top1_P3 = nth(P3, 1),
    top2_P3 = nth(P3, 2),
    top3_P3 = nth(P3, 3),
    top1_pvalue = nth(`p-value`, 1),
    top1_f4= nth(`f4-ratio`, 1),
    
    # Calculate differences
    diff_1st_2nd = top1_D - top2_D,
    diff_2nd_3rd = top2_D - top3_D,
    
    # Apply your logic
    result_P3 = case_when(
      top1_pvalue > 0.0001 ~ "None",
      diff_1st_2nd > 0.01 ~ top1_P3,
      diff_2nd_3rd > 0.01 ~ paste(sort(c(top1_P3, top2_P3)), collapse = "/"),
      TRUE ~ "None"  # or some default value
    ),
    confidence = case_when(
      top1_pvalue > 0.0001 ~ "None",
      diff_1st_2nd > 0.02 | diff_2nd_3rd > 0.02 ~ "High",
      diff_1st_2nd > 0.01 | diff_2nd_3rd > 0.01 ~ "Low",
      TRUE ~ "None"  # or some default value
    ),
    .groups = 'drop'
  ) %>% 
  select(P2, result_P3, top1_pvalue, top1_f4, top1_D, confidence)

result %>%
  filter(result_P3 == "None") %>%
  select(P2) %>%
  write_tsv("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/unadmixed.samples.txt")

palette_1 <- unname(piratepal("basel"))
palette_2 <- unname(piratepal("pony"))
target_species <- c("aurora","brown","china", "rougheye","black","deacon","vermillion",
                    "yelloweye","quillback","whitespeckled","boccaccio",
                    "canary","widow")
#No admixture plot
plot_data <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  inner_join(result) %>%
  mutate(P2 = fct_reorder(P2, top1_D)) %>%
  mutate(species_color = case_when(!P3 %in% target_species ~ "other",
                                   TRUE ~ P3)) %>%
  filter(confidence == "None") %>%
  mutate(species_color = str_to_title(species_color)) %>%
  mutate(species_color = fct_relevel(species_color, "Other", after=Inf)) 



# Create color blocks data
color_blocks <- plot_data %>%
  select(P2, population, result_P3) %>%
  distinct() %>%
  mutate(y_position = min(plot_data$Dstatistic) - 0.05)  # Position below the lowest point



# Main plot
plot_none <- plot_data %>%
  ggplot(aes(x = P2, y = Dstatistic, color =species_color)) +
  geom_jitter(data = . %>% filter(species_color == "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 0.4, 
              size = 1) +  # Smaller size
  # Second layer: species points (normal size)
  geom_jitter(data = . %>% filter(species_color != "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 1.0, 
              size = 2) +
  # Add color blocks at the bottom
  geom_tile(data = color_blocks, 
            aes(x = P2, y = y_position, fill = population), 
            height = 0.02, width = 0.8, inherit.aes = FALSE) +
  facet_grid(.~result_P3, scales = "free_x", space="free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # Adjust y-axis limits to show color blocks
  expand_limits(y = min(plot_data$Dstatistic) - 0.08) +
  scale_fill_manual(values=pop_palette, guide = "none") +
  scale_color_manual(values = c("Aurora" = palette_1[2], "Brown" = palette_1[10],
                                "Quillback" = "#0A369D", "China" = palette_1[4],
                                "Deacon" = palette_1[5], "Vermillion" = palette_1[6],
                                "Rougheye" = palette_1[7], "Whitespeckled" = palette_1[8],
                                "Boccaccio" = palette_1[9],
                                "Canary" = palette_2[3], Black = "black",
                                "Yelloweye" = palette_2[9], "Widow" = palette_2[4],
                                "Other" = "grey"),
                     name="P3") +
  theme(legend.position = "None") + 
  ylab("D-score")



#Low confidence admixture
plot_data <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  inner_join(result) %>%
  mutate(P2 = fct_reorder(P2, top1_D)) %>%
  mutate(species_color = case_when(!P3 %in% target_species ~ "other",
                                   TRUE ~ P3)) %>%
  filter(confidence == "Low") %>%
  mutate(species_color = str_to_title(species_color)) %>%
  mutate(species_color = fct_relevel(species_color, "Other", after=Inf)) 



# Create color blocks data
color_blocks <- plot_data %>%
  select(P2, population, result_P3) %>%
  distinct() %>%
  mutate(y_position = min(plot_data$Dstatistic) - 0.05)  # Position below the lowest point



# Main plot
plot_low <- plot_data %>%
  ggplot(aes(x = P2, y = Dstatistic, color =species_color)) +
  geom_jitter(data = . %>% filter(species_color == "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 0.4, 
              size = 1) +  # Smaller size
  # Second layer: species points (normal size)
  geom_jitter(data = . %>% filter(species_color != "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 1.0, 
              size = 2) +
  # Add color blocks at the bottom
  geom_tile(data = color_blocks, 
            aes(x = P2, y = y_position, fill = population), 
            height = 0.02, width = 0.8, inherit.aes = FALSE) +
  facet_grid(.~result_P3, scales = "free_x", space="free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # Adjust y-axis limits to show color blocks
  expand_limits(y = min(plot_data$Dstatistic) - 0.08) +
  scale_fill_manual(values=pop_palette, guide = "none") +
  scale_color_manual(values = c("Aurora" = palette_1[2], "Brown" = palette_1[10],
                                "Quillback" = "#0A369D", "China" = palette_1[4],
                                "Deacon" = palette_1[5], "Vermillion" = palette_1[6],
                                "Rougheye" = palette_1[7], "Whitespeckled" = palette_1[8],
                                "Boccaccio" = palette_1[9],
                                "Canary" = palette_2[3], Black = "black",
                                "Yelloweye" = palette_2[9], "Widow" = palette_2[4],
                                "Other" = "grey"),
                     name="P3") +
  theme(legend.position="none") +
  ylab("D-score")

#High confidence admixture
plot_data <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  inner_join(result) %>%
  mutate(P2 = fct_reorder(P2, top1_D)) %>%
  mutate(species_color = case_when(!P3 %in% target_species ~ "other",
                                   TRUE ~ P3)) %>%
  filter(confidence == "High") %>%
  mutate(species_color = str_to_title(species_color)) %>%
  mutate(species_color = fct_relevel(species_color, "Other", after=Inf)) 



# Create color blocks data
color_blocks <- plot_data %>%
  select(P2, population, result_P3) %>%
  distinct() %>%
  mutate(y_position = min(plot_data$Dstatistic) - 0.05)  # Position below the lowest point



# Main plot
plot_high <- plot_data %>%
  ggplot(aes(x = P2, y = Dstatistic, color =species_color)) +
  geom_jitter(data = . %>% filter(species_color == "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 0.4, 
              size = 1) +  # Smaller size
  # Second layer: species points (normal size)
  geom_jitter(data = . %>% filter(species_color != "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 1.0, 
              size = 2) +
  # Add color blocks at the bottom
  geom_tile(data = color_blocks, 
            aes(x = P2, y = y_position, fill = population), 
            height = 0.02, width = 0.8, inherit.aes = FALSE) +
  facet_grid(.~result_P3, scales = "free_x", space="free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # Adjust y-axis limits to show color blocks
  expand_limits(y = min(plot_data$Dstatistic) - 0.08) +
  scale_fill_manual(values=pop_palette, guide = "none") +
  scale_color_manual(values = c("Aurora" = palette_1[2], "Brown" = palette_1[10],
                                "Quillback" = "#0A369D", "China" = palette_1[4],
                                "Deacon" = palette_1[5], "Vermillion" = palette_1[6],
                                "Rougheye" = palette_1[7], "Whitespeckled" = palette_1[8],
                                "Boccaccio" = palette_1[9],
                                "Canary" = palette_2[3], Black = "black",
                                "Yelloweye" = palette_2[9], "Widow" = palette_2[4],
                                "Other" = "grey"),
                     name="P3") +
  theme(legend.position = "none") +
  ylab("D-score")

plot_legend <- plot_data %>%
  ggplot(aes(x = P2, y = Dstatistic, color =species_color)) +
  geom_jitter(data = . %>% filter(species_color == "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 0.4, 
              size = 1) +  # Smaller size
  # Second layer: species points (normal size)
  geom_jitter(data = . %>% filter(species_color != "Other"),
              aes(color = species_color), 
              width = 0.1, 
              alpha = 1.0, 
              size = 2) +
  # Add color blocks at the bottom
  geom_tile(data = color_blocks, 
            aes(x = P2, y = y_position, fill = population), 
            height = 0.02, width = 0.8, inherit.aes = FALSE) +
  facet_grid(.~result_P3, scales = "free_x", space="free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # Adjust y-axis limits to show color blocks
  expand_limits(y = min(plot_data$Dstatistic) - 0.08) +
  scale_fill_manual(values=pop_palette, guide = "none") +
  scale_color_manual(values = c("Aurora" = palette_1[2], "Brown" = palette_1[10],
                                "Quillback" = "#0A369D", "China" = palette_1[4],
                                "Deacon" = palette_1[5], "Vermillion" = palette_1[6],
                                "Rougheye" = palette_1[7], "Whitespeckled" = palette_1[8],
                                "Boccaccio" = palette_1[9],
                                "Canary" = palette_2[3], Black="black",
                                "Yelloweye" = palette_2[9], "Widow" = palette_2[4],
                                "Other" = "grey"),
                     name="P3") +
  theme(legend.position = "bottom")


plot_high
plot_low


###


desired_order <- c("aurora","black","boccaccio", "brown","canary","canary/vermillion","china", "deacon",
                   "deacon/widow",
                   "quillback", "quillback/yelloweye","rougheye","whitespeckled",
                   "yelloweye","None")

plot_blocks <- inner_join(result %>% rename(sample=P2), sample_info) %>%
  group_by(population, result_P3) %>%
  summarize(
    n=n()
  ) %>%
  ungroup() %>% 
  mutate(result_P3 = factor(result_P3, levels = desired_order)) %>%
  ggplot(
    # Keep only data for wheat
    aes(values=n, fill=result_P3)
  )+
  waffle::geom_waffle(
    n_rows = 5,        # Number of squares in each row
    color = "white",   # Border color
    flip = TRUE, na.rm=TRUE
  )+
  facet_grid(~population)+
  coord_equal() +
  scale_fill_manual(values = c("aurora" = palette_1[2], "brown" = palette_1[10],
                                "quillback" = "#0A369D", "quillback/yelloweye" = "#0A369D",
                               "china" = palette_1[4],"black" = "black",
                                "deacon" = palette_1[5], "deacon/widow" = palette_1[5],
                               "vermillion" = palette_1[6],
                                "rougheye" = palette_1[7], "whitespeckled" = palette_1[8],
                                "boccaccio" = palette_1[9],
                                "canary" = palette_2[3], "canary/vermillion" = palette_2[3],
                                "yelloweye" = palette_2[9], "widow" = palette_2[4],
                                "None" = "grey"),
                    name="Admixed") +
  theme(
    # Enable markdown for title and subtitle
    # "Clean" facets 
    panel.background=element_rect(fill="white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    strip.background.x = element_rect(fill="white"),
    strip.background.y = element_rect(fill="dimgrey"),
    strip.text.y = element_text(color="white"),
    legend.position = "none"
  )

plot_blocks




plot_bars <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  inner_join(result) %>%
  mutate(P2 = fct_reorder(P2, top1_D)) %>%
  mutate(species_color = case_when(!P3 %in% target_species ~ "other",
                                   TRUE ~ P3)) %>%
  mutate(species_color = str_to_title(species_color)) %>%
  mutate(species_color = fct_relevel(species_color, "Other", after=Inf))  %>% 
  group_by(P2) %>%
  slice_max(., Dstatistic, n = 1) %>%
  mutate(admixture_amount = case_when(result_P3 == "None" ~ 0,
                                      TRUE ~ `top1_f4`*100)) %>%
  ggplot(.,aes(x=fct_reorder(P2, admixture_amount),y=admixture_amount)) +
  geom_col(aes(fill = result_P3)) +
  facet_grid(.~pop_short, scales = "free_x",space="free_x") +
  scale_fill_manual(values = c("aurora" = palette_1[2], "brown" = palette_1[10],
                               "quillback" = "#0A369D", "quillback/yelloweye" = "#0A369D",
                               "china" = palette_1[4],"black" = "black",
                               "deacon" = palette_1[5], "deacon/widow" = palette_1[5],
                               "vermillion" = palette_1[6],
                               "rougheye" = palette_1[7], "whitespeckled" = palette_1[8],
                               "boccaccio" = palette_1[9],
                               "canary" = palette_2[3], "canary/vermillion" = palette_2[3],
                               "yelloweye" = palette_2[9], "widow" = palette_2[4],
                               "None" = "grey"),
                    name="Donor Species") +
  theme_cowplot() +
  theme(axis.text.x = element_blank()) +
  xlab("Sample") +
  ylab("Admixture amount (%)") +
  theme(legend.position = "none")
plot_bars

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/admixture_broad_copper.v2.pdf",
    height=14,width=10)
plot_high / plot_low / plot_none /plot_blocks / plot_bars + plot_annotation(tag_levels = 'A')
dev.off()



pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/admixture_broad_copper_legend.v1.pdf",
    height=4,width=10)
plot_legend
dev.off()


####inside vs outside
all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  inner_join(result) %>%
  mutate(P2 = fct_reorder(P2, top1_D)) %>%
  mutate(species_color = case_when(!P3 %in% target_species ~ "other",
                                   TRUE ~ P3)) %>%
  mutate(species_color = str_to_title(species_color)) %>%
  mutate(species_color = fct_relevel(species_color, "Other", after=Inf))  %>% 
  group_by(P2) %>%
  slice_max(., Dstatistic, n = 1) %>%
  mutate(admixture_amount = case_when(result_P3 == "None" ~ 0,
                                      TRUE ~ `top1_f4`*100)) %>%
  mutate(area = case_when(population == "Salish Sea" | population == "Puget Sound" ~ "Inside",
                          TRUE ~ "Outside")) %>%
  ggplot(.,aes(x=area,y=admixture_amount)) +
  geom_boxplot() +
  theme_cowplot() +
  xlab("Region") + ylab("Admixture amount (%)")

all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  inner_join(result) %>%
  mutate(P2 = fct_reorder(P2, top1_D)) %>%
  mutate(species_color = case_when(!P3 %in% target_species ~ "other",
                                   TRUE ~ P3)) %>%
  mutate(species_color = str_to_title(species_color)) %>%
  mutate(species_color = fct_relevel(species_color, "Other", after=Inf))  %>% 
  group_by(P2) %>%
  slice_max(., Dstatistic, n = 1) %>%
  mutate(admixture_amount = case_when(result_P3 == "None" ~ 0,
                                      TRUE ~ `top1_f4`*100)) %>%
  mutate(admixture_binary = case_when(result_P3 == "None" ~ 0,
                                      TRUE ~ 1)) %>%
  mutate(area = case_when(population == "Salish Sea" | population == "Puget Sound" ~ "Inside",
                          TRUE ~ "Outside")) %>%
  t.test(admixture_binary ~ area, data = .)

###Correlation plot
quill_pca <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback.snprelate") %>%
  select(sample, PC1, PC2, PC3, PC4, PC5)

p3 <- all_stats %>%
  filter(P1 == "quillback") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  inner_join(location_renamed) %>%
  inner_join(copper_D) %>%
  #filter(copper_D < -0.02) %>%
  filter(P3 == "yelloweye") %>% 
  inner_join(., quill_pca %>% rename(P2 = sample)) %>%
  ggplot(.,aes(y=PC1, x=Dstatistic)) +
  geom_point() + geom_smooth(linetype="dashed",se=F,color="#0A369D") +
  xlab("Yelloweye D-score") +
  ylab("Quillback PC1") +
  theme_cowplot()

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/admixture_broad_correlation.v1.pdf",
    height=6,width=6)
p3
dev.off()
