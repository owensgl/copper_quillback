library(tidyverse)
library(patchwork)
library(yarrr)
library(stringr)
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
all_files <-  list.files("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/", pattern="Dmin")
non_empty_files <- all_files[file.info(file.path("/project/ctb-grego/ntbsykes/copper_quillback/broad_copper/ind_runs/", all_files))$size > 0]
sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/copper_quillback_summary.txt") %>%
  select(sample, species, latitude, longitude, population, minor_parent_ancestry)



all_stats <- tibble()
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


plot_data_copper <- all_stats %>%
  filter(P1 == "copper") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  select(P2, P3, Dstatistic, `f4-ratio`,`p-value`) %>%
  rename(sample = P2) %>%
  mutate(admixture_amount = case_when(`p-value`> 0.05 ~ 0,
                                      Dstatistic < 0 ~ 0,
                                      T ~ `f4-ratio`))


###Emu plotting Copper

emu_pca_copper <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/copper_combined.emu.eigvecs") %>%
  select(-`#FID`) %>%
  rename(sample = IID) %>%
  inner_join(sample_info)

brown_plot_1 <- emu_pca_copper %>% 
  inner_join(plot_data_copper %>% filter(P3 == "brown")) %>%
  ggplot(., aes(PC2, admixture_amount, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  scale_shape_manual(name="Species", values=c(1,2)) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm"),      # Shrinks the actual color boxes
    legend.position = "none"
  ) +
  xlab("PC2") + ylab("Brown f4 score") +
  ggtitle("Copper")

emu_pca_copper %>% 
  inner_join(plot_data_copper %>% filter(P3 == "quillback")) %>%
  ggplot(., aes(PC1, admixture_amount*100)) +
  geom_point(aes(colour = population), size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  scale_shape_manual(name="Species", values=c(1,2)) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm"),      # Shrinks the actual color boxes
    legend.position = "none"
  ) +
  xlab("PC1") + ylab("Quillback admixture (%)") +
  ggtitle("Copper")


#####


quill_samples <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback.nosex",
                          col_names = c("sample","blank")) %>% select(-blank)
quill_structure <- read_table("/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback.2.Q",
                              col_names = c("Q1","Q2"))
quill_structure <- cbind(quill_samples, quill_structure)

all_files <-  list.files("/project/ctb-grego/ntbsykes/copper_quillback/broad_samples/ind_runs/", pattern="BBAA")

all_stats <- tibble()
for (file in all_files){
  sample_name <- sub("_BBAA\\.txt$", "", file)
  data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/broad_samples/ind_runs/",file)) %>%
    select(P1, P2, P3, Dstatistic, `p-value`,`f4-ratio`)
  flipped_data <- data %>%
    mutate(P2_backup = P1,
           P1_backup = P2,
           D_backup = Dstatistic*-1) %>%
    mutate(P1 = P1_backup,
           P2 = P2_backup,
           Dstatistic = D_backup) %>%
    select(-P1_backup, -P2_backup, -D_backup)
  data <- rbind(data, flipped_data)
  extra_data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/broad_samples/ind_runs/",file)) %>%
    select(P1, P2, P3, Dstatistic, `p-value`,`f4-ratio`, BBAA, ABBA, BABA) %>%
    filter(((grepl("quillback",P1) | grepl("quillback",P2)) & grepl("ABL",P3) | 
              (grepl("ABL",P1) | grepl("ABL",P2)) & grepl("quillback",P3))) %>%
    
    mutate(Dstat1 = (ABBA- BBAA)/(ABBA+BBAA),
           Dstat2 = (BBAA - ABBA)/(BBAA +ABBA),
           Dstat3 = (BBAA- BABA)/(BBAA+BABA),
           Dstat4 = (BABA - BBAA)/(BABA +BBAA)) %>% 
    mutate(Dstat = case_when((P1 == "quillback" & grepl("ABL",P3)) ~ Dstat1,
                             (P3 == "quillback" & grepl("ABL",P1)) ~ Dstat2,
                             (P2 == "quillback" & grepl("ABL",P3)) ~ Dstat4,
                             (P3 == "quillback" & grepl("ABL",P2)) ~ Dstat3
    )) %>%
    mutate(other_spe = case_when((P1 == "quillback" & grepl("ABL",P3)) ~ P2,
                                 (P3 == "quillback" & grepl("ABL",P1)) ~ P2,
                                 (P2 == "quillback" & grepl("ABL",P3)) ~ P1,
                                 (P3 == "quillback" & grepl("ABL",P2)) ~ P1 
    )) %>%
    mutate(
      P1 = "quillback", 
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

# Create the base data
plot_data_quillback <- all_stats %>%
  filter(P1 == "quillback") %>%
  filter(grepl("ABL",P2)) %>%
  inner_join(sample_info %>% rename(P2 = sample)) %>%
  select(P2, P3, Dstatistic, `f4-ratio`,`p-value`) %>%
  rename(sample = P2) %>%
  mutate(admixture_amount = case_when(`p-value`> 0.05 ~ 0,
                                      Dstatistic < 0 ~ 0,
                                      T ~ `f4-ratio`))


emu_pca_quillback <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/quillback_combined.emu.eigvecs") %>%
  select(-`#FID`) %>%
  rename(sample = IID) %>%
  inner_join(sample_info)


yelloweye_plot_1 <-emu_pca_quillback %>% 
  inner_join(plot_data_quillback %>% filter(P3 == "yelloweye")) %>%
  mutate(Dtmp = case_when(Dstatistic < 0 ~ 0,
                          T~ Dstatistic)) %>%
  ggplot(., aes(PC1, admixture_amount, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  scale_shape_manual(name="Species", values=c(1,2)) +
  theme_cowplot() +
  #theme(legend.position = "none") +
  labs(colour = "Population")+
  guides(colour = guide_legend(position = "bottom",ncol = 1)) +
  theme(
    legend.text = element_text(size = 8),   # Size of the color labels
    legend.title = element_text(size = 9, face = "bold"), # Size of "Population"
    legend.key.size = unit(0.4, "cm"),      # Shrinks the actual color boxes
    legend.position = "none"
  ) +
  xlab("PC1") + ylab("Yelloweye f4 score") +
  ggtitle("Quillback")


pdf("plots/admixture_causes_PC.v1.pdf",
    height=4,width=8)
brown_plot_1 + yelloweye_plot_1
dev.off()
