#Process individual sample Dtrios

library(tidyverse)
library(patchwork)
library(yarrr)
library(stringr)
library(ape)
library(aplot)
library(ggtree)


scenarios <- c("scenario1","scenario2","scenario3")

all_stats <- tibble()
all_files <-  list.files("/project/ctb-grego/ntbsykes/copper_quillback/d_test_2/ind_runs_scenario1", pattern="BBAA.txt")

for (file in all_files){
  for (scenario in scenarios){
    sample_name <- sub("_BBAA\\.txt$", "", file)
    
    data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/d_test_2/ind_runs_",scenario,"/",file)) %>%
      select(P1, P2, P3, Dstatistic, `p-value`,`f4-ratio`)
    flipped_data <- data %>%
      mutate(P2_backup = P1,
             P1_backup = P2,
             D_backup = Dstatistic*-1) %>%
      mutate(P1 = P1_backup,
             P2 = P2_backup,
             Dstatistic = D_backup) %>%
      select(-P1_backup, -P2_backup, -D_backup)
    data <- rbind(data, flipped_data) %>% mutate(scenario = scenario) %>%
      filter(P1 == "p2") %>%
      mutate(group = case_when(
        # Matches i followed by 10 through 19
        grepl("p2", P2) ~ "unadmixed",
        # Matches i followed by 20 through 29
        grepl("p3", P2) ~ "admixed",
        TRUE ~ "other"
      ))

    
    all_stats <- rbind(all_stats, data)
  }
}

all_files <-  list.files("/project/ctb-grego/ntbsykes/copper_quillback/d_test_2/ind_runs_scenario4", pattern="BBAA.txt")

for (file in all_files){
  scenario <- "scenario4"
  sample_name <- sub("_BBAA\\.txt$", "", file)
  
  data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/d_test_2/ind_runs_",scenario,"/",file)) %>%
    select(P1, P2, P3, Dstatistic, `p-value`,`f4-ratio`)
  flipped_data <- data %>%
    mutate(P2_backup = P1,
           P1_backup = P2,
           D_backup = Dstatistic*-1) %>%
    mutate(P1 = P1_backup,
           P2 = P2_backup,
           Dstatistic = D_backup) %>%
    select(-P1_backup, -P2_backup, -D_backup) 
  data <- rbind(data, flipped_data) %>% mutate(scenario = scenario)
  data <- data %>% filter(P3 == "p2") %>%
    filter(P1 == "p3") %>%
    mutate(group = case_when(
      # Matches i followed by 10 through 19
      grepl("p3", P2) ~ "unadmixed",
      # Matches i followed by 20 through 29
      grepl("p4", P2) ~ "admixed",
      TRUE ~ "other"
    ))
  
  all_stats <- rbind(all_stats, data)
}


pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/simulation_Drel.v1.pdf",
    height=4,width=15)
all_stats %>% 
  mutate(pop = case_when(group == "admixed" ~ "Pop2",
                         group == "unadmixed" ~ "Pop1")) %>%
  ggplot(.,aes(x=fct_relevel(scenario, c("scenario3", "scenario2","scenario1","scenario4")) , y=Dstatistic)) +
  geom_boxplot(aes(fill=pop)) +
  theme_cowplot() +
  geom_hline(yintercept=0,linetype="dotted") +
  labs(y = expression(D[rel]),
       x = "") +
  scale_fill_brewer(palette = "Set1",
                    name="Population") +
  scale_x_discrete(
    labels = c("scenario3" = "Model 1", "scenario2" = "Model 2", "scenario1" = "Model 3",
               "scenario4" = "Model 4")
  )
dev.off()

