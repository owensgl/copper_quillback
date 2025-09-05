pi <- tibble()

for (chr in 1:24){
  data <- read_tsv(paste0("/project/ctb-grego/ntbsykes/copper_quillback/pixy/genome_hic_scaffold_",chr,"_pi.txt")) %>%
    filter(no_sites >= 200) %>%
    mutate(weighted_pi = avg_pi * no_sites) %>%
    mutate(chromosome = chr)
  
  
  pi <- rbind(pi, data)
}

pi %>%
  group_by(chromosome, pop) %>%
  summarize(mean_pi = sum(weighted_pi)/sum(no_sites)) 
  
pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/pixy_pi_boxplot.v2.pdf", width = 3, height = 3)
plt <- pi %>%
  group_by(chromosome, pop) %>%
  summarize(mean_pi = sum(count_diffs)/sum(count_comparisons)) %>%
  ggplot(aes(x = pop, y = mean_pi, fill = pop)) +
  geom_boxplot() +
  scale_fill_manual(values = sp_palette) +
  labs(x = "Species",
       y = "Mean Pi") +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank())
print(plt)
dev.off()
pi %>%
  group_by(pop) %>%
  summarize(mean_pi = sum(count_diffs)/sum(count_comparisons)) %>%
  ggplot(aes(x = pop, y = mean_pi, fill = pop)) +
  geom_boxplot() +
  scale_fill_manual(values = sp_palette) +
  labs(x = "Species",
       y = "Mean Pi") +
  theme_cowplot() +
  theme(legend.position = "none",
        axis.title.x = element_blank())
dev.off()

pi 
