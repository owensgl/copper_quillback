library(tidyverse)
library(zoo)
library(cowplot)

setwd("/project/ctb-grego/ntbsykes/copper_quillback/")

chr_map <- read_tsv("meta/chr_map.txt", col_names = c("chr", "chr_n"))
chr_lengths <- read_tsv("ref/Ssc.HIC.genome.fa.fai",
                        col_names = c("chr", "length", "bits", 
                                      "spacer1", "spacer2")) %>%
  inner_join(chr_map) %>%
  filter(chr_n != "NA") %>%
  arrange(chr_n) %>%
  mutate(total = cumsum(length) - length) %>%
  dplyr::select(chr, chr_n, total, length)

chr_breaks <- tibble(chr = chr_lengths$chr,
                     start = chr_lengths$total,
                     end = chr_lengths$total+chr_lengths$length-1,
                     colours = c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)) %>%
  mutate(centre = ((end-start)/2)+start) %>%
  inner_join(chr_map)

sp_palette <- c("#daa038", "#483633")

# Plotting copper --------------------------------------------------------------
fst <- read_tsv("fst/inside_outside/copper.fst") %>%
  inner_join(chr_lengths) %>%
  mutate(position = pos + total)

fst <- fst %>%
  arrange(chr_n, pos) %>%
  mutate(mean_fst = zoo::rollmean(Fst, 1000, fill = NA))

pdf(file = "plots/fst_inside_other_copper.pdf", width = 12, height = 3)
plt <- fst %>%
  ggplot() +
  geom_rect(data = chr_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = as.factor(colours)),
            alpha = 0.1) +
  scale_fill_manual(values = c("grey50", "white")) +
  geom_line(aes(position, mean_fst), colour = sp_palette[1]) +
  scale_x_continuous(label = chr_breaks$chr_n,
                     breaks = chr_breaks$centre) +
  theme_cowplot() +
  theme(legend.position = "none") +
  ylim(0, 0.09) +
  labs(title = "Genome-wide Fst: Copper Inside vs. Other",
       x = "Chromosome",
       y = "Fst")
print(plt)
dev.off()

# Plotting quillback -----------------------------------------------------------
fst <- read_tsv("fst/quillback/inside_other.fst") %>%
  inner_join(chr_lengths) %>%
  mutate(position = pos + total)

fst <- fst %>%
  arrange(chr_n, pos) %>%
  mutate(mean_fst = zoo::rollmean(Fst, 1000, fill = NA))

pdf(file = "plots/fst_inside_other_quillback.pdf", width = 12, height = 3)
plt <- fst %>%
  ggplot() +
  geom_rect(data = chr_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = as.factor(colours)),
            alpha = 0.1) +
  scale_fill_manual(values = c("grey50", "white")) +
  geom_line(aes(position, mean_fst), colour = sp_palette[2]) +
  scale_x_continuous(label = chr_breaks$chr_n,
                     breaks = chr_breaks$centre) +
  theme_cowplot() +
  theme(legend.position = "none") +
  ylim(0, 0.09) +
  labs(title = "Genome-wide Fst: Quillback Inside vs. Other",
       x = "Chromosome",
       y = "Fst")
print(plt)
dev.off()