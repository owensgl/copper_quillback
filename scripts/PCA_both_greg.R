library(tidyverse)
library(cowplot)
library(patchwork)
library(SNPRelate)
library(zoo)


# load sample info
sample_info <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/meta/sample_info.txt")

# create colour palettes
pop_palette <- c("Alaska" = "#29335C",
                 "Hecate Strait" = "#6A679E",
                 "Queen Charlotte Sound" = "#009FFD",
                 "Salish Sea" = "#058C42",
                 "Vancouver Island" = "#FFB710",
                 "Puget Sound" = "#F75524",
                 "Washington & Oregon" = "#CB002D",
                 "California" = "#7B1919")


sp_palette <- c("Copper" = "#daa038",
                "Quillback" = "#0A369D")



snpgdsVCF2GDS("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/genome_hic_combined.missing5.vcf.gz",
              "/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/genome_hic_combined.missing5.gds")
genofile <- snpgdsOpen("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/genome_hic_combined.missing5.gds")

snpset <- snpgdsLDpruning(genofile, ld.threshold=0.2, autosome.only = F)
snpset.id <- unlist(unname(snpset))


# run and plot the PCA
pca <- snpgdsPCA(genofile, num.thread=2, eigen.cnt=10,autosome.only = F, snp.id = snpset.id)

samples <- pca$sample.id # extract sample names from PCA object
pcs <- pca$eigenvect # extract PCs

# combine into a tibble
pca_plotting <- as_tibble(cbind(sample = samples, pcs)) %>%
  mutate(across(starts_with("V"), as.numeric)) %>%
  rename_with(~paste0("PC", 1:10), starts_with("V"))

# write out those PCA results so that you can load them in more easily later

# isolate and write out the percent variance explained by each PC
pve <- tibble(pca$varprop[1:10])

# join with sample_info
pca_plotting <- inner_join(pca_plotting, sample_info)

missing <- read_table("/project/ctb-grego/ntbsykes/copper_quillback/vcf/structure/genome_hic_combined.missing5.missing.imiss") %>%
  select(INDV, F_MISS) %>%
  rename(sample = INDV)

plot1 <- pca_plotting %>%
  inner_join(missing) %>%
  ggplot(., aes(PC1, PC2, colour = species)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = sp_palette) +
  xlab(paste0("PC1 (", round(pca$varprop[1]*100, 2), "%)")) +
  ylab(paste0("PC2 (", round(pca$varprop[2]*100, 2), "%)")) +
  theme_cowplot() +
  theme(legend.position = "none") +
  labs(colour = "Species")

plot2 <- pca_plotting %>%
  inner_join(missing) %>%
  ggplot(., aes(PC1, PC, color=population)) +
  geom_point(size = 2, alpha = 0.9) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  xlab(paste0("PC1 (", round(pca$varprop[1]*100, 2), "%)")) +
  ylab(paste0("PC3 (", round(pca$varprop[3]*100, 2), "%)")) +
  theme_cowplot() +
  theme(legend.position = "none") +
  labs(colour = "Proportion missing",shape="Species")

plot3 <- pca_plotting %>%
  inner_join(missing) %>%
  ggplot(., aes(PC1, F_MISS, color = species)) +
  geom_point(size = 2, alpha = 0.9) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = sp_palette) +
  xlab("Interspecies PC1") +
  ylab("Proportion missing data" ) +
  theme_cowplot() +
  theme(legend.position = "none") +
  labs(colour = "Proportion missing",shape="Species") +
  facet_wrap(~species,scales="free_x") +
  geom_smooth(method = "lm", se=F,color="grey",linetype="dashed") +
  scale_x_continuous(n.breaks = 3)   # Only 4 breaks on x-axis
  

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/pca_both_greg.v2.pdf",
    height=3,width=8)
plot1 + plot3
dev.off()
