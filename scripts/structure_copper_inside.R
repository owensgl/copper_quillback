# load required packages
library(tidyverse)
library(cowplot)
library(patchwork)
library(SNPRelate)
library(zoo)


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

# between species --------------------------------------------------------------
# Convert PLINK files to GDS format
path <- "/project/ctb-grego/ntbsykes/copper_quillback/vcf/inside_copper/combined.copper.inside."
vcf.fn <- paste0(path,"vcf.gz")

gds.fn <- paste0(path,"gds")

snpgdsVCF2GDS(vcf.fn, gds.fn)
genofile <- snpgdsOpen(gds.fn)
snpset <- snpgdsLDpruning(genofile, ld.threshold=0.2, autosome.only=F)
snpset.id <- unlist(unname(snpset))

# run and plot the PCA
pca <- snpgdsPCA(genofile, num.thread=2, eigen.cnt=10, autosome.only=F, snp.id = snpset.id)

samples <- pca$sample.id # extract sample names from PCA object
pcs <- pca$eigenvect # extract PCs

# combine into a tibble
pca_plotting <- as_tibble(cbind(sample = samples, pcs)) %>%
  mutate(across(starts_with("V"), as.numeric)) %>%
  rename_with(~paste0("PC", 1:10), starts_with("V"))

missing_data <- read_table("/project/ctb-grego/ntbsykes/copper_quillback/vcf/inside_copper/combined.copper.inside.imiss") %>%
  select(INDV, F_MISS) %>%
  rename(sample = INDV)

# isolate and write out the percent variance explained by each PC
pve <- tibble(pca$varprop[1:10])

# join with sample_info
pca_plotting <- inner_join(pca_plotting, sample_info) %>%
  inner_join(missing_data)

# plot it
plot_1 <- pca_plotting %>%
  ggplot(., aes(PC1, PC2, colour = population)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_manual(values = pop_palette) +
  xlab(paste0("PC1 (", round(pca$varprop[1]*100, 2), "%)")) +
  ylab(paste0("PC2 (", round(pca$varprop[2]*100, 2), "%)")) +
  theme_cowplot() +
  labs(colour = "Population")

plot_2 <- pca_plotting %>%
  ggplot(., aes(PC1, PC2, colour = F_MISS)) +
  geom_point(size = 2, alpha = 0.7) +
  #stat_ellipse(type = "norm", level = 0.95, aes(group = population)) +
  scale_colour_viridis_c() +
  xlab(paste0("PC1 (", round(pca$varprop[1]*100, 2), "%)")) +
  ylab(paste0("PC2 (", round(pca$varprop[2]*100, 2), "%)")) +
  theme_cowplot() +
  labs(colour = "Proportion\nmissing")
##### Admixture

samples <- read_tsv("/project/ctb-grego/ntbsykes/copper_quillback/structure/copper_inside/combined.copper.inside.nosex",
                    col_names = c("sample", "trash")) %>%
  subset(select = 1)

# Make an empty data frame to slap all this stuff into
copper_admix <- tibble(sample = character(),
                       k = numeric(),
                       Q = character(),
                       value = numeric())

# Loop up the reading for each k
for (k in 1:10){
  data <- read.delim(paste0("/project/ctb-grego/ntbsykes/copper_quillback/structure/copper_inside/combined.copper.inside.",k,".Q"),
                     header = FALSE,
                     col.names = paste0("Q",seq(1:k)),
                     sep = " ")
  data$sample <- samples$sample
  data$k <- k
  
  # Convert to longer
  data %>% gather(Q, value, -sample, -k) -> data
  copper_admix <- rbind(copper_admix, data)
}

# Load in the location info for sorting the plot
copper_admix <- inner_join(copper_admix, sample_info)



# plot the admixture bars
#pdf(file = "plots/admixture_copper.pdf", width = 4, height = 2)
#png(file = "copper_admixture.png", width = 3500, height = 2000, res = 300)
plot_3 <- copper_admix %>%
  group_by(reorder(population, latitude)) %>%
  filter(k != 1, k < 4) %>%
  arrange(latitude) %>%
  ggplot(., aes(x = reorder(sample, latitude), y = value, fill = factor(Q))) +
  geom_bar(stat = "identity", position = "stack") +
  #scale_fill_manual(values = c("grey80", "#daa038")) +
  xlab("Samples sorted by latitude") +
  ylab("Ancestry") +
  facet_grid(k~population, scales = "free", space = "free") +
  theme_cowplot() +
  theme(
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.text = element_text(size = 12),
    strip.background.y = element_blank(),
    #strip.text.y = element_blank(),
    legend.position = "none") +
  scale_fill_viridis_d() 


# load in cross-validation error
copper_cv <- read_lines("/project/ctb-grego/ntbsykes/copper_quillback/structure/copper_inside/copper.cv.error") %>%
  tibble(text = .) %>%
  extract(text, into = c("K", "CV_error"), 
          regex = "CV error \\(K=(\\d+)\\): ([0-9.]+)") %>%
  mutate(
    K = as.numeric(K),
    CV_error = as.numeric(CV_error)
  )
plot_4 <- copper_cv %>%
  ggplot(aes(K, CV_error)) +
  geom_line(colour = "grey") +
  geom_point(colour = "cornflowerblue", size = 4) +
  theme_cowplot() +
  labs(x = "K", y = "CV error") +
   scale_x_continuous(breaks = 1:10)

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/admixture_copper_inside.pdf",
    height=8,width=9)
(plot_1 | plot_2) / (plot_3 | plot_4) + plot_annotation(tag_levels = 'A')
dev.off()
