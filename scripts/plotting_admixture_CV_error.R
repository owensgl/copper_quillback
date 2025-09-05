# load in cross-validation error
copper_cv <- read_lines("/project/ctb-grego/ntbsykes/copper_quillback/structure/copper.CV.error") %>%
  tibble(text = .) %>%
  extract(text, into = c("K", "CV_error"), 
          regex = "CV error \\(K=(\\d+)\\): ([0-9.]+)") %>%
  mutate(
    K = as.numeric(K),
    CV_error = as.numeric(CV_error),
    species = "Copper"
  )

quillback_cv <- read_lines("/project/ctb-grego/ntbsykes/copper_quillback/structure/quillback.CV.error") %>%
  tibble(text = .) %>%
  extract(text, into = c("K", "CV_error"), 
          regex = "CV error \\(K=(\\d+)\\): ([0-9.]+)") %>%
  mutate(
    K = as.numeric(K),
    CV_error = as.numeric(CV_error),
    species = "Quillback"
  )

pdf("/project/ctb-grego/ntbsykes/copper_quillback/plots/CV_errors.pdf",
    height=3,width = 6)

rbind(copper_cv, quillback_cv) %>%
  ggplot(aes(K, CV_error)) +
  geom_line(colour = "grey") +
  geom_point(colour = "black", size = 4) +
  theme_cowplot() +
  labs(x = "K", y = "CV error") +
  scale_x_continuous(breaks = 1:10) +
  facet_wrap(~species)
dev.off()
