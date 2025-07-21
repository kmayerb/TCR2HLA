require(ggplot2)
require(dplyr)
d = readr::read_csv('/Volumes/fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_dataset_allele_prevalence.csv')
lvs = d %>% select(binary_imgt, TRAIN) %>% arrange(desc(TRAIN)) %>% 
  #mutate(binary_imgt = sub(".*\\*", "", binary_imgt)) %>%
  pull(binary_imgt)
lvs
th = theme(
  panel.grid = element_blank(),                  # remove gridlines
  axis.title = element_blank(),                  # remove axis titles
  axis.text.y = element_text(size = 6),
  axis.text.x = element_text(size = 6 , angle = 90),# smaller text
  axis.ticks = element_line(size = 0.3),         # thin ticks
  strip.background = element_blank(),            # no strip background
  strip.text = element_text(size = 8),  # facet labels
  panel.border = element_rect(color = "black", fill = NA, size =0),
  plot.margin = margin(5, 5, 5, 5)                # tight margins
)



p= d %>% filter(locus %in% c("A","B","C","DR","DQA","DQB")) %>% 
  select(locus, binary, binary_imgt, TRAIN, VALA, MIRA, ROS, TOW, MUSVOSVI) %>%
  tidyr::gather(dataset, prev, -binary_imgt, -locus, -binary) %>% 
  #mutate(binary_imgt = sub(".*\\*", "", binary_imgt)) %>%
  mutate(binary_imgt_fct = factor(binary_imgt, levels = lvs)) %>%
  mutate(dataset = factor(dataset, 
                          levels = c("TRAIN", "VALA","MUSVOSVI", "TOW", "MIRA", "ROS")))%>%
  ggplot(aes(y = forcats::fct_rev(binary_imgt_fct), x = prev)) + 
  geom_point(aes(col =dataset, shape = dataset)) +
  facet_wrap(~locus, nrow = 1, scale = "free_y")+ 
  theme_classic() + 
  scale_color_manual("", values = c("TRAIN"="black", 
                                    "MIRA"= "#33a02c", 
                                    "VALA" = "navy",
                                    "ROS" = "red", 
                                    "TOW" = "orange", 
                                    "MUSVOSVI"= "purple"),
                     labels = c(
                       "TRAIN"     = "Training Data",
                       "MIRA"      = "Nolan et al.",
                       "VALA"       = "Rawat et al. (Primary Validation)",
                       "ROS"       = "Rosati et al.",
                       "TOW"       = "Towlerton et al.",
                       "MUSVOSVI"  = "Musvosvi et al. (RSA Training Data)"
                     )) + 
  scale_shape_manual(
    name = "",  # or provide a name like "Dataset"
    values = c(
      "TRAIN"     = 20,
      "MIRA"      = 1,
      "VALA"       = 2,
      "ROS"       = 4,
      "TOW"       = 3,
      "MUSVOSVI"  = 5
    ),
    labels = c(
      "TRAIN"     = "Training Data",
      "MIRA"      = "Nolan et al.",
      "VALA"       = "Rawat et al. (Primary Validation)",
      "ROS"       = "Rosati et al.",
      "TOW"       = "Towlerton et al.",
      "MUSVOSVI"  = "Musvosvi et al. (RSA Training Data)"
    )
  ) + 
  th + 
  scale_x_continuous(limits = c(0, .55), expand = c(0,0))+
  theme(legend.position = "top")
p
ggsave("/Volumes/fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_dataset_allele_prevalence.pdf", 
       plot = p, width = 7.5, height = 3, units = "in")
ggsave("/Volumes/fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_dataset_allele_prevalence.png",
       plot = p, width = 7.5, height = 3, units = "in", dpi = 300)
  