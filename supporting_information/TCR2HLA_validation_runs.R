require(ggplot2)
require(dplyr)
d = readr::read_csv("/Volumes//fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_validation_runs.csv")
d = d %>% mutate(gated = ifelse(stringr::str_detect(pattern = "gated", 
                                                    string = name), 
                                "P<0.1 | P>0.9","P<0.5 | P>0.5")) 
table(d$gated)
unique(d$model)

name_map <- c(
  'VALA_v4e_S'        = 'VAL: Rawat et al. 2025  > 25K TCRb',
  'VALA_v4e_S_gated'  = 'VAL: Rawat et al. 2025  > 25K TCRb (gated)',
  'VALB_v4e_S'        = 'VALB: Rawat et al. 2025  > 25K TCRb, > 65y',
  'VALB_v4e_S_gated'  = 'VALB: Rawat et al. 2025  > 25K TCRb, > 65y (gated)',
  'MIRA_v4e_S'        = 'MIRA: Nolan et al. 2025',
  'MIRA_v4e_S_gated'  = 'MIRA: Nolan et al. 2025 (gated)',
  'ROSA_v4e_S'        = 'ROSA: Rosati et al. 2022 > 25K TCRb',
  'ROSA_v4e_S_gated'  = 'ROSA: Rosati et al. 2022 > 25K TCRb (gated)',
  'ROSB_v4e_S'        = 'ROSB: Rosati et al. 2022 < 25K TCRb',
  'ROSB_v4e_S_gated'  = 'ROSB: Rosati et al. 2022 < 25K TCRb (gated)',
  'TOWA_v4e_S'        = 'TOWA: Towlerton et al. 2022 > 25K TCRb',
  'TOWA_v4e_S_gated'  = 'TOWA: Towlerton et al. 2022 > 25K TCRb (gated)',
  'TOWB_v4e_S'        = 'TOWB: Towlerton et al. 2022 < 25K TCRb',
  'TOWB_v4e_S_gated'  = 'TOWB: Towlerton et al. 2022 < 25K TCRb (gated)',
  'VALA_v4e_HS2'        = 'VAL: Rawat et al. 2025  > 25K TCRb',
  'VALA_v4e_HS2_gated'  = 'VAL: Rawat et al. 2025  > 25K TCRb (gated)',
  'VALB_v4e_HS2'        = 'VALB: Rawat et al. 2025  > 25K TCRb, > 65y',
  'VALB_v4e_HS2_gated'  = 'VALB: Rawat et al. 2025  > 25K TCRb, > 65y (gated)',
  'MIRA_v4e_HS2'        = 'MIRA: Nolan et al. 2025',
  'MIRA_v4e_HS2_gated'  = 'MIRA: Nolan et al. 2025 (gated)',
  'ROSA_v4e_HS2'        = 'ROSA: Rosati et al. 2022 > 25K TCRb',
  'ROSA_v4e_HS2_gated'  = 'ROSA: Rosati et al. 2022 > 25K TCRb (gated)',
  'ROSB_v4e_HS2'        = 'ROSB: Rosati et al. 2022 < 25K TCRb',
  'ROSB_v4e_HS2_gated'  = 'ROSB: Rosati et al. 2022 < 25K TCRb (gated)',
  'TOWA_v4e_HS2'        = 'TOWA: Towlerton et al. 2022 > 25K TCRb',
  'TOWA_v4e_HS2_gated'  = 'TOWA: Towlerton et al. 2022 > 25K TCRb (gated)',
  'TOWB_v4e_HS2'        = 'TOWB: Towlerton et al. 2022 < 25K TCRb',
  'TOWB_v4e_HS2_gated'  = 'TOWB: Towlerton et al. 2022 < 25K TCRb (gated)',
  'TOW_v4e_HS2'         = 'TOW: Towlerton et al. 2022 > 5K TCRb',
  'TOW_v4e_HS2_gated'   = 'TOW: Towlerton et al. 2022 > 5K TCRb (gated)',
  'TOW_v4e_S'           = 'TOW: Towlerton et al. 2022 > 5K TCRb',
  'TOW_v4e_S_gated'     = 'TOW: Towlerton et al. 2022 > 5K TCRb (gated)')


# category_colors_named <- c(
#   'VALA: Rawat et al. 2025  > 25K TCRb'         = "#33a02c",
#   'VALA: Rawat et al. 2025  > 25K TCRb (gated)' = "#b2df8a",
#   'VALB: Rawat et al. 2025  > 25K TCRb, > 65y'         = "#1b9e77",
#   'VALB: Rawat et al. 2025  > 25K TCRb, > 65y (gated)' = "#a6d854",
#   'MIRA: Nolan et al. 2025'               = "#1f78b4",
#   'MIRA: Nolan et al. 2025 (gated)'       = "#a6cee3",
#   'ROSA: Rosati et al. 2022 > 25K TCRb'        = "#e31a1c",
#   'ROSA: Rosati et al. 2022 > 25K TCRb (gated)'= "#fb9a99",
#   'ROSB: Rosati et al. 2022 < 25K TCRb'        = "orange",
#   'ROSB: Rosati et al. 2022 < 25K TCRb (gated)'= "#fdbf6f",
#   'TOW: Towlerton et al. 2022 > 5K TCRb'     = "black",
#   'TOW: Towlerton et al. 2022 > 5K TCRb (gated)' ="darkgray",
#   'TOWA: Towlerton et al. 2022 > 25K TCRb'     = "#6a3d9a",
#   'TOWA: Towlerton et al. 2022 > 25K TCRb (gated)' = "#cab2d6",
#   'TOWB: Towlerton et al. 2022 < 25K TCRb'     = "#5e3c58",
#   'TOWB: Towlerton et al. 2022 < 25K TCRb (gated)' = "#bfa6b8"
# )
category_colors_named <- c(
  'VAL: Rawat et al. 2025  > 25K TCRb'         = "navy",
  'VALB: Rawat et al. 2025  > 25K TCRb, > 65y'  = "#1f78b4",
  'TOW: Towlerton et al. 2022 > 5K TCRb'        ="orange",
  'TOWA: Towlerton et al. 2022 > 25K TCRb'     = "#6a3d9a",
  'TOWB: Towlerton et al. 2022 < 25K TCRb'     = "#5e3c58",
  'MIRA: Nolan et al. 2025'                    = "#33a02c",
  'ROSA: Rosati et al. 2022 > 25K TCRb'        = "#e31a1c",
  'ROSB: Rosati et al. 2022 < 25K TCRb'        = "salmon",
  
  
  'VAL: Rawat et al. 2025  > 25K TCRb (gated)'         = "steelblue",
  'VALB: Rawat et al. 2025  > 25K TCRb, > 65y (gated)'  = "#a6cee3",
  'TOW: Towlerton et al. 2022 > 5K TCRb (gated)'        = "#fdbf6f",
  'TOWA: Towlerton et al. 2022 > 25K TCRb (gated)'      = "#cab2d6",
  'TOWB: Towlerton et al. 2022 < 25K TCRb (gated)'      = "#bfa6b8",
  'MIRA: Nolan et al. 2025 (gated)'                     = "#b2df8a",
  'ROSA: Rosati et al. 2022 > 25K TCRb (gated)'         = "#fb9a99",
  'ROSB: Rosati et al. 2022 < 25K TCRb (gated)'         = "pink"

 )


# category_colors_named <- c(
#   'VALA: Rawat et al. 2025  > 25K TCRb'         = "#33a02c",
# 
#   'VALB: Rawat et al. 2025  > 25K TCRb, > 65y'         = "#1b9e77",
#   'MIRA: Nolan et al. 2025'               = "#1f78b4",
#   'ROSA: Rosati et al. 2022 > 25K TCRb'        = "#e31a1c",
#   'ROSB: Rosati et al. 2022 < 25K TCRb'        = "orange",
#   'TOWA: Towlerton et al. 2022 > 25K TCRb'     = "#6a3d9a",
#   'TOWB: Towlerton et al. 2022 < 25K TCRb'     = "#5e3c58",
#   'TOW: Towlerton et al. 2022 > 5K TCRb'     = "black",
#   
#   'VALA: Rawat et al. 2025  > 25K TCRb (gated)' = "#b2df8a",
#   'VALB: Rawat et al. 2025  > 25K TCRb, > 65y (gated)' = "#a6d854",
#   'MIRA: Nolan et al. 2025 (gated)'       = "#a6cee3",
#   'ROSA: Rosati et al. 2022 > 25K TCRb (gated)'= "#fb9a99",
#   'ROSB: Rosati et al. 2022 < 25K TCRb (gated)'= "#fdbf6f",
#   'TOWA: Towlerton et al. 2022 > 25K TCRb (gated)' = "#cab2d6",
#   'TOWB: Towlerton et al. 2022 < 25K TCRb (gated)' = "#bfa6b8",
#   'TOW: Towlerton et al. 2022 > 5K TCRb (gated)' ="darkgray")

d = readr::read_csv("/Volumes//fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_validation_runs.csv")
d = d %>% mutate(gated = ifelse(stringr::str_detect(pattern = "gated", 
                                                    string = name), 
                                "Thresholds: P<0.1 | P>0.9","Thresholds: P<0.5 | P>0.5"))
d <- d %>%
  mutate(name_label = name_map[name]) %>%
  mutate(model = ifelse(model == "v4e","Model: common allele (v4e)","")) %>% 
  mutate(calibration = c("S"="Calibration: standard","HS2"="Calibration: high sensitivity")[calibration]) 


library(ggplot2)
library(dplyr)
library(forcats)

metrics <- list(
  ba_acc = "Balanced Accuracy",
  auc    = "AUROC",
  sens   = "Sensitivity",
  spec   = "Specificity"
)

output_names <- list(
  ba_acc = "TCR2HLA_Balanced_Accuracy.pdf",
  auc    = "TCR2HLA_AUROC.pdf",
  sens   = "TCR2HLA_Sensitivity.pdf",
  spec   = "TCR2HLA_Specificity.pdf"
)

output_dir <- "/Volumes/fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/"

for (metric in names(metrics)) {
  print(metric)
  plot <- ggplot(
    data = d %>%
      filter(!locus %in% c("DR3", "DR4", "DR5", "DPA", "DPB", "DQA", "DQB")),
    aes(
      fill = forcats::fct_rev(forcats::fct_relevel(name_label, names(category_colors_named))),
      y = forcats::fct_rev(forcats::fct_relevel(locus, loci)),
      x = .data[[metric]]
    )
  ) +
    geom_boxplot(outlier.size = 0.1) +
    geom_point(pch = 21, col = "black", size = 0.5, position = position_dodge(width = 0.8)) +
    theme_bw() +
    scale_fill_manual(values = category_colors_named, name = NULL) +
    facet_wrap(model ~ forcats::fct_rev(gated) ~ forcats::fct_rev(calibration), ncol = 4) +
    ylab("") +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(reverse = TRUE, ncol = 2)) +
    geom_vline(aes(xintercept = 0.9), linetype = "dashed") +
    xlab(metrics[[metric]])
  
  ggsave(
    filename = file.path(output_dir, output_names[[metric]]),
    plot = plot,
    width = 8,
    height = 11
  )
}

# 
# 
# plot1 = ggplot(data = d %>%
#          filter(!locus %in% c("DR3", "DR4", "DR5", "DPA", "DPB", "DQA", "DQB")),
#        aes(
#          fill = forcats::fct_rev(forcats::fct_relevel(name_label, names(category_colors_named))),
#          y = forcats::fct_rev(forcats::fct_relevel(locus, loci)),
#          x = ba_acc
#        )) +
#   geom_boxplot(outlier.size = 0.1) +
#   geom_point(pch = 21, col = "black", size = .5,position = position_dodge(width = .8))+
#   theme_bw() +
#   scale_fill_manual(values = category_colors_named, name = NULL) +
#   facet_wrap(model~forcats::fct_rev(gated)~forcats::fct_rev(calibration), ncol = 4) +
#   ylab("") +
#   theme(legend.position = "bottom")+
#   guides(fill = guide_legend(reverse = TRUE, ncol = 2)) + 
#   geom_vline(aes(xintercept = .9), linetype = "dashed") + 
#   xlab("Balanced Accuracy")
# plot1
# 
# ggsave("/Volumes//fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_Balanced_Accuracy.pdf", 
#        plot= plot1,
#        width = 8,
#        height = 11)
# 
# 
# plot2 = ggplot(data = d %>%
#                  filter(!locus %in% c("DR3", "DR4", "DR5", "DPA", "DPB", "DQA", "DQB")),
#                aes(
#                  fill = forcats::fct_rev(forcats::fct_relevel(name_label, names(category_colors_named))),
#                  y = forcats::fct_rev(forcats::fct_relevel(locus, loci)),
#                  x = auc
#                )) +
#   geom_boxplot(outlier.size = 0.1) +
#   geom_point(pch = 21, col = "black", size = .5,position = position_dodge(width = .8))+
#   theme_bw() +
#   scale_fill_manual(values = category_colors_named, name = NULL) +
#   facet_wrap(model~forcats::fct_rev(gated)~forcats::fct_rev(calibration), ncol = 4) +
#   ylab("") +
#   theme(legend.position = "bottom")+
#   guides(fill = guide_legend(reverse = TRUE, ncol = 2)) + 
#   geom_vline(aes(xintercept = .9), linetype = "dashed") + 
#   xlab("AUROC")
# 
# ggsave("/Volumes//fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_AUROC.pdf", 
#        plot= plot2,
#        width = 8,
#        height = 11)
# 
# 
# plot3 = ggplot(data = d %>%
#                  filter(!locus %in% c("DR3", "DR4", "DR5", "DPA", "DPB", "DQA", "DQB")),
#                aes(
#                  fill = forcats::fct_rev(forcats::fct_relevel(name_label, names(category_colors_named))),
#                  y = forcats::fct_rev(forcats::fct_relevel(locus, loci)),
#                  x = sens
#                )) +
#   geom_boxplot(outlier.size = 0.1) +
#   geom_point(pch = 21, col = "black", size = .5,position = position_dodge(width = .8))+
#   theme_bw() +
#   scale_fill_manual(values = category_colors_named, name = NULL) +
#   facet_wrap(model~forcats::fct_rev(gated)~forcats::fct_rev(calibration), ncol = 4) +
#   ylab("") +
#   theme(legend.position = "bottom")+
#   guides(fill = guide_legend(reverse = TRUE, ncol = 2)) + 
#   geom_vline(aes(xintercept = .9), linetype = "dashed") + 
#   xlab("Sensitivity")
# 
# ggsave("/Volumes//fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_Sensitivity.pdf", 
#        plot= plot3,
#        width = 8,
#        height = 11)
# 
# 
# plot4 = ggplot(data = d %>%
#                  filter(!locus %in% c("DR3", "DR4", "DR5", "DPA", "DPB", "DQA", "DQB")),
#                aes(
#                  fill = forcats::fct_rev(forcats::fct_relevel(name_label, names(category_colors_named))),
#                  y = forcats::fct_rev(forcats::fct_relevel(locus, loci)),
#                  x = spec
#                )) +
#   geom_boxplot(outlier.size = 0.1) +
#   geom_point(pch = 21, col = "black", size = .5,position = position_dodge(width = .8))+
#   theme_bw() +
#   scale_fill_manual(values = category_colors_named, name = NULL) +
#   facet_wrap(model~forcats::fct_rev(gated)~forcats::fct_rev(calibration), ncol = 4) +
#   ylab("") +
#   theme(legend.position = "bottom")+
#   guides(fill = guide_legend(reverse = TRUE, ncol = 2)) + 
#   geom_vline(aes(xintercept = .9), linetype = "dashed") + 
#   xlab("Specificity")
# 
# ggsave("/Volumes//fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_Specificity.pdf", 
#        plot= plot4,
#        width = 8,
#        height = 11)
# 
# 
# plot2 = ggplot(data = d %>%
#                  filter(!locus %in% c("DR3", "DR4", "DR5", "DPA", "DPB", "DQA", "DQB")),
#                aes(
#                  fill = forcats::fct_rev(forcats::fct_relevel(name_label, names(category_colors_named))),
#                  y = forcats::fct_rev(forcats::fct_relevel(locus, loci)),
#                  x = ba_acc
#                )) +
#   geom_boxplot(outlier.size = 0.1) +
#   geom_point(pch = 21, col = "black", size = .5,position = position_dodge(width = .8))+
#   theme_bw() +
#   scale_fill_manual(values = category_colors_named, name = NULL) +
#   facet_wrap(model~forcats::fct_rev(calibration)~forcats::fct_rev(gated)) +
#   ylab("") +
#   theme(legend.position = "bottom")+
#   guides(fill = guide_legend(reverse = TRUE, ncol = 2)) + 
#   geom_vline(aes(xintercept = .9), linetype = "dashed") + 
#   xlab("AUROC")
# 
# ggsave("/Volumes//fh/fast/gilbert_p/kmayerbl/TCR2HLA/supporting_information/TCR2HLA_AUROC.pdf", 
#        plot= plot2,
#        width = 8,
#        height = 11)
# 
# category_colors <- c(
#   'VALA_v4e'         = "#33a02c",
#   'VALA_v4e_gated'   = "#b2df8a",
#   'VALB_v4e'         = "#1b9e77",
#   'VALB_v4e_gated'   = "#a6d854",
#   'MIRA_v4e'         = "#1f78b4",
#   'MIRA_v4e_gated'   = "#a6cee3",
#   'ROSA_v4e'         = "#e31a1c",
#   'ROSA_v4e_gated'   = "#fb9a99",
#   'ROSB_v4e'         = "orange",
#   'ROSB_v4e_gated'   = "#fdbf6f",
#   'TOWA_v4e'         = "#6a3d9a",
#   'TOWA_v4e_gated'   = "#cab2d6",
#   'TOWB_v4e'         = "#5e3c58",
#   'TOWB_v4e_gated'   = "#bfa6b8")
# 
# # name_map <- c(
# #   'VALA_v4e'        = 'VALA: Rawat et al. 2025  > 25K TCRb',
# #   'VALA_v4e_gated'  = 'VALA: Rawat et al. 2025  > 25K TCRb (gated)',
# #   'VALB_v4e'        = 'VALB: Rawat et al. 2025  > 25K TCRb, > 65y',
# #   'VALB_v4e_gated'  = 'VALB: Rawat et al. 2025  > 25K TCRb, > 65y (gated)',
# #   'MIRA_v4e'        = 'MIRA: Nolan et al. 2025',
# #   'MIRA_v4e_gated'  = 'MIRA: Nolan et al. 2025 (gated)',
# #   'ROSA_v4e'        = 'ROSA: Rosati et al. 2022 > 25K TCRb',
# #   'ROSA_v4e_gated'  = 'ROSA: Rosati et al. 2022 > 25K TCRb (gated)',
# #   'ROSB_v4e'        = 'ROSB: Rosati et al. 2022 < 25K TCRb',
# #   'ROSB_v4e_gated'  = 'ROSB: Rosati et al. 2022 < 25K TCRb (gated)',
# #   'TOWA_v4e'        = 'TOWA: Towlerton et al. 2022 > 25K TCRb',
# #   'TOWA_v4e_gated'  = 'TOWA: Towlerton et al. 2022 > 25K TCRb (gated)',
# #   'TOWB_v4e'        = 'TOWB: Towlerton et al. 2022 < 25K TCRb',
# #   'TOWB_v4e_gated'  = 'TOWB: Towlerton et al. 2022 < 25K TCRb (gated)'
# # )
