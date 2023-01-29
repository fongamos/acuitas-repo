# import raw data
# file should have Lipid, Dose, Sample, Liver.Concentration, Spleen.Concentration
df <- read.csv("~/acuitas/amos files/AIC-NC-0645.csv")

# rename sample
for (i in 1:nrow(df)){
  df[i,"Sample"] = str_extract(df[i,"Sample"], "[:digit:]")
  df[i,"Sample"] = str_c(df[i,"Lipid"], " ", df[i,"Sample"])
}

# create named vector for colors
MyColors <- c(brewer.pal(9,"Greys")[3:7],
              rev(colorRampPalette(brewer.pal(9,"YlOrRd"))(10)),
              brewer.pal(9, "Greens")[5:9],
              brewer.pal(9, "GnBu")[5:9])
Sample <- unique(df$Samples)
names(MyColors) <- Sample

# create comparisons list
Lipid <- unique(df$Lipid)
MyComparisons1 <- list(c(Lipid[1], Lipid[2]),
                       c(Lipid[1], Lipid[3]),
                       c(Lipid[1], Lipid[4]),
                       c(Lipid[1], Lipid[5]))

# create facet titles
Dose_label <- c("High Dose 1.0[mg/kg]", "Low Dose 0.3[mg/kg]")
names(Dose_label) <- c("0.3", "1")

# function for NLS boxplot
NLS_boxplot <- function(i, j){
  ggplot(i, aes(y = j, x = Lipid)) + 
    geom_boxplot(outlier.size = 0) +
    geom_point(aes(color = Sample), position = position_jitter(width=0.25), size = 2.5) +
    scale_color_manual(values = MyColors) +
    theme_bw() + 
    labs(x = NULL, y = "Luc [ng/g tissue]", color = "Sample") +
    facet_wrap("Dose", scales = "fixed", ncol = 2, labeller = labeller(Dose = Dose_label)) +
    theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.ticks.x = element_blank(), axis.text = element_text(color = "black"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15), axis.title.y = element_text(vjust = 0.5, size = 15), legend.title = element_blank(), legend.text = element_text(size = 10)) +
    ggpubr::stat_compare_means(comparisons = MyComparisons1, method = "t.test", label = after_stat("p.format")) +
    guides(color=guide_legend(ncol=5))
}

# export
ggsave(NLS_boxplot(df, df$Liver.Concentration), filename = "~/acuitas/export/AIC-NC-0645_liver.png", height = 8, width = 16)
ggsave(NLS_boxplot(df, df$Spleen.Concentration), filename = "~/acuitas/export/AIC-NC-0645_spleen.png", height = 8, width = 16)
  