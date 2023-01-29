# create named vector for colors
MyColors <- brewer.pal(n=10,"Set3")[1:10]
names(MyColors) <- c("IFN-α2a","IL-18","IL-1RA","IL-1β","IL-6","IP-10","MCP-1","MIP-1α","MIP-1β","TNF-α")

# create comparisons list 
MyComparisons1 <- list(c("LPS", "PBS"), c("unmod LNP", "PBS"), c("mod LNP", "PBS"), c("empty LNP", "PBS"))
MyComparisons2 <- list(c("mod LNP", "PBS"), c("mod LNP + DMSO", "PBS"), c("LPS", "PBS"), c("mod LNP", "mod LNP + DMSO"))
MyComparisons3 <- list(c("100FM Dex", "PBS"),c("10pM Dex", "PBS"),c("1nM Dex", "PBS"),c("100FM MCC","PBS"),c("10pM MCC","PBS"),c("1nM MCC","PBS"),c("LPS","PBS"))
MyComparisons4 <- list(c("100FM Dex", "PBS"),c("100pM Dex", "PBS"),c("10pM Dex", "PBS"),c("1pM Dex", "PBS"),c("1nM Dex", "PBS"),c("100FM Fos", "PBS"),c("100pM Fos", "PBS"),c("10pM Fos", "PBS"),c("1pM Fos", "PBS"),c("1nM Fos", "PBS"),c("mod LNP", "PBS"), c("unmod LNP", "PBS"), c("LPS", "PBS"))

# import raw data
# file should have Sample, Assay, Detection.Limits..Calc..High, Detection.Limits..Calc..Low, Detection.Range, Calc..Concentration, Calc..Conc..Mean, Calc..Conc..CV
df <- read.csv("~/acuitas/amos files/20220908_WBA.csv")
df <- read.csv("~/acuitas/amos files/20220914_WBA.csv")
df <- read.csv("~/acuitas/amos files/20220927_WBA.csv")
df <- read.csv("~/acuitas/amos files/20221104_WBA.csv")
df <- read.csv("~/acuitas/amos files/20221118_WBA.csv")
df <- read.csv("~/acuitas/amos files/20221208_WBA.csv")

# adjust concentrations for readings above / below standard curve
# above -> Detection.Limits..Calc..High
# below -> Detection.Limits..Calc..Low
concentration <- c()
for (i in 1:nrow(df)){
  if (str_detect(df[i,grep("Range", colnames(df))], "Below")){
    concentration = c(concentration, df[i,grep("Low", colnames(df))])}
  else if (str_detect(df[i,grep("Range", colnames(df))], "Above")){
    concentration = c(concentration, df[i,grep("High", colnames(df))])}
  else{concentration = c(concentration, df[i,grep("Concentration", colnames(df))])}
}

# rename donor
donor <- c()
for (i in 1:nrow(df)){
  donor = c(donor, str_sub(df[i,"Sample"], 1, 1))
}

# rename blood draw alphabetical donor ID to numeric donor id

# # 20220908
# donor_id <- c()
# for (i in seq_along(donor)){
#   if (str_detect(donor[i], "7")){
#     donor_id = c(donor_id, "0007")}
#   else if (str_detect(donor[i], "8")){
#     donor_id = c(donor_id, "0008")}
#   else{donor_id = c(donor_id, donor[i])}
# }

# # 20220914
# donor_id <- c()
# for (i in seq_along(donor)){
#   if (str_detect(donor[i], "A")){
#     donor_id = c(donor_id, "0017")}
#   else if (str_detect(donor[i], "B")){
#     donor_id = c(donor_id, "0018")}
#   else if (str_detect(donor[i], "C")){
#     donor_id = c(donor_id, "0019")}
#   else if (str_detect(donor[i], "D")){
#     donor_id = c(donor_id, "0020")}
#   else if (str_detect(donor[i], "E")){
#     donor_id = c(donor_id, "0021")}
#   else if (str_detect(donor[i], "F")){
#     donor_id = c(donor_id, "0013")}
#   else if (str_detect(donor[i], "G")){
#     donor_id = c(donor_id, "0022")}
#   else if (str_detect(donor[i], "H")){
#     donor_id = c(donor_id, "0023")}
#   else if (str_detect(donor[i], "I")){
#     donor_id = c(donor_id, "0016")}
#   else if (str_detect(donor[i], "J")){
#     donor_id = c(donor_id, "0024")}
#   else{donor_id = c(donor_id, donor[i])}
# }

# # 20220927
# donor_id <- c()
# for (i in seq_along(donor)){
#   if (str_detect(donor[i], "A")){
#     donor_id = c(donor_id, "0025")}
#   else if (str_detect(donor[i], "B")){
#     donor_id = c(donor_id, "0026")}
#   else if (str_detect(donor[i], "C")){
#     donor_id = c(donor_id, "0016")}
#   else if (str_detect(donor[i], "D")){
#     donor_id = c(donor_id, "0021")}
#   else if (str_detect(donor[i], "E")){
#     donor_id = c(donor_id, "0027")}
#   else if (str_detect(donor[i], "F")){
#     donor_id = c(donor_id, "0028")}
#   else if (str_detect(donor[i], "G")){
#     donor_id = c(donor_id, "0008")}
#   else if (str_detect(donor[i], "H")){
#     donor_id = c(donor_id, "0029")}
#   else{donor_id = c(donor_id, donor[i])}
# }

# # 20221104
# donor_id <- c()
# for (i in seq_along(donor)){
#   if (str_detect(donor[i], "A")){
#     donor_id = c(donor_id, "0029")}
#   else if (str_detect(donor[i], "B")){
#     donor_id = c(donor_id, "0030")}
#   else if (str_detect(donor[i], "C")){
#     donor_id = c(donor_id, "0031")}
#   else if (str_detect(donor[i], "D")){
#     donor_id = c(donor_id, "0032")}
#   else if (str_detect(donor[i], "E")){
#     donor_id = c(donor_id, "0033")}
#   else if (str_detect(donor[i], "F")){
#     donor_id = c(donor_id, "0002")}
#   else if (str_detect(donor[i], "G")){
#     donor_id = c(donor_id, "0014")}
#   else if (str_detect(donor[i], "H")){
#     donor_id = c(donor_id, "0017")}
#   else{donor_id = c(donor_id, donor[i])}
# }

# # 20221118
# donor_id <- c()
# for (i in seq_along(donor)){
#   if (str_detect(donor[i], "A")){
#     donor_id = c(donor_id, "0014")}
#   else if (str_detect(donor[i], "B")){
#     donor_id = c(donor_id, "0007")}
#   else if (str_detect(donor[i], "C")){
#     donor_id = c(donor_id, "0015")}
#   else if (str_detect(donor[i], "D")){
#     donor_id = c(donor_id, "0021")}
#   else if (str_detect(donor[i], "E")){
#     donor_id = c(donor_id, "0024")}
#   else{donor_id = c(donor_id, donor[i])}
# }


# # 20221208
# donor_id <- c()
# for (i in seq_along(donor)){
#   if (str_detect(donor[i], "A")){
#     donor_id = c(donor_id, "0025")}
#   else if (str_detect(donor[i], "B")){
#     donor_id = c(donor_id, "0026")}
#   else if (str_detect(donor[i], "C")){
#     donor_id = c(donor_id, "0027")}
#   else if (str_detect(donor[i], "D")){
#     donor_id = c(donor_id, "0029")}
#   else if (str_detect(donor[i], "E")){
#     donor_id = c(donor_id, "0002")}
#   else if (str_detect(donor[i], "F")){
#     donor_id = c(donor_id, "0030")}
#   else if (str_detect(donor[i], "G")){
#     donor_id = c(donor_id, "0032")}
#   else if (str_detect(donor[i], "H")){
#     donor_id = c(donor_id, "0033")}
#   else{donor_id = c(donor_id, donor[i])}
# }

# rename treatment
treatment <- c()
for (i in 1:nrow(df)){
  if(str_detect(df[i,"Sample"], "EMT")){
    treatment = c(treatment, "empty LNP")}
  else if(str_detect(df[i,"Sample"], "LPS")){
    treatment = c(treatment, "LPS")}
  else if(str_detect(df[i,"Sample"], "MOD")){
    treatment = c(treatment, "mod LNP")}
  else if(str_detect(df[i,"Sample"], "PBS")){
    treatment = c(treatment, "PBS")}
  else if(str_detect(df[i,"Sample"], "UN")){
    treatment = c(treatment, "unmod LNP")}
  else if(str_detect(df[i,"Sample"], "BLANK")){
    treatment = c(treatment, "blank")}
  else if(str_detect(df[i,"Sample"], "STD")){
    treatment = c(treatment, df[i,"Sample"])}
  else if(str_detect(df[i,"Sample"], "LNP")){
    treatment = c(treatment, "mod LNP")}
  else if(str_detect(df[i,"Sample"], "V")){
    treatment = c(treatment, "mod LNP + DMSO")}
  else if(str_detect(df[i,"Sample"], "100FM Dex")){
    treatment = c(treatment, "100FM Dex")}
  else if(str_detect(df[i,"Sample"], "10pM Dex")){
    treatment = c(treatment, "10pM Dex")}
  else if(str_detect(df[i,"Sample"], "1nM Dex")){
    treatment = c(treatment, "1nM Dex")}
  else if(str_detect(df[i,"Sample"], "100FM MCC")){
    treatment = c(treatment, "100FM MCC")}
  else if(str_detect(df[i,"Sample"], "10pM MCC")){
    treatment = c(treatment, "10pM MCC")}
  else if(str_detect(df[i,"Sample"], "1nM MCC")){
    treatment = c(treatment, "1nM MCC")}
  else if(str_detect(df[i,"Sample"], "D 100f")){
    treatment = c(treatment, "100FM Dex")}
  else if(str_detect(df[i,"Sample"], "D 100p")){
    treatment = c(treatment, "100pM Dex")}
  else if(str_detect(df[i,"Sample"], "D 10p")){
    treatment = c(treatment, "10pM Dex")}
  else if(str_detect(df[i,"Sample"], "D 1p")){
    treatment = c(treatment, "1pM Dex")}
  else if(str_detect(df[i,"Sample"], "D 1n")){
    treatment = c(treatment, "1nM Dex")}
  else if(str_detect(df[i,"Sample"], "F 100f")){
    treatment = c(treatment, "100FM Fos")}
  else if(str_detect(df[i,"Sample"], "F 100p")){
    treatment = c(treatment, "100pM Fos")}
  else if(str_detect(df[i,"Sample"], "F 10p")){
    treatment = c(treatment, "10pM Fos")}
  else if(str_detect(df[i,"Sample"], "F 1p")){
    treatment = c(treatment, "1pM Fos")}
  else if(str_detect(df[i,"Sample"], "F 1n")){
    treatment = c(treatment, "1nM Fos")}
  else if(str_detect(df[i,"Sample"], "Lg")){
    treatment = c(treatment, "Large LNP")}
  else if(str_detect(df[i,"Sample"], "mod")){
    treatment = c(treatment, "mod LNP")}
  else if(str_detect(df[i,"Sample"], "un")){
    treatment = c(treatment, "unmod LNP")}
  else{treatment = c(treatment, df[i,"Sample"])}
}

# add new columns
df <- df %>% mutate(concentration = concentration) %>%
  mutate(donor = donor) %>%
  mutate(treatment = treatment) %>%
  mutate(donor_id = donor_id)

# filter standards, blanks, no data
p <- df %>% filter(donor != "S" & treatment != "no data") %>% filter(!str_detect(.$treatment, regex("blank", ignore_case = TRUE)))

# compile donors of interests
exp1 <- p %>% filter(!donor_id %in% c("0002","0025","0026","0027","0029","0030","0032","0033"))
exp2 <- p %>% filter(!donor_id %in% c("0002","0025","0026","0027","0029","0030","0032","0033"))
exp3 <- p 
compiled <- rbind(exp1, exp2, exp3)
p <- compiled %>% filter(donor_id %in% c("0016","0026","0028"))
p <- compiled %>% filter(donor_id %in% c("0014","0021","0025","0033"))
p <- compiled %>% filter(donor_id %in% c("0027", "0032"))

# factor treatment
# p[,"treatment"] <- factor(p[,"treatment"], levels=c("100FM Dex", "10pM Dex", "1nM Dex", "100FM MCC", "10pM MCC", "1nM MCC", "LPS", "PBS"))
# p[,"treatment"] <- factor(p[,"treatment"], levels=c("unmod LNP", "mod LNP", "empty LNP", "LPS", "PBS"))
# p[,"treatment"] <- factor(p[,"treatment"], levels=c("mod LNP", "mod LNP + DMSO", "LPS", "PBS"))
# p[,"treatment"] <- factor(p[,"treatment"], levels=c("100FM Dex", "100pM Dex","10pM Dex", "1pM Dex", "1nM Dex", "100FM Fos", "100pM Fos","10pM Fos", "1pM Fos", "1nM Fos", "mod LNP", "unmod LNP", "LPS", "PBS"))
p[,"treatment"] <- factor(p[,"treatment"], levels=c("LPS", "unmod LNP", "mod LNP", "empty LNP", "PBS"))

# filter for cytokines of interest
p_clean <- p %>% filter(Assay != "IP-10")
p_IFN_a2a <- p %>% filter(Assay == "IFN-α2a")
p_Il_18 <- p %>% filter(Assay == "IL-18")
p_Il_1RA <- p %>% filter(Assay == "IL-1RA")
p_Il_1b <- p %>% filter(Assay == "IL-1β")
p_Il_6 <- p %>% filter(Assay == "IL-6")
p_IP_10 <- p %>% filter(Assay == "IP-10")
p_MCP_1 <- p %>% filter(Assay == "MCP-1")
p_MIP_1a <- p %>% filter(Assay == "MIP-1α")
p_MIP_1b <- p %>% filter(Assay == "MIP-1β")
p_TNF_a <- p %>% filter(Assay == "TNF-α")








# functions for boxplot
custom_filter_df <- function(x, y){
  list=list()
  for (i in seq_along(unique(x[,y]))){
    list[[i]]=filter(x, x[,y] %in% unique(x[,y])[i])}
  return(list)
}

custom_t_test <- function(x, y, z){
  df=data.frame()
  for (i in seq_along(z)){
    tryCatch({
      res=rstatix::t_test(data=x%>%group_by(donor_id), y, z[i]) %>%
        rstatix::add_xy_position(x="treatment", comparisons=z)
      df=rbind(df,res)},
      error=function(x, y, z){df=df})}
  return(df)
}

custom_t_test_helper <- function(x){
  for (i in seq_along(x)){
    x[[i]] = custom_t_test(x[[i]], concentration~treatment, MyComparisons1)}
  return(x)
}

max_xy_position <- function(x){
  combinations = unique(x[,c("group1", "group2")])
  yposition = list()
  for (i in 1:nrow(x)){
    subset = x %>% filter(group1 %in% combinations[i,1] & group2 %in% combinations[i,2])
    subset[,"y.position"] = max(subset$y.position)
    yposition[[i]] = subset}
  yposition=bind_rows(yposition)
  return(yposition)
}

WBA_boxplot <- function(i) {
  
  t_test = custom_filter_df(i, "donor_id")
  t_test = custom_t_test_helper(t_test)
  t_test = bind_rows(t_test)
  t_test = max_xy_position(t_test)
  
  anova_test = i %>%
    group_by(donor_id) %>%
    anova_test(concentration~treatment, dv = concentration, wid = donor_id)
  
  multiple_comparisons_test = i %>%
    group_by(donor_id) %>%
    dunn_test(concentration~treatment, p.adjust.method = "holm", detailed = TRUE)
  
  res = left_join(t_test, multiple_comparisons_test,
                  by = intersect(colnames(t_test)[1:6], colnames(multiple_comparisons_test)[1:6]),
                  suffix = c(".t_test", ".multiple_comparisons_test"))
  
  ggplot(i, aes(y = concentration, x = treatment)) + 
    geom_boxplot(outlier.size = 0) +
    geom_point(aes(color = Assay), position = position_jitter(width=0.25), size = 2.5) +
    scale_color_manual(values = MyColors) +
    theme_bw() + 
    labs(x = NULL, y = "Cytokines [pg/mL]", color = "Assay") +
    facet_wrap("donor_id", scales = "fixed", ncol = 4) +
    theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.ticks.x = element_blank(), axis.text = element_text(color = "black"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15), axis.title.y = element_text(vjust = 0.5, size = 15), legend.title = element_blank(), legend.text = element_text(size = 10)) +
    ggpubr::stat_pvalue_manual(res, label = "p.adj.multiple_comparisons_test") +
    guides(color=guide_legend(ncol=1))
}

WBA_boxplot_no_t_test <- function(i) {

  ggplot(i, aes(y = concentration, x = treatment)) + 
    geom_boxplot(outlier.size = 0) +
    geom_point(aes(color = Assay), position = position_jitter(width=0.25), size = 2.5) +
    scale_color_manual(values = MyColors) +
    theme_bw() + 
    labs(x = NULL, y = "Cytokines [pg/mL]", color = "Assay") +
    facet_wrap("donor_id", scales = "fixed", ncol = 1) +
    theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.ticks.x = element_blank(), axis.text = element_text(color = "black"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15), axis.title.y = element_text(vjust = 0.5, size = 15), legend.title = element_blank(), legend.text = element_text(size = 10)) +
    guides(color=guide_legend(ncol=1))
}

# export boxplot
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/20220914_WBA_all_cytokines.png", height = 9, width = 15)
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/20220927_WBA_all_cytokines.png", height = 9, width = 15)
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/20221104_WBA_all_cytokines.png", height = 9, width = 15)
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/20221118_WBA_all_cytokines.png", height = 6, width = 18)
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/20221208_WBA_all_cytokines.png", height = 9, width = 15)
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/non_responders_WBA_all_cytokines.png", height = 5, width = 15)
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/responders_WBA_all_cytokines.png", height = 5, width = 15)
ggsave(WBA_boxplot(p), filename = "~/acuitas/export/lipid_responders_WBA_all_cytokines.png", height = 5, width = 10)










# functions for stacked barplot
custom_filter_df <- function(x, y){
  list=list()
  for (i in seq_along(unique(x[,y]))){
    list[[i]]=filter(x, x[,y] %in% unique(x[,y])[i])}
  return(list)
}

custom_t_test <- function(x, y, z){
  df=data.frame()
  for (i in seq_along(z)){
    tryCatch({
      res=rstatix::t_test(data=x%>%group_by(donor_id), y, z[i]) %>%
        rstatix::add_xy_position(x="treatment", comparisons=z)
      df=rbind(df,res)},
      error=function(x, y, z){df=df})}
  return(df)
}

custom_t_test_helper <- function(x){
  for (i in seq_along(x)){
    df = custom_t_test(x[[i]], concentration~treatment, MyComparisons1)
    
    data <- x[[i]] %>%
      select(concentration, treatment) %>%
      group_by(treatment) %>%
      summarize_if(is.numeric, sum)
    
    data <- max(data$concentration)
    df[,"max_sum_concentration"] = data
    df[,"y.position_gap"] = df[2,"y.position"] - df[1,"y.position"]
    df[,"y.position"] = df[,"max_sum_concentration"] + df[,"y.position_gap"]*df[,"xmin"]*4
    
    x[[i]] = df
  }
  return(x)
}

max_xy_position <- function(x){
  combinations = unique(x[,c("group1", "group2")])
  yposition = list()
  for (i in 1:nrow(x)){
    subset = x %>% filter(group1 %in% combinations[i,1] & group2 %in% combinations[i,2])
    subset[,"y.position"] = max(subset$y.position)
    yposition[[i]] = subset}
  yposition=bind_rows(yposition)
  return(yposition)
}

WBA_barplot <- function(i) {
  
  t_test = custom_filter_df(i, "donor_id")
  t_test = custom_t_test_helper(t_test)
  t_test = bind_rows(t_test)
  t_test = max_xy_position(t_test)
  
  anova_test = i %>%
    group_by(donor_id) %>%
    anova_test(concentration~treatment, dv = concentration, wid = donor_id)
  
  multiple_comparisons_test = i %>%
    group_by(donor_id) %>%
    dunn_test(concentration~treatment, p.adjust.method = "holm", detailed = TRUE)
  
  res = left_join(t_test, multiple_comparisons_test,
                  by = intersect(colnames(t_test)[1:6], colnames(multiple_comparisons_test)[1:6]),
                  suffix = c(".t_test", ".multiple_comparisons_test"))
  
  ggplot(i, aes(y = concentration, x = treatment)) + 
    geom_col(aes(fill = Assay), position = "stack") +
    scale_fill_manual(values = MyColors) +
    theme_bw() + 
    labs(x = NULL, y = "Cytokines [pg/mL]", color = "Assay") +
    facet_wrap("donor_id", scales = "fixed", ncol = 4) +
    theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.ticks.x = element_blank(), axis.text = element_text(color = "black"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15), axis.title.y = element_text(vjust = 0.5, size = 15), legend.title = element_blank(), legend.text = element_text(size = 10)) +
    ggpubr::stat_pvalue_manual(res, label = "p.adj.t_test") +
    guides(color=guide_legend(ncol=1))
}

ggsave(WBA_barplot(p), filename = "~/acuitas/export/non_responders_WBA_all_cytokines_MC_test.png", height = 7.5, width = 12.5)
ggsave(WBA_barplot(p), filename = "~/acuitas/export/responders_WBA_all_cytokines_MC_test.png", height = 7.5, width = 15)
ggsave(WBA_barplot(p), filename = "~/acuitas/export/lipid_responders_WBA_all_cytokines_MC_test.png", height = 7.5, width = 10)

ggsave(WBA_barplot(p), filename = "~/acuitas/export/non_responders_WBA_all_cytokines_t_test.png", height = 7.5, width = 12.5)
ggsave(WBA_barplot(p), filename = "~/acuitas/export/responders_WBA_all_cytokines_t_test.png", height = 7.5, width = 15)
ggsave(WBA_barplot(p), filename = "~/acuitas/export/lipid_responders_WBA_all_cytokines_t_test.png", height = 7.5, width = 10)






