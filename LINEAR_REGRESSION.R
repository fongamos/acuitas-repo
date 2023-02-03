# import raw data 
# file should have Class, pKa, Encaps, Size, PDI, Liver.fLuc.0.3mpk, Liver.fLuc.1.0mpk, Spleen.fLuc.0.3mpk, Spleen.fLuc.1.0mpk
df <- read.csv("~/acuitas/amos files/linear_regression.csv")

# rename col
colnames(df) <- c("lipid", "pKa", "encapsulation", "size", "PDI", "liver_concentration_0.3", "liver_concentration_1", "spleen_concentration_0.3", "spleen_concentration_1")
df$lipid <- as.character(df$lipid)

# create new columns concentration, tissue, dose
df1 <- df[,c(1:6)] %>% mutate(tissue = "liver", dose = "0.3") 
colnames(df1)[6] <- "concentration"
df2 <- df[,c(1:5,7)] %>% mutate(tissue = "liver", dose = "1")
colnames(df2)[6] <- "concentration"
df3 <- df[,c(1:5,8)] %>% mutate(tissue = "spleen", dose = "0.3")
colnames(df3)[6] <- "concentration"
df4 <- df[,c(1:5,9)] %>% mutate(tissue = "spleen", dose = "1")
colnames(df4)[6] <- "concentration"

df <- rbind(df1, df2, df3, df4)

# create facet titles
dose_label <- c("Low Dose 0.3 [mg/kg]", "High Dose 1.0 [mg/kg]")
names(dose_label) <- c("0.3", "1")









# filter by metadata
model_df <- df %>% filter(tissue == "liver" & dose == "1")
model_df <- df %>% filter(tissue == "spleen" & dose == "1")

# generate models
mod1 <- lm(concentration ~ poly(pKa, 1, raw=T), data = model_df)
mod2 <- lm(concentration ~ poly(pKa, 2), data = model_df)
mod3 <- lm(concentration ~ poly(pKa, 3), data = model_df)
mod4 <- lm(concentration ~ poly(pKa, 4), data = model_df)
mod5 <- lm(concentration ~ poly(pKa, 5), data = model_df)

# grid search
grid <- model_df %>%
  data_grid(pKa = seq_range(pKa, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5)

# residuals 
model_df <- model_df %>%
  add_residuals(mod1, "resid1") %>%
  add_residuals(mod2, "resid2") %>%
  add_residuals(mod3, "resid3") %>%
  add_residuals(mod4, "resid4") %>%
  add_residuals(mod5, "resid5")

# plot grid search
ggplot(model_df, aes(pKa, concentration)) + 
  geom_point() +
  geom_line(aes(y=pred), data = grid, colour = "red") +
  facet_wrap(~ model)

# plot residuals
ggplot(model_df, aes(pKa, resid1)) + 
  geom_point() +
  geom_ref_line(h = 0)
ggplot(model_df, aes(pKa, resid2)) + 
  geom_point() +
  geom_ref_line(h = 0)
ggplot(model_df, aes(pKa, resid3)) + 
  geom_point() +
  geom_ref_line(h = 0)
ggplot(model_df, aes(pKa, resid4)) + 
  geom_point() +
  geom_ref_line(h = 0)
ggplot(model_df, aes(pKa, resid5)) + 
  geom_point() +
  geom_ref_line(h = 0)









# create variable for formula
my_formula <- concentration ~ poly(pKa, 5)
coef(mod5)

# filter grid for model
my_grid <- grid %>% filter(model == "mod5")

# create named vector for custom colors (19)
lipids <- unique(df$lipid)
MyColors <- colorRampPalette(c(rev(brewer.pal(9, "YlOrRd")), brewer.pal(9, "YlGnBu")[2:9]))(length(unique(df$lipid)))
names(MyColors) <- lipids

# function for linear regression curve
linear_regression <- function(i, j, k){
  df=i %>% filter(tissue == j)
  ggplot(df, aes_string(y = "concentration", x = "pKa")) + 
    geom_point(aes_string(color = "lipid"), size = 2.5) +
    geom_line(aes(y=pred), data = my_grid, colour = "red") +
    scale_color_manual(values = MyColors) +
    theme_bw() + 
    labs(x = "pKa", y = "Relative fold change", color = "lipid") +
    facet_wrap("dose", scales = "fixed", ncol = 3, labeller = labeller(dose = dose_label)) +
    theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.ticks.x = element_blank(), axis.text = element_text(color = "black", size = 15), axis.text.x = element_text(size = 15), axis.title.y = element_text(vjust = 0.5, size = 15), axis.title.x = element_text(vjust = 0.5, size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 10))
}

# save plot (make sure to remake model based on metadata)
ggsave(linear_regression(df, "liver", my_formula), filename = "~/acuitas/export/linear_regression_liver.png", height = 8, width = 16)
ggsave(linear_regression(df, "spleen", my_formula), filename = "~/acuitas/export/linear_regression_spleen.png", height = 8, width = 16)














# create named vector for colors
dose <- unique(df$dose)
MyColors <- brewer.pal(9, "Set1")[1:2]
names(MyColors) <- dose

linear_regression_combined <- function(i, j){
  df=i %>% filter(tissue == j)
  ggplot(df, aes_string(y = "concentration", x = "pKa", color = "dose")) + 
    geom_point(aes_string(color = "dose"), size = 2.5) +
    stat_smooth(method = "lm", formula = y ~ poly(x, 5), se = FALSE, size = 1) +
    ggpubr::stat_regline_equation(formula = y ~ poly(x, 5)) +
    scale_color_manual(values = MyColors) +
    theme_bw() + 
    labs(x = "pKa", y = "Relative fold change", color = "dose") +
    theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.ticks.x = element_blank(), axis.text = element_text(color = "black", size = 15), axis.text.x = element_text(size = 15), axis.title.y = element_text(vjust = 0.5, size = 15), axis.title.x = element_text(vjust = 0.5, size = 15), legend.title = element_text(size = 15), legend.text = element_text(size = 10))
}

# save plot 
ggsave(linear_regression_combined(df, "liver"), filename = "~/acuitas/export/linear_regression_liver2.png", height = 8, width = 16)
ggsave(linear_regression_combined(df, "spleen"), filename = "~/acuitas/export/linear_regression_spleen2.png", height = 8, width = 16)
