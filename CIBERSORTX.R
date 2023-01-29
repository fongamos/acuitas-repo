# import single cell liver data
seurat <- readRDS("~/acuitas/amos files/cibersort/Seurat4.0_Liver.rds")
ggsave(DimPlot(seurat, group.by="Celltype"), file="~/acuitas/export/dimplot_NHP.png")

# retrieve count matrix
count <- seurat@assays[["RNA"]]@counts

# rename colnames to Celltype
colnames(count) <- seurat@meta.data$Celltype

# filter by Celltype
# <=2500 samples
# transform to df
Central_hepatocyte <- count[,colnames(count) == "Central hepatocyte"] %>% .[,c(1:2000)] %>% as.data.frame(.)  
Portal_hepatocyte <- count[,colnames(count) == "Portal hepatocyte"] %>% .[,c(1:2000)] %>% as.data.frame(.)
Midzonal_hepatocyte <- count[,colnames(count) == "Midzonal hepatocyte"] %>% .[,c(1:2000)] %>% as.data.frame(.)
Monocyte <- count[,colnames(count) == "Monocyte"] %>% .[,c(seq_along(.))] %>% as.data.frame(.)
Endothelial_cell <- count[,colnames(count) == "Endothelial cell"] %>% .[,c(1:2000)] %>% as.data.frame(.)
Kupffer_cell <- count[,colnames(count) == "Kupffer cell"] %>% .[,c(seq_along(.))] %>% as.data.frame(.)
Stellate_cell <- count[,colnames(count) == "Stellate cell"] %>% .[,c(seq_along(.))] %>% as.data.frame(.)
Cholangiocyte <- count[,colnames(count) == "Cholangiocyte"] %>% .[,c(seq_along(.))] %>% as.data.frame(.)
Dendritic_cell <- count[,colnames(count) == "Dendritic cell"] %>% .[,c(seq_along(.))] %>% as.data.frame(.)
  
# cbind
# transform to dgCMatrix
export <- cbind(Central_hepatocyte,
                Portal_hepatocyte,
                Midzonal_hepatocyte,
                Monocyte,
                Endothelial_cell,
                Kupffer_cell,
                Stellate_cell,
                Cholangiocyte,
                Dendritic_cell) %>%
  as.matrix()

nrow(export)
length(unique(rownames(export)))

# cbind
# transform to dgCMatrix
export <- cbind(Central_hepatocyte,
                Portal_hepatocyte,
                Midzonal_hepatocyte,
                Monocyte,
                Endothelial_cell,
                Kupffer_cell,
                Stellate_cell,
                Cholangiocyte,
                Dendritic_cell) %>%
  as.matrix() %>%
  Matrix(sparse=TRUE)

# export
write_sparse_tsv(export, "~/acuitas/export/reference_matrix.tsv")











# import raw data
# file should have Mixture, n cell types, P.value, Correlation, RMSE
cibersortx <- read.delim("~/acuitas/amos files/cibersort/ACUTH16_cibersort_results.txt")

# replace all "\\." with " "
# represent raw data with Sample, Proportion and Celltype
list <- list()
for(i in 2:10){
  df=cibersortx[,c(1,i)]
  df=df %>% mutate(Celltype=colnames(df)[2]%>% str_replace_all("\\.", " "))
  colnames(df)[1:2]=c("Sample", "Proportion")
  list[[i-1]]=df
}
df <- bind_rows(list)

# rename Sample
for(i in 1:nrow(df)){
  if(str_detect(df[i,"Sample"], "101")){
    df[i,"Sample"] = "low307_1"}
  else if(str_detect(df[i,"Sample"], "102")){
    df[i,"Sample"] = "low307_2"}
  else if(str_detect(df[i,"Sample"], "103")){
    df[i,"Sample"] = "low307_3"}
  else if(str_detect(df[i,"Sample"], "201")){
    df[i,"Sample"] = "high307_1"}
  else if(str_detect(df[i,"Sample"], "202")){
    df[i,"Sample"] = "high307_2"}
  else if(str_detect(df[i,"Sample"], "203")){
    df[i,"Sample"] = "high307_3"}
  else if(str_detect(df[i,"Sample"], "301")){
    df[i,"Sample"] = "low315_1"}
  else if(str_detect(df[i,"Sample"], "302")){
    df[i,"Sample"] = "low315_2"}
  else if(str_detect(df[i,"Sample"], "303")){
    df[i,"Sample"] = "low315_3"}
  else if(str_detect(df[i,"Sample"], "401")){
    df[i,"Sample"] = "high315_1"}
  else if(str_detect(df[i,"Sample"], "402")){
    df[i,"Sample"] = "high315_2"}
  else if(str_detect(df[i,"Sample"], "403")){
    df[i,"Sample"] = "high315_3"}
  else if(str_detect(df[i,"Sample"], "501")){
    df[i,"Sample"] = "low366_1"}
  else if(str_detect(df[i,"Sample"], "502")){
    df[i,"Sample"] = "low366_2"}
  else if(str_detect(df[i,"Sample"], "503")){
    df[i,"Sample"] = "low366_3"}
  else if(str_detect(df[i,"Sample"], "601")){
    df[i,"Sample"] = "high366_1"}
  else if(str_detect(df[i,"Sample"], "602")){
    df[i,"Sample"] = "high366_2"}
  else if(str_detect(df[i,"Sample"], "603")){
    df[i,"Sample"] = "high366_3"}
  else if(str_detect(df[i,"Sample"], "701")){
    df[i,"Sample"] = "low535_1"}
  else if(str_detect(df[i,"Sample"], "702")){
    df[i,"Sample"] = "low535_2"}
  else if(str_detect(df[i,"Sample"], "703")){
    df[i,"Sample"] = "low535_3"}
  else if(str_detect(df[i,"Sample"], "801")){
    df[i,"Sample"] = "high535_1"}
  else if(str_detect(df[i,"Sample"], "802")){
    df[i,"Sample"] = "high535_2"}
  else if(str_detect(df[i,"Sample"], "803")){
    df[i,"Sample"] = "high535_3"}
  else{df[i,"Sample"] = df[i,"Sample"]}
}

# create new column for dose
dose <- c()
for(i in 1:nrow(df)){
  if(str_detect(df[i,"Sample"], "high")){
    dose=c(dose, "1.5mg/kg")}
  else if(str_detect(df[i,"Sample"], "low")){
    dose=c(dose, "0.5mg/kg")}
}
df[,"dose"] <- dose

# create named vector for colors
MyColors <- rand_color(length(unique(df$Celltype)))
names(MyColors) <- unique(df$Celltype) 

# plot stacked barplot
ggplot(df, aes(x=Sample, y=Proportion)) + 
  geom_col(aes(fill=Celltype), width = 0.9, color = NA, position = "fill") +
  theme_bw() +
  labs(x = NULL, y = "Proportion of Celltype") +
  facet_wrap("dose", scales = "free_x", ncol = 2) +
  theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.ticks.x = element_blank(), axis.text = element_text(color = "black"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15), axis.title.y = element_text(vjust = 0.5, size = 15), legend.title = element_blank(), legend.text = element_text(size = 10)) +
  guides(fill=guide_legend(ncol=1))

# export
ggsave(filename="~/acuitas/export/ACUTH16_cibersortx.png", width = 16, height = 8)










# QC
signature_matrix <- read.delim("~/acuitas/amos files/cibersort/signature_matrix.txt")
length(unique(signature_matrix$NAME)) == nrow(signature_matrix)

count_matrix <- read.delim("~/acuitas/amos files/cibersort/mixture.tsv")
length(unique(count_matrix$X)) == nrow(count_matrix)
test <- count_matrix[duplicated(count_matrix),]

reference_matrix <- read.delim("~/acuitas/amos files/cibersort/reference_matrix.tsv")
length(unique(reference_matrix$X)) == nrow(reference_matrix)
test <- reference_matrix[duplicated(reference_matrix),]



