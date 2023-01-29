# transcript abundance quantifiers to create gene-level count matrices for use with DESeq2
# examples: Salmon, Sailfish, kallisto, RSEM

### PART 1: RSEM -> DESeqDataSet
# import RSEM count matrix
files <- list.files("~/acuitas/ACUTH16 PROJECT/amos files/raw_count_matrices", pattern = "\\.txt$")
filepaths <- here::here("~/acuitas/ACUTH16 PROJECT/amos files/raw_count_matrices", files)
df <- lapply(filepaths, read.delim) %>%
  reduce(left_join, by = "GeneID", suffix=c("",".x")) %>%
  select(-ends_with(".x"))

# select columns of interest
cols <- c()
for (i in seq_along(df)){
  if (str_detect(colnames(df)[i], "GeneID|T$|76_V|77_V|78_V"))
    cols = c(cols, i)
}
df <- df[,cols]

# reformat columns
df <- df[,c(1:7,11:25,8:10)]
colnames(df) <- c("GeneID",
                  "low307_1","low307_2","low307_3",
                  "high307_1","high307_2","high307_3",
                  "low315_1","low315_2","low315_3",
                  "high315_1","high315_2","high315_3",
                  "low366_1","low366_2","low366_3",
                  "high366_1","high366_2","high366_3",
                  "low535_1","low535_2","low535_3",
                  "high535_1","high535_2","high535_3")

# create count matrix
cts <- as.matrix(df[,c(2:ncol(df))])
rownames(cts) <- df$GeneID

# create metadata
coldata <- data.frame(lipid = rep(c("307", "315", "366", "535"), c(6,6,6,6)))
rownames(coldata) <- colnames(cts)
dose <- c()
for (i in 1:nrow(coldata)){
  if (str_detect(rownames(coldata)[i], "low")){
    dose = c(dose, "low")}
  if (str_detect(rownames(coldata)[i], "high")){
    dose = c(dose, "high")}
}
coldata <- coldata %>% mutate(dose = dose) %>% mutate(condition = str_c(dose, lipid))

### PART 2: DESeqDataSet -> DE results
### log2FC shrinkage, median count >= 25
# create DESeq2 object
dds <- DESeqDataSetFromMatrix(countData = round(cts),
                              colData = coldata,
                              design = ~ condition)

# exclude samples
subset <- c(grep("low307", colnames(dds)))
dds <- dds[,-subset]
dds$condition <- droplevels(dds$condition)

# pre-filtering for minimum counts
count <- counts(dds) %>% as.data.frame()
count$median <- apply(count, 1, median, na.rm=T)
keep <- count$median >= 25
dds <- DESeq(dds[keep,])

# DE analysis for .csv export
dds$condition <- relevel(dds$condition, ref = "high307")
dds <- DESeq(dds)
res <- lfcShrink(dds, coef="condition_low307_vs_high307", type="apeglm")

dds$condition <- relevel(dds$condition, ref = "high315")
dds <- DESeq(dds)
res <- lfcShrink(dds, coef="condition_high307_vs_high315", type="apeglm")
res <- lfcShrink(dds, coef="condition_low315_vs_high315", type="apeglm")

dds$condition <- relevel(dds$condition, ref = "high366")
dds <- DESeq(dds)
res <- lfcShrink(dds, coef="condition_high307_vs_high366", type="apeglm")
res <- lfcShrink(dds, coef="condition_high315_vs_high366", type="apeglm")
res <- lfcShrink(dds, coef="condition_low366_vs_high366", type="apeglm")

dds$condition <- relevel(dds$condition, ref = "high535")
dds <- DESeq(dds)
res <- lfcShrink(dds, coef="condition_high307_vs_high535", type="apeglm")
res <- lfcShrink(dds, coef="condition_high315_vs_high535", type="apeglm")
res <- lfcShrink(dds, coef="condition_high366_vs_high535", type="apeglm")
res <- lfcShrink(dds, coef="condition_low535_vs_high535", type="apeglm")

dds$condition <- relevel(dds$condition, ref = "low315")
dds <- DESeq(dds)
res <- lfcShrink(dds, coef="condition_low307_vs_low315", type="apeglm")

dds$condition <- relevel(dds$condition, ref = "low366")
dds <- DESeq(dds)
res <- lfcShrink(dds, coef="condition_low307_vs_low366", type="apeglm")
res <- lfcShrink(dds, coef="condition_low315_vs_low366", type="apeglm")

dds$condition <- relevel(dds$condition, ref = "low535")
dds <- DESeq(dds)
res <- lfcShrink(dds, coef="condition_low307_vs_low535", type="apeglm")
res <- lfcShrink(dds, coef="condition_low315_vs_low535", type="apeglm")
res <- lfcShrink(dds, coef="condition_low366_vs_low535", type="apeglm")

# export DE results
write.csv(as.data.frame(subset(res, padj < 0.05)), file="~/acuitas/export/results_name.csv")










# extract transformed count matrix
vsd <- vst(dds, blind=FALSE)
assay(vsd)

# Euclidean distance analysis
sampleDists <- dist(t(assay(vsd)))
sampleDistMatrix <- as.matrix(sampleDists)
rownames(sampleDistMatrix) <- paste(vsd@colData@rownames)
colnames(sampleDistMatrix) <- paste(vsd@colData@rownames)
colors <- colorRampPalette(rev(brewer.pal(9,"Blues")))(max(sampleDists))
simple = HeatmapAnnotation(df=annotation_col, na_col = "black")
annotation_col <- data.frame(colData(dds)[,c("lipid", "dose")])
Heatmap(sampleDistMatrix, name = "dist", col = colorRamp2(seq(0:max(sampleDists)), colors), top_annotation = simple,
        clustering_distance_rows = sampleDists, clustering_distance_columns = sampleDists)

# PCA analysis
pcaData <- plotPCA(vsd, intgroup=c("dose", "lipid"), returnData=TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
ggsave(ggplot(pcaData, aes(PC1, PC2, color=lipid, shape=dose), height = 10, width = 10) +
  geom_point(size=3) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, color = NA), strip.text = element_text(face = "bold", size = 15), axis.text = element_text(color = "black"), legend.text = element_text(size = 10)) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) +
  ylab(paste0("PC2: ",percentVar[2],"% variance")) + 
  coord_fixed()
  ,filename="~/acuitas/export/PCA.png")

# Cook's distance boxplot
# Cook’s distance is a measure of how much a single sample is influencing the fitted coefficients for a gene, and a large value of Cook’s distance is intended to indicate an outlier count
par(mar=c(8,5,2,2))
boxplot(log10(assays(dds)[["cooks"]]), range=0, las=2)

### Recommendations for single-cell analysis

















