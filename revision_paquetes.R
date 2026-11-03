# Revisión de paquetes de datos de transcriptómica en Bioconductor y CRAN
# (cifras de la diapositiva 2). Clasificación por biocViews y palabras clave
# en título/descripción: es una aproximación, no una curaduría manual.
# Corrida: 2 nov 2026, Bioconductor 3.23.

bioc <- function(repo) {
  u <- sprintf("https://bioconductor.org/packages/release/%s/VIEWS", repo)
  as.data.frame(read.dcf(url(u), fields = c("Package", "Title", "Description",
    "biocViews", "vignetteTitles")))
}
txt <- function(d) tolower(gsub("\\s+", " ", paste(d$Package, d$Title, d$Description)))

# --- Bioconductor: paquetes de datos (ExperimentData) ---
e <- bioc("data/experiment")
t <- txt(e)
ti <- tolower(paste(e$Package, e$Title))
tx <- grepl("RNASeqData|ExpressionData|MicroarrayData|SingleCellData|SpatialData", e$biocViews)
enf <- grepl("CancerData|DiseaseModel", e$biocViews) |
  grepl("cancer|tumou?r|disease|patient|clinical|leuka?emia|lupus|autoimmun|diabet|alzheimer|covid|infect|sepsis|carcinoma|lymphoma|glioma|melanoma|asthma|tubercul|arthritis|rheumat|hiv|tcga", ti)
cancer <- grepl("CancerData", e$biocViews) |
  grepl("cancer|tumou?r|carcinoma|leuka?emia|lymphoma|glioma|melanoma|tcga|cll|nci-60", ti)
latam <- "mexic|latin america|brazil|argentin|colombi|chile|peru|hispanic|latino|mestiz"

cat("Bioconductor ExperimentData:", nrow(e), "\n")
cat("  transcriptómica:", sum(tx), "\n")
cat("  + enfermedad humana:", sum(tx & enf), "(cáncer:", sum(tx & enf & cancer), ")\n")
cat("  autoinmunes:", e$Package[grepl("autoimmun|rheumat|arthritis", t)], "\n")
cat("  lupus:", sum(grepl("lupus", t)), "\n")
cat("  datos de Latinoamérica:", sum(grepl(latam, t)), "\n")

# --- Bioconductor: workflows (material educativo) ---
w <- bioc("workflows")
wtx <- grepl("rna-seq|rnaseq|transcript|gene expression|single-cell|single cell|differential expression|microarray", txt(w))
cat("Workflows:", nrow(w), "| de transcriptómica:", sum(wtx),
    "| en español:", sum(grepl("español|spanish", tolower(paste(w$Title, w$vignetteTitles)))), "\n")

# --- CRAN ---
d <- tools::CRAN_package_db()
d <- d[!duplicated(d$Package), ]
ct <- txt(d)
ctx <- grepl("rna-seq|rnaseq|transcriptom|gene expression|scrna|single-cell rna|microarray", ct)
cat("CRAN:", nrow(d), "| mencionan transcriptómica:", sum(ctx),
    "| Latinoamérica:", sum(ctx & grepl(latam, ct)),
    "| español:", sum(ctx & grepl("spanish|español", ct)), "\n")
# Revisados a mano entre los que se describen como datos:
# DAAGbio (microarreglos, para demostraciones), MM2Sdata (meduloblastoma),
# NeuroDataSets (neurología), WallomicsData (Arabidopsis).
