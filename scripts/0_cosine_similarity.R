# =============================================================================
# ConceptNet / Numberbatch — pairwise semantic distances between Polish concepts
# =============================================================================
#
# WHAT THIS DOES
#   Builds a pairwise "semantic distance" matrix for a fixed list of Polish
#   words from ConceptNet Numberbatch word embeddings, offline.
#   Distance = 1 - cosine similarity, which reproduces ConceptNet's
#   /relatedness measure (the API computes exactly this cosine internally).
#
# WHY OFFLINE
#   The hosted ConceptNet API (api.conceptnet.io) returns 502 Bad Gateway, so
#   we compute the same quantity locally from the downloadable vector file.
#
# DATA
#   ConceptNet Numberbatch 19.08, multilingual, 300 dimensions, word2vec text
#   format. Downloaded once from:
#   https://conceptnet.s3.amazonaws.com/downloads/2019/numberbatch/numberbatch-19.08.txt.gz
#
# EXPECTED FOLDER LAYOUT
#   <parent>/
#     scripts/      <- this script
#     Numberbatch/  <- numberbatch-19.08.txt.gz  (+ cached Polish subset)
#     output/       <- created automatically; results land here
#
# OUTPUT
#   output/conceptnet_distance.csv     (rounded, human-readable)
#   output/conceptnet_distance.rds     (full precision, for further R work)
#   output/conceptnet_similarity.csv
#
# Author:  Aleksandra Ćwiek
# Updated: 2026-06-09
# =============================================================================


# ---- 0. Configuration: paths -------------------------------------------------
# We figure out where THIS script lives, then derive everything from its parent
# folder. Works when run in RStudio or via Rscript. If auto-detection ever fails,
# just hard-code `parent_dir` on the marked line and ignore the detection block.

script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    dirname(rstudioapi::getSourceEditorContext()$path)          # interactive RStudio
  } else {
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
    if (length(f)) dirname(normalizePath(f)) else getwd()       # Rscript / fallback
  }
}, error = function(e) getwd())

parent_dir <- dirname(script_dir)   # <-- or set manually, e.g. "C:/Users/you/project"
data_dir   <- file.path(parent_dir, "Numberbatch")
out_dir    <- file.path(parent_dir, "output")
dir.create(out_dir, showWarnings = FALSE)

gz_file <- file.path(data_dir, "numberbatch-19.08.txt.gz")
pl_file <- file.path(data_dir, "numberbatch-pl.txt")   # cached Polish-only subset


# ---- 1. Packages -------------------------------------------------------------
library(data.table)   # fast reader for the vector file


# ---- 2. Extract the Polish vectors (runs only the first time) ----------------
# The full file holds ~9 million terms across ~78 languages — too big to load
# into memory. We keep only lines whose term URI starts with "/c/pl/" and cache
# them to numberbatch-pl.txt. On later runs the cache exists and this is skipped.
# We read in binary and write with useBytes = TRUE so Polish diacritics (ł, ó,
# ś, ...) survive regardless of the system locale; the "/c/pl/" test is ASCII.

if (!file.exists(pl_file)) {
  message("Polish subset not found — extracting from the .gz (one-off, it can take some time)...")
  con <- gzfile(gz_file, open = "rb")
  out <- file(pl_file, open = "wb")
  repeat {
    lines <- readLines(con, n = 100000, warn = FALSE)
    if (!length(lines)) break
    keep <- lines[startsWith(lines, "/c/pl/")]
    if (length(keep)) writeLines(keep, out, useBytes = TRUE)
    cat(".")
  }
  close(out); close(con); cat("\n")
  message("Cached Polish subset to: ", pl_file)
} else {
  message("Using cached Polish subset: ", pl_file)
}


# ---- 3. Load the Polish vectors ----------------------------------------------
# Word2vec text format: column 1 = term URI (e.g. /c/pl/okno), columns 2-301 =
# the 300 embedding dimensions. Our filtered file has no header line.
nb    <- fread(pl_file, header = FALSE, quote = "", encoding = "UTF-8")
terms <- nb$V1
mat   <- as.matrix(nb[, -1])
rownames(mat) <- terms


# ---- 4. The concepts to compare ----------------------------------------------
concepts <- c("okno", "krzesło", "wieszak", "obraz", "pokój", "piwnica",
              "laboratorium", "miasto", "schody", "kanapa", "drzewo",
              "dywan", "dom", "strych", "góry", "wieś")
keys <- paste0("/c/pl/", concepts)   # ConceptNet URI form for each word


# ---- 5. Coverage check -------------------------------------------------------
# Numberbatch may lack a specific inflected form. Anything printed here is
# missing — swap it for its lemma (e.g. "góry" -> "góra"). To discover which
# related forms DO exist, run e.g.:  grep("gór", terms, value = TRUE)
missing <- concepts[!keys %in% terms]
if (length(missing)) warning("Not in Numberbatch: ", paste(missing, collapse = ", "))
print(missing)

present <- concepts[keys %in% terms]   # continue with the words we actually have


# ---- 6. Cosine similarity  ->  distance --------------------------------------
# L2-normalize each vector so a dot product equals the cosine, take all pairwise
# products in one matrix multiply, then convert to distance: d = 1 - cosine.
# Interpretation: 0 = identical, ~1 = unrelated, up to 2 = opposite direction.
V   <- mat[paste0("/c/pl/", present), , drop = FALSE]
Vn  <- V / sqrt(rowSums(V^2))
sim <- Vn %*% t(Vn)
dimnames(sim) <- list(present, present)

dist_mat <- 1 - sim


# ---- 7. Save the results -----------------------------------------------------
write.csv(round(sim,      4), file.path(out_dir, "conceptnet_similarity.csv"), fileEncoding = "UTF-8")
write.csv(round(dist_mat, 4), file.path(out_dir, "conceptnet_distance.csv"), fileEncoding = "UTF-8")
saveRDS(dist_mat,             file.path(out_dir, "conceptnet_distance.rds"))
saveRDS(V,                    file.path(out_dir, "conceptnet_vectors.rds"))   # 16 x 300, rownames = concepts
message("Saved similarity + distance matrices to: ", out_dir)


# ---- 8. Quick visual sanity check ---------------------------------
# Not part of "creating the embedding"; eyeball that it looks sane.
d <- as.dist(dist_mat)
plot(hclust(d, method = "average"),
     main = "ConceptNet (Numberbatch) — Polish concepts", xlab = "", sub = "")
mds <- cmdscale(d, k = 2)
plot(mds, type = "n", xlab = "", ylab = ""); text(mds, labels = present)