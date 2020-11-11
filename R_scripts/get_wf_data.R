
library(dplyr)
library(stringr)
library(filesstrings)
library(rstudioapi)

# Set working directory to be the same as source file
setwd(dirname(getActiveDocumentContext()$path))


#### Get files ########
#  Read in manifest for file paths
NAM_3km_manifest <- read.table(file = file.path("./NAM-3km_manifest.txt"), colClasses = c("NULL", "character"),
                               col.names = c("", "path"))[, "path"] %>% str_replace("./", "") %>% str_subset(".nc")
NAM_3km_paths    <- file.path("https://haze.airfire.org/bluesky-daily/output/hysplit-pp/NAM-3km", NAM_3km_manifest)
write.table(NAM_3km_paths, file = file.path("./NAM-3km_paths.txt"), row.names=FALSE, col.names=FALSE, quote=FALSE)

################################ Process .nc and .tgz files storing all .nc files ##############################
# Read in .nc files
nc.data    <- list.files(file.path("NAM-3km_data"), pattern = "smoke_dispersion.nc", recursive = T, full.names = T)
nc.mod.typ <- str_split(nc.data, pattern="/", simplify=T)[,12] %>% str_trunc(4, "right", "")
nc.dates   <- str_extract(nc.data, "[:digit:]{10}")
file.rename(nc.data, paste0(nc.dates, "_smoke_", nc.mod.typ, ".nc"))  
move_files(list.files(file.path(dir), pattern = "smoke.nc", recursive = F, full.names = T), file.path(dir, "raw_exp_data/bluesky_daily_NAM_3km"))
setwd(file.path(dir, "raw_exp_data/bluesky_daily_NAM_3km"))
unlink(file.path(dir, "raw_exp_data/bluesky_daily_NAM_3km", nc.dates), recursive = T)
