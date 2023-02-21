library(data.table)
library(dplyr)

args <- commandArgs(trailingOnly=T)
disease <- args[1]
input_path <- paste0("result_train/auc_", disease, "_age.csv")
output_path <- paste0("result_train/best_auc_", disease, "_age.csv")

best_prs <- fread(input_path) %>%
    group_by(method) %>%
    filter(abs(auc) == max(abs(auc))) %>%
    as.data.frame()

write.table(best, output_path, quote = T, sep = ",", col.names = T)

