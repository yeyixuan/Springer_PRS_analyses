library(survival)
library(pROC)
library(stringr)
library(data.table)
library(dplyr)

args <- commandArgs(trailingOnly=T)
disease <- args[1]
method <- args[2]
prs_file_path <- paste0("prs/", disease, "_", method, "_score.tsv")
pheno_file_path <- paste0("pheno/ukbb_", disease, ".tsv")
res_file_path <- paste0("result_train/auc_", disease, "_age.csv")

score <- fread(prs_file_path)
pheno <- fread(pheno_file_path)
train_id <- fread("pheno/ukbb_eur_train.id") %>%
    pull(id)
train_data <- pheno %>%
    filter(eid %in% train_id) %>%
    rename("FID" = "eid") %>%
    rename("status" = disease) %>%
    mutate(status = ifelse(status >= 1, 2, 1)) %>%
    inner_join(score, by = "FID")

get_n <- function(data) {
    n_case <- data %>%
        filter(status == 2) %>%
        nrow()
    n_control <- data %>%
        filter(status == 1) %>%
        nrow()
    n <- paste0(n_case, "/", n_control)
    return(n)
}

for (i in 13:ncol(train_data)) {
    train_data <- train_data %>%
        mutate(prs = cur_data()[[i]])

    res_cox_prs <- coxph(Surv(age_end, status) ~ prs , data = train_data)
    prediction_prs <- predict(res_cox_prs, newdata = train_data, type = "risk")
    roc_prs <- roc(train_data$status, prediction_prs, col = "1", lwd = 1, main = "ROC Curves", plot = F)
    
    train_res <- data.frame(
        method = method, prs = names(train_data)[i],
        auc = roc_prs$auc, auc_LL = ci.auc(roc_prs)[1],
        auc_UL = ci.auc(roc.prs)[3], n = get_n(train_data))
    
    if (!file.exists(res_file_path)) {
        write.table(train_res, res_file_path, quote = T, sep = ",", col.names = T, append = T)
    }else {
        write.table(train_res, res_file_path, quote = T, sep = ",", col.names = F, append = T)
    }
}
