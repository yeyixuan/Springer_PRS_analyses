library(survival)
library(pROC)
library(stringr)
library(data.table)
library(dplyr)

args <- commandArgs(trailingOnly = T)
disease <- args[1]

pheno_file_path <- paste0("pheno/ukbb_", disease, ".tsv")
res_file_path <- paste0("result_test/test_auc_", disease, "_age.csv")

train_id <- fread("pheno/ukbb_eur_train.id") %>%
    pull(id)
test_data <- fread(pheno_file_path) %>%
    filter(!eid %in% train_id) %>%
    rename("FID" = "eid") %>%
    rename("status" = disease) %>%
    mutate(status = ifelse(status >= 1, 2, 1))
pop_file <- fread("pheno/ethnic_unrelated.tsv") %>%
    as.data.frame()
best_auc <- fread(paste0("result_train/best_auc_", disease, "_age.csv")) %>%
    as.data.frame()

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

test_main <- function(data, prs_name, method, population) {
    data$prs <- unlist(data[, ..prs_name])	
    res_cox_prs <- coxph(Surv(age_end, status) ~ prs, data = data)
    prediction_prs <- predict(res_cox_prs, newdata = data, type = "risk")
    roc_prs <- roc(data$status, prediction_prs, col = "1", lwd = 1, main = "ROC Curves", plot = F)

    test_res <- data.frame(
        population = population, method = method, prs = prs,
        auc =roc_prs$auc, auc_LL = ci.auc(roc_prs)[1],
        auc_UL = ci.auc(roc_prs)[3], n = get_n(data))

    if (!file.exists(res_file_path)) {
        write.table(test_res, res_file_path, quote = T, sep = ",", col.names = T, append = T)
    }else {
        write.table(test_res, res_file_path, quote = T, sep = ",", col.names = F, append = T)
    }
}

for (i in 1:nrow(best_auc)) {
    method <- best_auc[i, "method"]
    prs_name <- best_auc[i, "prs"]
    score <- fread(paste0("prs/", disease, "_anno1500_tier4_score.tsv"), header = F)
    for (pop in c("EUR", "AFR", "SAS", "AMR", "EAS", "MIX")) {
        pop_id <- pop_file$V1[which(pop_file$V3 == pop)]
	test_pop <- test_data %>%
            filter(FID %in% pop_id) %>%
            inner_join(score, by = "FID")
        test_main(data = test_pop, prs_name = prs_name, method = method, population = pop)
    }	
}
