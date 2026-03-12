# Test the strsplit logic from the code
model_id_example <- "xgboost_global_R1"
model_id_list <- model_id_example %>%
  strsplit(split = "_") %>%
  unlist()

print("Raw output:")
print(model_id_list)

print("After stringr::str_replace:")
prev_best_model_list <- model_id_list %>%
  stringr::str_replace("--.*$", "") %>%
  unique()
print(prev_best_model_list)

# What if Model_ID format is different?
model_id_example2 <- "xgboost--global--R1"
model_id_list2 <- model_id_example2 %>%
  strsplit(split = "_") %>%
  unlist()

print("\nWith -- separator:")
print(model_id_list2)
