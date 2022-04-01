#' Select Best Models
#' 
#' @param refit_predictions_tbl individual model predictions
#' @param ensemble_predictions_tbl ensemble model predictions
#' @param average_predictions_tbl model average predictions
#' @param model_train_test_tbl train test splits
#' @param parallel_processing parallel processing
#' @param num_cores number of cores
#'  
#' @return tbl with best model flag
#' @keywords internal
#' @export
best_models <- function(refit_predictions_tbl,
                        ensemble_predictions_tbl = NULL,
                        average_predictions_tbl = NULL, 
                        model_train_test_tbl, 
                        parallel_processing = NULL, 
                        num_cores = NULL) {
 
  # get model predictions
  train_test_id_list <- model_train_test_tbl %>%
    dplyr::filter(Run_Type != "Ensemble") %>%
    dplyr::pull(Run_ID) %>%
    unique()
  
  predictions_tbl <- refit_predictions_tbl %>%
    dplyr::mutate(Model_Suffix = ifelse(Combo == "All-Data", "global", "local")) %>%
    dplyr::select(Model, Recipe_ID, Model_Suffix, Train_Test_ID, Prediction) %>%
    tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
    dplyr::select(-Model_Suffix, -Model, -Recipe_ID) %>%
    dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
    tidyr::unnest(Prediction) %>%
    rbind(
      if(!is.null(ensemble_predictions_tbl)) {
        ensemble_predictions_tbl %>%
          dplyr::mutate(Model_Suffix = ifelse(Combo == "All-Data", "global", "local")) %>%
          dplyr::select(Model, Recipe_ID, Model_Suffix, Train_Test_ID, Prediction) %>%
          tidyr::unite(col = 'Model_Name', c("Model", "Recipe_ID", "Model_Suffix"), sep= "-", remove = FALSE) %>%
          dplyr::select(-Model_Suffix, -Model, -Recipe_ID) %>%
          dplyr::filter(Train_Test_ID %in% train_test_id_list) %>%
          tidyr::unnest(Prediction)
      } else {
        tibble::tibble()
      }
    ) %>%
    rbind(
      if(!is.null(average_predictions_tbl)) {
        average_predictions_tbl %>%
          dplyr::rename(Model_Name = Model)
      } else {
        tibble::tibble()
      }
    )
  
  # calculate model accuracy
  mape_tbl <- predictions_tbl %>%
    dplyr::filter(Train_Test_ID != "01")
  
  return(predictions_tbl)
}