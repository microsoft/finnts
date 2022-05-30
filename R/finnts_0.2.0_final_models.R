#' Select Best Models and Prep Final Outputs
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
final_models <- function(refit_predictions_tbl,
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
  back_test_mape <- predictions_tbl %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID), 
                  Target = ifelse(Target == 0, 0.1, Target)) %>%
    dplyr::filter(Train_Test_ID != 1) %>%
    dplyr::mutate(MAPE = round(abs((Forecast - Target) / Target), digits = 4)) 

  best_model_tbl <- back_test_mape %>%
    dplyr::group_by(Model_Name, Combo) %>%
    dplyr::mutate(Combo_Total = sum(abs(Target), na.rm = TRUE), 
                  weighted_MAPE = (abs(Target)/Combo_Total)*MAPE) %>%
    dplyr::summarise(Rolling_MAPE = sum(weighted_MAPE, na.rm=TRUE)) %>%
    dplyr::arrange(Rolling_MAPE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Combo) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Best_Model = "Yes") %>%
    dplyr::select(Combo, Model_Name, Best_Model)
  
  back_test_mape_final <- back_test_mape %>%
    dplyr::left_join(best_model_tbl) %>%
    dplyr::mutate(Best_Model = ifelse(!is.na(Best_Model), "Yes", "No"), 
                  Train_Test_ID = Train_Test_ID -1) %>%
    dplyr::rename(Back_Test_Scenario = Train_Test_ID, 
                  Model = Model_Name) %>%
    dplyr::group_by(Combo, Model, Back_Test_Scenario) %>%
    dplyr::mutate(Horizon = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(Combo, Model, Back_Test_Scenario, Horizon, Date, Forecast, Target, MAPE, Best_Model)

  return(back_test_mape_final)
}