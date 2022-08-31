
hash_data <- function(x) {
  digest::digest(object = x, algo = "xxhash64")
}

write_data <- function(x, 
                       combo, 
                       run_info, 
                       output_type,
                       folder = NULL, 
                       suffix = NULL) {
  
  if(output_type == 'data') {
    file_type <- run_info$data_output
  } else if(output_type == 'log') {
    file_type <- "csv"
  } else {
    file_type <- run_info$object_output
  }
  
  if(is.null(combo)) {
    combo_hash <- NULL
  } else {
    combo_hash <- paste0('-', hash_data(combo))
  }
  
  # write to temp folder
  temp_path <- NULL
  
  if(inherits(run_info$storage_object, c("blob_container", "ms_drive")) || 
     (inherits(run_info$storage_object, 'NULL') & is.null(run_info$path))) {
    
    fs::dir_create(tempdir(), folder)
    
    temp_path <- paste0(fs::path(tempdir(), folder), '\\', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), combo_hash, suffix, '.', file_type)
    
    write_data_type(x, temp_path, file_type)
  }
  
  # write to final folder output
  if(inherits(run_info$storage_object, c("blob_container", "ms_drive", "NULL")) & !is.null(run_info$path)) {
    
    if(is.null(run_info$storage_object)) {
      fs::dir_create(run_info$path, folder)
    }
    
    final_path <- paste0(fs::path(run_info$path, folder), '/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), combo_hash, suffix, '.', file_type)

    write_data_folder(x, 
                      run_info$storage_object, 
                      class(run_info$storage_object)[[1]], 
                      temp_path, 
                      final_path, 
                      file_type)
  }
  
}

write_data_type <- function(x, 
                            path, 
                            type) {
  
  switch(type, 
         rds = saveRDS(x, path), 
         parquet = arrow::write_parquet(x, path), 
         csv = utils::write.csv(x, path, row.names = FALSE), 
         qs = qs::qsave(x, path))
  
}

write_data_folder <- function(x, 
                              storage_object, 
                              final_dest,
                              temp_path, 
                              final_path, 
                              type) {
  
  options(azure_storage_progress_bar=FALSE)
  
  switch(final_dest, 
         'NULL' = write_data_type(x, final_path, type), 
         blob_container = AzureStor::storage_upload(storage_object, src = temp_path, dest = final_path), 
         ms_drive = storage_object$upload_file(src = temp_path, dest = final_path))
  
}

#' test
#'  
#' @return table
#' @keywords internal
#' @export
list_files <- function(storage_object, 
                       path) {
  
  dir <- fs::path_dir(path)
  file <- fs::path_file(path)
  
  switch(class(storage_object)[[1]], 
         'NULL' = if(grepl("*", file, fixed = TRUE)) {fs::dir_ls(path = dir, glob = file)} else{path},
         ms_drive = storage_object$list_files(dir) %>% dplyr::filter(grepl(file, name)) %>% dplyr::pull(name), 
         ms_blob = AzureStor::list_storage_files(storage_object, dir) %>% dplyr::filter(grepl(file, name)) %>% dplyr::pull(name))
  
}

download_file <- function(storage_object, 
                           path, 
                           folder) {
  
  options(azure_storage_progress_bar=FALSE)
  
  fs::dir_create(tempdir(), folder)
  
  dest_path <- fs::path(tempdir(), folder)
  
  if(inherits(storage_object, c("ms_drive", "NULL"))) {
    files <- list_files(storage_object, path)
  }
  
  switch(class(storage_object)[[1]], 
         blob_container = if(grepl("*", fs::path_file(path), fixed = TRUE)) {AzureStor::storage_multidownload(storage_object, src = path, dest = dest_path, overwrite = TRUE)}
                                                                        else{AzureStor::storage_download(storage_object, src = path, dest = paste0(dest_path, '/', fs::path_file(path)), overwrite = TRUE)}, 
         ms_drive = for(file in files) {storage_object$download_file(src = fs::path(fs::path_dir(path), file), dest = paste0(dest_path, '/', fs::path_file(file)), overwrite = TRUE)})
}

read_data_type <- function(x, 
                           path, 
                           type) {
  
  switch(type, 
         rds = readRDS(path), 
         parquet = arrow::read_parquet(path), 
         csv = utils::read.csv(path), 
         qs = qs::qread(path))
  
}

read_file <- function(run_info, 
                      path, 
                      return_type = 'df') {
  
  folder <- fs::path_dir(path)
  storage_object <- run_info$storage_object
  initial_path <- run_info$path
  file <- fs::path_file(path)

  if(inherits(storage_object, c("blob_container", "ms_drive"))) {
    download_file(storage_object, fs::path(initial_path, path), folder)
    final_path <- fs::path(tempdir(), folder)
    files <- list_files(NULL, fs::path(final_path, file))
  } else if(is.null(run_info$path)) {
    final_path <- fs::path(tempdir(), folder)
    files <- list_files(storage_object, fs::path(final_path, file))
  } else {
    #path <- fs::path(path, folder)
    files <- list_files(storage_object, fs::path(initial_path, path))
  }

  if(return_type == 'df') {
    switch(fs::path_ext(file), 
           rds = readRDS(files), 
           parquet = files %>% purrr::map_dfr(function(path) {arrow::read_parquet(path)}),
           csv = tryCatch(
             vroom::vroom(files, show_col_types = FALSE, altrep = FALSE, delim = ","),
             error = function(e){
               files %>% purrr::map(function(path) {read.csv(path, stringsAsFactors = FALSE)}) %>% plyr::rbind.fill() %>% tibble()}),
           qs = qs::qread(path))
  } else if(return_type == "sdf") {
    switch(fs::path_ext(file), 
           parquet = sparklyr::spark_read_parquet(sc, path = fs::path(initial_path, path)), 
           csv = sparklyr::spark_read_csv(sc, path = fs::path(initial_path, path)))
  }
}

get_recipe_data <- function(run_info, 
                            combo = 'single') {
  
  get_combo <- function(df, 
                        combo) {
    
    if(combo == "single") {
      df %>%
        dplyr::filter(Combo == .$Combo[[1]])
    } else if(combo == "All-Data") {
      df
    } else {
      df %>%
        dplyr::filter(Combo == combo)
    }
  }
  
  
  recipe_tbl <- tibble::tibble()
  
  file_name_tbl <- list_files(run_info$storage_object, 
                              paste0(run_info$path, "/prep_data/*", hash_data(run_info$experiment_name), '-', 
                                     hash_data(run_info$run_name), "*.", run_info$data_output)) %>%
    tibble::tibble(Path = .,
                   File = fs::path_file(.)) %>%
    tidyr::separate(File, into = c("Experiment", "Run", "Combo", "Recipe"), sep = '-', remove = TRUE) %>%
    get_combo(combo) %>%
    dplyr::mutate(Recipe = substr(Recipe, 1, 2))

  for(recipe in unique(file_name_tbl$Recipe)) {
    
    temp_path <- file_name_tbl %>%
      dplyr::filter(Recipe == recipe)

    if(nrow(temp_path) > 1) {
      temp_path <- paste0("/prep_data/*", hash_data(run_info$experiment_name), '-', 
                          hash_data(run_info$run_name), "*", recipe, '.', run_info$data_output)
    } else {
      temp_path <- temp_path %>%
        dplyr::pull(Path)
      
      temp_path <- gsub(fs::path(run_info$path), "", temp_path)
    }

    temp_file_tbl <- read_file(run_info, 
                               path = temp_path, 
                               return_type = 'df')
    
    temp_final_tbl <- tibble::tibble(Recipe = recipe, 
                                     Data = list(temp_file_tbl))
    
    recipe_tbl <- rbind(recipe_tbl, temp_final_tbl)
  }
  
  return(recipe_tbl)
}

get_forecast_data <- function(run_info, 
                              return_type = "df") {
  
  model_train_test_tbl <- read_file(run_info, 
                                    path = paste0('/model_utility/', hash_data(run_info$experiment_name), '-', hash_data(run_info$run_name), 
                                                  '-train_test_split.', run_info$data_output), 
                                    return_type = 'df') %>%
    dplyr::select(Run_Type, Train_Test_ID) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID))
  
  fcst_path <- paste0("/forecasts/*", hash_data(run_info$experiment_name), '-', 
                      hash_data(run_info$run_name), "*", '.', run_info$data_output)
  
  forecast_tbl <- read_file(run_info, 
                             path = fcst_path, 
                             return_type = return_type) %>%
    dplyr::mutate(Train_Test_ID = as.numeric(Train_Test_ID)) %>%
    dplyr::left_join(model_train_test_tbl, 
                     by = "Train_Test_ID") %>%
    dplyr::relocate(Run_Type, .before = Train_Test_ID) %>%
    dplyr::select(-Combo_ID, -Hyperparameter_ID) %>%
    dplyr::relocate(Combo)
  
  return(forecast_tbl)
}
