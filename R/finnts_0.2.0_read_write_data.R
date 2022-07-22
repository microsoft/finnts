
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
  } else {
    file_type <- run_info$object_output
  }
  
  # write to temp folder
  temp_path <- NULL
  
  if(inherits(run_info$storage_object, c("blob_container", "ms_drive")) || 
     (inherits(run_info$storage_object, 'NULL') & is.null(run_info$path))) {
    
    fs::dir_create(tempdir(), folder)
    
    temp_path <- paste0(fs::path(tempdir(), folder), '\\', hash_data(run_info$run_name), '-', hash_data(combo), suffix, '.', file_type)
    
    write_data_type(x, temp_path, file_type)
  }
  
  # write to final folder output
  if(inherits(run_info$storage_object, c("blob_container", "ms_drive", "NULL")) & !is.null(run_info$path)) {
    
    if(is.null(run_info$storage_object)) {
      fs::dir_create(run_info$path, folder)
    }
    
    final_path <- paste0(fs::path(run_info$path, folder), '/', hash_data(run_info$run_name), '-', hash_data(combo), suffix, '.', file_type)

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
  
  switch(final_dest, 
         'NULL' = write_data_type(x, final_path, type), 
         blob_container = AzureStor::storage_upload(storage_object, src = temp_path, dest = final_path), 
         ms_drive = storage_object$upload_file(src = temp_path, dest = final_path))
  
}

