################################################################################
# FUNCTION input_path
################################################################################

# This function converts ProteON experiment files to tidy data, in preparation
# for further analysis.

import_proteon <- function(input_path = NULL,
                           output_path = NULL,
                           ligand.names   = as.character(c(1:6)),
                           analyte.names  = as.character(c(1:6)),
                           exp.id         = "unknown"
){
  
  require(tidyverse)
  
  if (length(input_path) == 0) {
    input_path <- file.choose()
  }
  
  # Wrong length of ligand analyte and exp.id vectors
  if (length(ligand.names) != 6) {
    stop("Number of ligand names not six.")
  }
    
  if (length(analyte.names) != 6) {
    stop("Number of analyte names not six.")
  }
    
  if (length(exp.id) != 1) {
    stop("Number of experiment identifier not 1.")
  }
  
  # ZIP raw data extraction block ----------------------------------------------                             
                             
  # Extract all filenames contained in ZIP file
  zip_list <- unzip(zipfile = input_path,
                    list    = TRUE,
                    unzip   = "internal"
  )[[1]]

  # Unzip entire file
  unzip(zipfile = input_path,
        list    = FALSE,
        unzip   = "internal"
  )

  # We only want the run raw data
  filelist_pre <- grep(pattern = "^data",
                       x = zip_list,
                       value = TRUE
  )
  
  # Start an empty list that will be filled with raw data from each file
  filelist <- list()
  
  # Start counter to be used for reading in raw files
  file.id <- 0
  
  # Loop over the number of injections
  for(inj.id in c(1:(length(filelist_pre)/36))){
    # Loop over the number of ligand channels
    for (ana.id in c(1:6)){
      # Loop over the number of analytes
      for (lig.id in c(1:6)){
        # Read in raw data
        file.id <- file.id + 1
        filelist[[file.id]] <- as_tibble(read.table(header = FALSE,
                                                    sep = ",",
                                                    quote = "",
                                                    stringsAsFactors = F,
                                                    skip = 7,
                                                    fill = T,
                                                    file = filelist_pre[file.id]
                                         )
        )
        # Skip some XML tags & select first three cols (time, spot, interspot)
        file.length <- length(filelist[[file.id]][[1]])
        filelist[[file.id]] <- filelist[[file.id]][c(1:(file.length-2)), c(1:3)]
        
        # Wrongly assigned text type from aforementioned tags
        filelist[[file.id]][[1]] <- as.numeric(filelist[[file.id]][[1]])
        
        filelist[[file.id]] <- cbind(filelist[[file.id]],
                                     rep(lig.id,
                                         length(filelist[[file.id]][[1]])
                                     )
        )
        
        filelist[[file.id]] <- cbind(filelist[[file.id]],
                                     rep(ana.id,
                                         length(filelist[[file.id]][[1]])
                                     )
        )
        
        filelist[[file.id]] <- cbind(filelist[[file.id]],
                                     rep(inj.id,
                                         length(filelist[[file.id]][[1]])
                                     )
        )
        
        names(filelist[[file.id]]) <- c("time",
                                        "coated",
                                        "uncoated",
                                        "lig.id",
                                        "ana.id",
                                        "inj.id"
        )
      }
    }
  }

  # Remove now unnecessary files
  file.remove(list = zip_list)
  
  # Fusion to tidy dataset
  vertical <- do.call(rbind, filelist)

  # Naming of ligand, analyte and experiment block -----------------------------

  # Naming of ligand and analyte
  names(ligand.names) <- c(1:6)
  names(analyte.names) <- c(1:6)

  vertical <- vertical %>% 
  mutate(lig.name = recode(lig.id, !!! ligand.names)) %>% 
  mutate(ana.name = recode(ana.id, !!! analyte.names)) %>%
  add_column(exp.id = rep(exp.id, nrow(.)),
             .after = 3)
  
  # Creation of output file if it was specified --------------------------------
  if (length(output_path) != 0) {
    write.csv(vertical, paste(output_path,
                              "/",
                              exp.id,
                              ".csv",
                              sep = ""
                        )
    )
  }
  
  return(vertical)
}
