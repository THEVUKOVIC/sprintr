################################################################################
# FUNCTION input_path
################################################################################

# This function converts ProteON experiment files to tidy data, in preparation
# for further analysis.

import_proteon <- function(input_path) {
  
  # Extract all filenames contained in ZIP file
  zip_list <- unzip(zipfile = input_path,
                    list = TRUE,
                    unzip = "internal"
  )[[1]]

  # Unzip entire file
  unzip(zipfile = input_path,
        list = FALSE,
        unzip = "internal"
  )

  # We only want the run raw data
  filelist_pre <- grep(pattern = "^data",
                       x = zip_list,
                       value = TRUE
  )
  
  # Start an empty list that will be filled with raw data from each file
  filelist <- list()
  
  # Start counter to be used for reading in raw files
  c <- 0
  
  # Loop over the number of injections
  for(y in c(1:(length(filelist_pre)/36))){
    # Loop over the number of channels
    for (z in c(1:6)){
      # Loop over the number of analytes
      for (x in c(1:6)){
        # Read in raw data
        c <- c + 1
        filelist[[c]] <- as.data.frame(read.csv(stringsAsFactors=FALSE,
                                                file = filelist_pre[c],
                                                header = FALSE,
                                                skip = 7,
                                                sep = ","
                                       )
        )
        # Skip some XML tags)
        filelist[[c]] <- filelist[[c]][c(1:(length(filelist[[c]][[1]])-2)),c(1:3)]
        
        # Wrongly assigned text type from aforementioned tags
        filelist[[c]][[1]] <- as.numeric(filelist[[c]][[1]])
        
        # Add injection number column
        filelist[[c]] <- cbind(filelist[[c]],
                               rep(y, length(filelist[[c]][[1]])))
        
        # Add ligand number column
        filelist[[c]] <- cbind(filelist[[c]],
                               rep(x, length(filelist[[c]][[1]])))
        
        # Add analyte number column
        filelist[[c]] <- cbind(filelist[[c]],
                               rep(z, length(filelist[[c]][[1]])))
        
        # Add column names so that we can refer to them by name and not ID number
        names(filelist[[c]]) <- c("time",
                                  "coated",
                                  "uncoated",
                                  "injection.id",
                                  "ligand.id",
                                  "analyte.id"
        )
      }
    }
  }

  # Remove now unnecessary files
  file.remove(list = zip_list)
  
  # Fusion to tidy dataset and calculation of interspot-corrected values
  filelist.vertical <- do.call(rbind, filelist)
  
  # Create additional column for each ligand-analyte combination
  filelist.vertical <- cbind(filelist.vertical,
                             paste("A",
                                   filelist.vertical$analyte.id,
                                   "_L",
                                   filelist.vertical$ligand.id,
                                   sep = ""
                             )
  )
  namenumber <- length(names(filelist.vertical))
  names(filelist.vertical)[namenumber] <- "unique"
  
  return(filelist.vertical)
}