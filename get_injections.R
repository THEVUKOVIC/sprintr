################################################################################
# FUNCTION get_injections
################################################################################

# This function selects injections and zeroes time, injection id and signal

get_injections <- function(input = NULL,
                           output_path = NULL,
                           inj.sele,
                           analyte.names = NULL
){
  
  require(tidyverse)
  
  if (length(input) == 0) {
    
    input <- file.choose()
    
    input <- read.csv(stringsAsFactors = F,
                      file = input
    )
    
  }
  
  # Injection number selection block -------------------------------------------
  vertical.list <- list()
  
  if (typeof(inj.sele) == "integer") {
      
      inj.sele <- enquo(inj.sele)
  
      vertical.list[[1]] <- input %>% 
        dplyr::filter(inj.id %in% UQ(inj.sele)) %>% 
        droplevels()
  
  } else if (typeof(inj.sele) == "list") {
      
      for (x in seq_along(inj.sele)) {
        
        sele <- inj.sele[[x]]
        
        vertical.list[[x]] <- input %>%
          dplyr::filter(inj.id %in% sele) %>%
          droplevels()
        
      }
    
  }
  
  # Analyte renaming block
  if (length(analyte.names) != 0) {
    
    if(length(analyte.names) != length(inj.sele)) {
      stop("Number of selected blocks does not equal number of provided names.")
    }
    
    for (x in seq_along(vertical.list)) {
      
      renamingset <- analyte.names[[x]]
      
      names(renamingset) <- c(1:6)
      
      vertical.list[[x]]<- vertical.list[[x]] %>% 
      mutate(ana.name = recode(ana.id, !!! renamingset))
      
    }
  }
  
  # Data adjustment block ------------------------------------------------------
  for (x in seq_along(vertical.list)) {
    vertical.list[[x]] <- vertical.list[[x]] %>% 
      mutate(exp.id = paste(exp.id, "_", x, sep = "")) %>% 
      mutate(inj.id = inj.id - (first(inj.id)-1)) %>%
      unite(unique, lig.id, ana.id, remove = F) %>% 
      group_by(unique) %>% 
      mutate(coated.zero = coated - median(coated[c(1:7)])) %>%
      mutate(uncoated.zero = uncoated - median(uncoated[c(1:7)])) %>%
      mutate(time.zero = time - first(time)) %>% 
      ungroup() %>% 
      select(-unique)
  }
  
  # Creation of output file if it was specified --------------------------------
  if (length(output_path) != 0) {
    
    for (x in seq_along(vertical.list)) {
      
      write.csv(vertical.list[[x]],
                paste(output_path,
                      "/",
                      unique(vertical.list[[x]]$exp.id),
                      "_get_injections.csv",
                      sep = ""
                )
      )
      
    }
    
  }
  
  return(vertical.list)

}