# ################################################################################
# # FUNCTION inj_extr_proteon
# ################################################################################
# 
# # This function converts ProteON experiment files to tidy data, in preparation
# # for further analysis.
# 
# inj_extr_proteon <- function(input,
#                              output_path = NULL,
#                              inj.sele
# ){
#   
#   require(tidyverse)
#   
#   # # Input testing sequence
#   # if (length(concentrations) == 0) {
#   # } else if (length(concentrations) != length(injections)) {
#   #   stop("Number of concentrations does not equal number of injections.")
#   # }
#   
#   if (length(input_path) == 0) {
#     input_path <- file.choose()
#   }
#   
#   # Injection number selection block -------------------------------------------
#   vertical.list <- list()
#   
#   if (typeof(inj.sele) == "integer") {
#       
#       inj.sele <- enquo(inj.sele)
#   
#       vertical.list[[1]] <- vertical %>% 
#         filter(inj.id %in% UQ(inj.sele)) %>% 
#         mutate(inj.id = inj.id - (first(inj.id)-1)) %>%
#         droplevels()
#   
#   } else if (typeof(inj.sele) == "list") {
#       
#       for (x in seq_along(inj.sele)) {
#         
#         sele <- inj.sele[[x]]
#         
#         vertical.list[[x]] <- vertical %>%
#           filter(inj.id %in% sele) %>% 
#           mutate(inj.id = inj.id - (first(inj.id)-1)) %>% 
#           droplevels()
#       }
#   }
#   
#   # Print overview of raw data on screen ---------------------------------------
#   for (x in seq_along(vertical.list)) {
#     
#     toprint <- ggplot(unite(vertical.list[[x]], unique, lig.id, ana.id, remove = F),
#          aes(time,
#              runmed(coated-uncoated, 33),
#              group = unique
#          )
#     ) +
#       geom_line() +
#       geom_line(aes(y = runmed(uncoated, 33)), color = "darkred") +
#       facet_grid(rows = vars(paste("lig.id = ", lig.id, sep = "")),
#                  cols = vars(paste("ana.id = ", ana.id, sep = "")),
#                  scales = "free_y"
#       ) +
#       labs(title = paste("Imported raw data (outliers removed) -",
#                          x,
#                          sep = ""
#            ),
#            subtitle = "black = coated, red = uncoated",
#            x = "Time [s]",
#            y = "Coated signal [RU]")
#                
#     print(toprint)
#   }
#   
#   # # Creation of output file if it was specified --------------------------------
#   # if (length(output_path) != 0) {
#   #   write.csv(vertical, paste(output_path,
#   #                             "/",
#   #                             exp.id,
#   #                             ".csv",
#   #                             sep = ""
#   #                       )
#   #   )
#   # }
#   
#   return(vertical.list)
# }
# 
# # Script testing area ----------------------------------------------------------
# input_path <- paste("U:/Projects/01_E3_modulation/SPR_Anna/",
#               "01_raw_data_and_analysis/01_proteon/2019-02-13/01_raw/",
#               "02_run1/",
#               "2019-02-14_ALS_kinetic_titration_incl_coat_L6.POMexp",
#                sep = ""
# )
# 
# analyte.names <- c("PBS",
#                   "PBS",
#                   "01_1932_H7",
#                   "02_1937_B1",
#                   "03_1951_F9",
#                   "04_1952_A4"
# )
# 
# ligand.names <- c("uncoated",
#                  "uncoated",
#                  "uncoated",
#                  "uncoated",
#                  "GST",
#                  "GST"
# )
# 
# exp.id <- "2019-02-13_1"
# 
# inj.sele <- list(c(4:12), c(1:3))
# 
# test <-import_proteon(inj.sele = inj.sele)
