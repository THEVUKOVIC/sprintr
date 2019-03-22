# FUNCTION spr_offrate_mono_glob -----------------------------------------------

# This function is used to perform monophasic dissociation fitting for larger
# datasets coming from different experiments and containing a variable number
# of dissociation phases.

# Individual phases are fit

spr_offrate_mono_glob <- function(input,
                                  xvar,
                                  yvar,
                                  groupvar,
                                  analytevar,
                                  phasevar
) {
  
  require(dplyr)
  require(ggplot2)
  require(tidyr)


  input <- as_tibble(input) %>% 
    unite(unique, !! analytevar, !! phasevar, remove = F) %>% 
    group_by(unique) %>%
    mutate(xvar.0 = !! xvar - first(!! xvar)) %>% 
    mutate(norm = ((!! yvar-min(!! yvar))/(max(!! yvar)-min(!! yvar))))
    
  # snr <- input %>%
  # summarize(snr = sd(yvar[(length(yvar)-10):length(yvar)])/first(yvar)*100) %>%
  # dplyr::filter(snr < 30 & snr > -5)
  # 
  # print(paste(snr$unique, " included in fit\n", sep = ""))
  # 
  # # Filter out dissociation phases that are too noisy, cut dissociation dataset to the same lengths
  # input <- input %>%
  # dplyr::filter(unique %in% snr$unique) %>%
  # droplevels()
  # 
  # # filter dissociation phases at median length
  # phaselength <- input %>%
  #   summarize(length = max(xvar.0))
  # medianlength <- median(phaselength$length)
  # 
  # input <- input %>% 
  #   dplyr::filter(xvar.0 <= medianlength)
  # 
  # linfit <- coef(lm(norm ~ xvar.0, data = input))[[2]]
  # expfit <- nls(formula = norm ~ 1 * exp(xvar.0 * kd),
  #                    data = input,
  #                    start = list(kd = linfit)
  # )
  # 
  # return(coef(expfit)[[1]])

}

# # Offrate analysis
# test <- spr_offrate_mono_glob(dplyr::filter(analyte1, onoff == 0),
#                                       time.inj.adj,
#                                       doubleref,
#                                       unique_name,
#                                       phaseid)
# ) %>% 
#   mutate(fit = first(yvar) * exp(xvar.0*k.exp))
# ggplot(test, aes(xvar.0, yvar)) + 
#   geom_line() + 
#   geom_line(aes(y = fit), color = "darkgreen") +
#   facet_grid(groupvar2 ~ groupvar1, scales = "free_y")
