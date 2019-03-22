# FUNCTION spr_offrate_mono_glob -----------------------------------------------

# This function is used to perform monophasic dissociation fitting for larger
# datasets coming from different experiments and containing a variable number
# of dissociation phases.

# Individual phases are fit

spr_offrate_mono_glob_os <- function(input,
                                  xvar,
                                  yvar,
                                  groupvar1,
                                  groupvar2
) {
  
  require(dplyr)
  require(ggplot2)
  require(tidyr)

  xvar      <- enquo(xvar)
  yvar      <- enquo(yvar)
  groupvar1 <- enquo(groupvar1)
  groupvar2 <- enquo(groupvar2)

  input <- as_tibble(input) %>% 
    ungroup() %>% 
    rename(xvar = UQ(xvar),
           yvar = UQ(yvar),
           groupvar1 = UQ(groupvar1),
           groupvar2 = UQ(groupvar2)
    ) %>%
    unite(unique, groupvar1, groupvar2, remove = F) %>% 
    group_by(unique) %>%
    mutate(xvar.0 = xvar - first(xvar)) %>% 
    mutate(norm = ((yvar-min(yvar))/(max(yvar)-min(yvar))))
    
  snr <- input %>%
  summarize(snr = sd(yvar[(length(yvar)-10):length(yvar)])/first(yvar)*100) %>%
  dplyr::filter(snr < 30 & snr > -5)
  
  # Filter out dissociation phases that are too noisy, cut dissociation dataset to the same lengths
  input <- input %>%
  dplyr::filter(unique %in% snr$unique) %>%
  droplevels()
  
  # filter dissociation phases at median length
  phaselength <- input %>%
    summarize(length = max(xvar.0))
  medianlength <- median(phaselength$length)
  
  input <- input %>% 
    dplyr::filter(xvar.0 <= medianlength)
  
  linfit <- coef(lm(norm ~ xvar.0, data = input))
  expfit <- nls(formula = norm ~ 1 * exp(xvar.0 * kd) + d,
                     data = input,
                     start = list(kd = linfit[[2]], d = 0.002)
  )
  
  return(c(coef(expfit)[[1]],
           coef(expfit)[[2]]
         )
  )

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
