# FUNCTION spr_offrate_mono ----------------------------------------------------

# This function is used to perform monophasic dissociation fitting for larger
# datasets coming from different experiments and containing a variable number
# of dissociation phases.

# Individual phases are fit

spr_offrate_mono_sing <- function(input,
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
    
  linfit <- input %>% 
    do(mod = lm(yvar ~ xvar.0, data = .)) %>%
    mutate(k.lin = coef(mod)[[2]]) %>%
    select(-mod)

  input <- full_join(input, linfit, by = "unique")

  expfit <- input %>%
    do(mod = try(nls(formula = norm ~ 1 * exp(xvar.0 * kd),
                     data = .,
                     start = list(kd = unique(.$k.lin))
                )
       )
  ) %>%
   mutate(k.exp = coef(mod)[[1]]) %>%
   select(-mod)

   input <- full_join(input, expfit, by = "unique")
  
   input <- input %>%
    rename(!!xvar := xvar,
           !!yvar := yvar,
           !!groupvar1 := groupvar1,
           !!groupvar2 := groupvar2
    )
  return(input)
}

# # Offrate analysis
# test <- spr_offrate_mono_sing(dplyr::filter(analyte1, onoff == 0),
#                                       time.inj.adj,
#                                       doubleref,
#                                       unique_name,
#                                       phaseid
# ) %>% 
#   mutate(fit = first(yvar) * exp(xvar.0*k.exp))
# ggplot(test, aes(xvar.0, yvar)) + 
#   geom_line() + 
#   geom_line(aes(y = fit), color = "darkgreen") +
#   facet_grid(groupvar2 ~ groupvar1, scales = "free_y")
