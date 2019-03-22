# FUNCTION spr_offrate_mono ----------------------------------------------------

# This function is used to perform monophasic dissociation fitting for larger
# datasets coming from different experiments and containing a variable number
# of dissociation phases.

# Individual phases are fit

offrate_mono_sing <- function(input,
                              xvar,
                              yvar,
                              groupvar
) {
  
  require(tidyverse)

  # Enquoting for future use in dplyr pipes
  xvar      <- enquo(xvar)
  yvar      <- enquo(yvar)
  groupvar  <- enquo(groupvar)

  # Calculation of splitting variable as well as zeroed time per injection
  input.mod <- as_tibble(input) %>% 
    group_by(!! groupvar) %>%  
    mutate(xvar.zero = !! xvar - first(!! xvar)) %>% 
    mutate(yvar.norm = ((!! yvar - min(!! yvar)) / (max(!! yvar) - min(!! yvar))))
  
  snr <- input.mod %>%
  summarize(snr = sd(!! yvar[(length(!! yvar)-10):length(!! yvar)])/first(!! yvar)*100) %>%
  dplyr::filter(snr < 30 & snr > -5)
  
  # Filter out dissociation phases that are too noisy, cut dissociation dataset to the same lengths
  input.mod <- input.mod %>%
  dplyr::filter(groupvar %in% snr$UQ(groupvar)) %>%
  droplevels()
    
  linfit <- input.mod %>% 
    do(mod = lm(yvar ~ xvar.0, data = .)) %>%
    mutate(k.lin = coef(mod)[[2]], d.lin = coef(mod)[[1]]) %>%
    select(-mod)

  input.mod <- full_join(input.mod, linfit, by = "unique")

  expfit <- input.mod %>%
    do(mod = try(nls(formula = norm ~ 1 * exp(xvar.0 * kd) + d,
                     data = .,
                     start = list(kd = tempunique(.$k.lin),
                                  d  = tempunique(.$d.lin))
                )
       )
  ) %>%
   mutate(k.exp = coef(mod)[[1]],
          d.exp = coef(mod)[[2]]) %>%
   select(-mod)

   input.mod <- full_join(input.mod, expfit, by = "tempunique")
  
   input.mod <- input.mod %>%
    rename(!!xvar := xvar,
           !!yvar := yvar,
           !!groupvar1 := groupvar1,
           !!groupvar2 := groupvar2
    ) %>% 
    select(-tempunique)
   
   input <- input.mod %>% 
     left_join(x = input, y = .)
   
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
