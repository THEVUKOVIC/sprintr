# FUNCTION phase_detection -----------------------------------------------------
spr_onphasedetect <- function(input,
                              filter.window = 33,
                              cutoff.on = 0.5
) {
  
  # These should one day be replaced by my own filter functions
  # needed for running median
  require(stats)
  # needed for savitzky-golay filter
  require(signal)
  
  # This should be expanded to a thorough check for supplied data
  input <- as.numeric(input)

  # Apply filters
  input.med <- runmed(input, filter.window)
  
  input.med.sgolderiv1 <- sgolayfilt(input.med,
                                     p = 3,
                                     n = filter.window,
                                     m = 1,
                                     ts = 1
  )
  
  input.med.sgolderiv2 <- sgolayfilt(input.med,
                                     p = 3,
                                     n = filter.window,
                                     m = 2,
                                     ts = 1
  )
  
  # Find regions with certain signal steepness over time
  filt.input.med.sgolderiv1 <- which(input.med.sgolderiv1 > cutoff.on)
  
  # Find regions where steepness is at maximum, i.e. second deriv close to zero
  filt.input.med.sgolderiv2 <- which(input.med.sgolderiv2 <0.1 &
                                     input.med.sgolderiv2 > 0
  )
  
  # Find values satisfying both conditions above
  filter.both <- intersect(filt.input.med.sgolderiv1, filt.input.med.sgolderiv2)
  
  # We calculate the differences of indices in order to exclude points too
  # close to each other, always the first found will be returned
  filter.difference <- cbind(filter.both, c(filter.both[-1], NA))
  filter.difference <- cbind(filter.difference, filter.difference[,1]-filter.difference[,2])

  toremove <- which(filter.difference[ , 3] > -10) + 1
  startpoints <- filter.both[-toremove]

  # if value is not maximum within +/- 20 datapoints, set to maximum of the region
  # Discard duplicates at the end
  
  # We give some minor output in order to allow the user to adjust parameter
  par(mfrow = c(2, 1))
  plot(seq_along(input),
       input,
       type = "l",
       xlab = "Value ID",
       ylab = "Input value"
  )
  title("Raw data with detected points")
  lines(seq_along(input.med), input.med, col = "darkred")
  for(x in seq_along(filter.both)) {abline(v = startpoints[x], lty = 3L, col = "darkgreen")}
  
  plot(seq_along(input.med.sgolderiv1),
       input.med.sgolderiv1,
       type = "l",
       xlab = "Value ID",
       ylab = "Input Derivative"
  )
  title("First derivative and cutoff")
  lines(seq_along(input.med.sgolderiv1), input.med.sgolderiv1, col = "darkred")
  for(x in seq_along(filter.both)) {abline(h = cutoff.on, lty = 3L)}
  for(x in seq_along(filter.both)) {abline(v = startpoints[x], lty = 3L, col = "darkgreen")}
  
  return(startpoints)
  
}
