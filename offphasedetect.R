# FUNCTION phase_detection -----------------------------------------------------
spr_offphasedetect <- function(input,
                               window = 33,
                               cutoff = -1
) {
  
  # These should one day be replaced by my own filter functions
  # needed for running median
  require(stats)
  # needed for savitzky-golay filter
  require(signal)
  
  # This should be expanded to a thorough check for supplied data
  if (is.numeric(input) == F) {
    stop("Provided input not numeric vector.")
  }

  # Apply filters
  input.med <- runmed(input, window)
  
  input.med.sgolderiv1 <- sgolayfilt(input.med,
                                     p = 3,
                                     n = window,
                                     m = 1,
                                     ts = 1
  )
  
  # Find regions with certain signal steepness over time
  filt.input.med.sgolderiv1 <- which(input.med.sgolderiv1 < cutoff)

  # We calculate the differences of indices in order to exclude points too
  # close to each other, always the first found will be returned
  filter.difference <- cbind(filt.input.med.sgolderiv1,
                             c(filt.input.med.sgolderiv1[-1], NA)
  )
  
  filter.difference <- cbind(filter.difference,
                             filter.difference[,1] - filter.difference[,2])

  toremove <- which(filter.difference[ , 3] > -40) + 1
  startpoints <- filter.difference[-toremove, 1]

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
  for(x in seq_along(startpoints)) {
    abline(v   = startpoints[x],
           lty = 3L,
           col = "darkgreen"
    )
  }
  
  plot(seq_along(input.med.sgolderiv1),
       input.med.sgolderiv1,
       type = "l",
       xlab = "Value ID",
       ylab = "Input Derivative"
  )
  title("First derivative and cutoff")
  lines(seq_along(input.med.sgolderiv1), input.med.sgolderiv1, col = "darkred")
  for(x in seq_along(startpoints)) {
    abline(h = cutoff, lty = 3L)
  }
  
  for(x in seq_along(startpoints)) {
    abline(v = startpoints[x], lty = 3L, col = "darkgreen")
  }
  
  return(startpoints)
  
}