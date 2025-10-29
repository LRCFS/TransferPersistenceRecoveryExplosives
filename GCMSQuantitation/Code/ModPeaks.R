ModPeaks <- function(x, y = NULL, minPH = NULL, minPW = 0, thr = NULL, stepF = 0.49) {
  xy <- getXY(x, y)
  xx <- xy$x
  yy <- xy$y
  len.yy <- length(yy)
  
  min.yy <- min(yy, na.rm = TRUE)
  max.yy <- max(yy, na.rm = TRUE)
  yy <- c(min.yy, yy)
  xx <- c(xx[1], xx)
  
  if (is.null(thr)) thr <- min.yy
  if (is.null(minPH)) minPH <- (max.yy - min.yy) / 10
  if (stepF >= 0.5) stop("'stepF' must be smaller than 0.5")
  
  peak.x <- peak.y <- peak.w <- numeric(0)
  lev <- thr - stepF * minPH
  
  repeat {
    lev <- lev + stepF * minPH
    if (lev >= max.yy) break
    
    hi <- yy > lev
    start <- which(diff(c(FALSE, hi)) > 0)
    end <- which(diff(c(hi, FALSE)) < 0)
    
    for (ii in seq_along(start)) {
      seg_x <- xx[start[ii]:end[ii]]
      seg_y <- yy[start[ii]:end[ii]]
      i <- which.max(seg_y)
      x_peak <- seg_x[i]
      
      if (x_peak %in% peak.x) next
      
      miny <- min(yy[max(1, start[ii] - 1):min(end[ii] + 1, len.yy)], na.rm = TRUE)
      PH <- seg_y[i] - miny
      half_height <- miny + PH / 2
      PW_indices <- which(seg_y > half_height)
      
      if (length(PW_indices) > 1) {
        PW <- abs(seg_x[max(PW_indices)] - seg_x[min(PW_indices)])
      } else {
        PW <- 0
      }
      
      if (PH >= minPH && PW >= minPW) {
        peak.x <- c(peak.x, x_peak)
        peak.y <- c(peak.y, seg_y[i])
        peak.w <- c(peak.w, PW)
      }
    }
  }
  
  if (length(peak.x) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), w = numeric(0)))
  }
  
  res <- data.frame(x = peak.x, y = peak.y, w = peak.w)
  res <- res[order(res$x), ]
  return(res)
}
