# Function to calculate the profit-debt distribution index
distIndex <- function(x, y, w, returnData = F){
  s    <- order(w, x)
  sx   <- x[s]
  sy   <- y[s]
  px   <- sx / sum(sx)
  py   <- sy / sum(sy)
  cpx  <- cumsum(px)
  cpy  <- cumsum(py)
  index<- 0
  for (i in 2:length(cpx)){
    index <- index - (cpy[i] * cpx[i - 1] - cpx[i] * cpy[i - 1])
  }
    if (returnData == F){
      return(index)
    } else {
      df <- data.frame(sx = sx, sy = sy, px = px, py = py, cpx = cpx, cpy = cpy)
      return(list(index = index, data = df))
    }
}