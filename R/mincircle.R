#' Return radius of minimum encompassing circle
#'
#' @param n Number of points (unit circles) to encompass.
#'
#' @return Radius of minimum encompassing circle to cover `n` unit circles. Below `n`<=20, returns exact value, based on [theory](https://erich-friedman.github.io/packing/cirincir/). Above 20, returns sqrt(`n`*1.35) as an approximation.
#' @export
#'
#' @examples
mincircle <- Vectorize(
  function(n){
    if(n<21){
      out.r <- circle_pack[n,"min_r"]
    }else{
      out.r <- sqrt(n*1.35)
    }
    return(out.r)
  }
)
