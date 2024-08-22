#' Choose top n best results and make them to be the starting value for the next round.
#'
#' @param el Estimated values from `pfiler`.
#' @param top_n_fits Number of best fits in terms of log-likelihood one wants to keep.
#' @param ncores Number of starting values possibly equal to the number of cores being used.
#'
#' @return A matrix with ncores number of rows and estimated parameters being in each row.
#' @export
#'
#' @source <https://github.com/AJAbkemeier/measlespkg/tree/main/R>
#'
#' @examples
#' choose_n_fits(el, 12, 36)
#' 

choose_n_fits = function(el, top_n_fits, ncores){
  fit_mat <- el
  score_total = fit_mat$logLik
  ranking_total = order(score_total, decreasing = TRUE)[1:top_n_fits]
  
  best_fits = dplyr::select(
    fit_mat[ranking_total,], -"logLik", -"se"
  )
  
  recycle_vec = sort(rep_len(1:top_n_fits, ncores))
  full_best_fit <- best_fits[recycle_vec, ] 
  
  starting_values <- vector(ncores, mode="list")
  
  for(i in 1:ncores){
    full_best_fit[i, ] -> starting_values[[i]] 
  }
  
  return(starting_values)
}