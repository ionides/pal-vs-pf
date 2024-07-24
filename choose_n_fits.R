# Choose top n fits

choose_n_fits = function(el, top_n_fits, ncores){
  ### Next-round code
  fit_mat <- el
  score_total = fit_mat$logLik
  ranking_total = order(score_total, decreasing = TRUE)[1:top_n_fits]
  
  best_fits = dplyr::select(
    fit_mat[ranking_total,], -"logLik", -"se"
  )
  
  recycle_vec = sort(rep_len(1:top_n_fits, ncores))
  full_best_fit <- best_fits[recycle_vec, ] 
  
  coef_names <- colnames(full_best_fit)
  colnames(full_best_fit) <- gsub(".{7}$","",coef_names)
  
  starting_values <- vector(ncores, mode="list")
  
  for(i in 1:ncores){
    t(full_best_fit[i, ])  |> 
      as.matrix() |>
      `colnames<-`("unit1") -> starting_values[[i]] 
  }
  
  return(starting_values)
}