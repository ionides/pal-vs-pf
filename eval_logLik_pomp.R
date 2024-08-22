#' To evaluate likelhoods of a list of pomp objects using pfilter with replications. 
#'
#' @param model_obj_list List of pomp results.
#' @param nreps Number of replications for each pfilter. 
#' @param np_pf Number of particles in pfilter.
#' @param seed  Set seed.
#' 
#' @return A matrix with ncores number of rows and estimated parameters being in each row.
#' @export
#'
#' @source <https://github.com/AJAbkemeier/measlespkg/tree/main/R>
#'
#' @examples
#' eval_logLik_pomp(model_obj_list, 36, 1000, 123)
#' 
eval_logLik_pomp = function(model_obj_list, nreps, np_pf, seed){
  pf_logLik_frame = data.frame(
    logLik = rep(0, length(model_obj_list)),
    se = rep(0, length(model_obj_list))
  ) %>% cbind(
    rbind(t(sapply(model_obj_list, pomp::coef)))
  )
  
  for(i in seq_along(model_obj_list)){
    plan(multicore, workers = nreps)
    foreach::foreach(
      j = 1:nreps,
      .options.future=list(seed=seed),
      .combine = rbind
    ) %dofuture% {
      pfilter_obj = pomp::pfilter(model_obj_list[[i]], Np = np_pf)
      pomp::logLik(pfilter_obj)
    } -> pf_logLik_matrix
    
    pf_logLik_frame[i, 1:2] = pomp::logmeanexp(
      pf_logLik_matrix,
      se = TRUE
    )
  }
  
  return(pf_logLik_frame)
}