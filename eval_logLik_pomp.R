#pfilter for a list of mif2 results
eval_logLik_pomp = function(model_obj_list, nreps, np_pf, seed){
  pf_logLik_frame = data.frame(
    logLik = rep(0, length(model_obj_list)),
    se = rep(0, length(model_obj_list))
  ) %>% cbind(
    rbind(t(sapply(model_obj_list, pomp::coef)))
  )
  
  for(i in seq_along(model_obj_list)){
    registerDoFuture()
    plan(multicore, workers = ncores)
    
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