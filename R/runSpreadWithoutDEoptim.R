runSpreadWithoutDEoptim <- function(sim) {
  minIndex <- if (!exists("vals1", envir = .GlobalEnv)) {

    vals1 <- list()
    0
  } else {
    length(vals1)
  }
  for (i in 1:10) {
    seed <- sample(1e6, 1)
    set.seed(seed)
    pars <- lapply(1:96, function(x) runif(length(P(sim)$lower), P(sim)$lower, P(sim)$upper))
    # print(pars)
    #lapply is easier to debug but mcmapply faster
    # st1 <- system.time(
    #   a <- lapply(pars,
    #               FUN = .objfun,
    #               FS_formula = sim$fireSense_formula, #loci = loci,
    #               landscape = sim$flammableRTM,
    #               annualDTx1000 = lapply(sim$fireSense_annualSpreadFitCovariates, setDF),
    #               nonAnnualDTx1000 = lapply(sim$fireSense_nonAnnualSpreadFitCovariates, setDF),
    #               fireBufferedListDT = lapply(sim$fireBufferedListDT, setDF),
    #               historicalFires = lapply(sim$lociList, setDF),
    #               tests = c("SNLL_FS"),
    #               #tests = c("SNLL_FS"),
    #               covMinMax = sim$covMinMax,
    #               Nreps = P(sim)$objfunFireReps,
    #               maxFireSpread = P(sim)$maxFireSpread,
    #               verbose = TRUE
    #   )
    # )
    st1 <- system.time(
      a <- mcmapply(mc.cores = min(8, length(pars)), par = pars, FUN = .objfunSpreadFit,
                    mc.preschedule = FALSE,
                    MoreArgs = list(
                      FS_formula = sim$fireSense_formula, #loci = loci,
                      landscape = sim$flammableRTM,
                      annualDTx1000 = lapply(sim$fireSense_annualSpreadFitCovariates, setDF),
                      nonAnnualDTx1000 = lapply(sim$fireSense_nonAnnualSpreadFitCovariates, setDF),
                      fireBufferedListDT = lapply(sim$fireBufferedListDT, setDF),
                      historicalFires = lapply(sim$lociList, setDF),
                      tests = c("SNLL_FS"),
                      #tests = c("SNLL_FS"),
                      covMinMax = sim$covMinMax,
                      Nreps = P(sim)$objfunFireReps,
                      maxFireSpread = P(sim)$maxFireSpread,
                      verbose = TRUE)
      )
    )
    vals1 <- append(vals1, purrr::map2(pars, a, function(.x, .y) list(pars = .x, objfun = .y)) )

  }
}
