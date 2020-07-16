plotDEOptimResults <- function(){
    aa <- showCache(userTags = "DEoptimForCache")
    bb <- aa[tagKey == "accessed"]
    setkey(bb, tagValue)
    dd <- lapply(tail(bb$cacheId, 4), function(xx) {
      aa <- loadFromCache(cacheId = xx, cachePath = Paths$cachePath)
      bests <- lapply(seq(aa$member$lower), 
                      function(x) MASS::fitdistr(aa$member$pop[,x], densfun = "normal"))
      fittedBest <- do.call(rbind, lapply(bests, function(x) x$estimate))
      dt <- as.data.table(fittedBest)
      dt[, param := names(aa$member$lower)]
      as.data.table(aa$member$pop)
    })
    names(dd) <- seq(dd)
    dd <- rbindlist(dd, idcol = "iteration")
    colnames(dd) <- gsub("V", "par", colnames(dd))
    dd <- melt(dd, measure.vars = patterns("par.*", cols = names(dd)), variable.name = "param")
    dd[, iteration := as.integer(iteration) * 150]
    # dd[, list(mean = mean(mean), meanSd = sd(mean), se = mean(sd), seSd = sd(sd)), by = "param"]
    
    (means <- ggplot(dd, aes(x = iteration, y = value)) +
        geom_point() +
        geom_smooth(method="lm", se=TRUE, fullrange=TRUE, level=0.95) +
        facet_wrap(~ param, scales = "free"))
    summary(lm(value ~ iteration, data = dd[param == "par1"]))
    
    ee <- dd[, list(vals = {
      a <- lm(value ~ iteration)
      b <- summary(a)
      coefficients(b)[2,]
    }), by = param]
    ee[, type := rep(c("mean", "se", "tvalue", "pvalue"), length.out = NROW(ee))]
    ee[type == "pvalue"]
    
    means <- ggplot(dd, aes(x = iteration, y = value)) +
      geom_point() +
      geom_smooth(method="lm", se=TRUE, fullrange=TRUE, level=0.95) +
      facet_wrap(~ param, scales = "free")
    
    ff <- dd[, list(value = sd(value)), by = c("iteration", "param")]
    (ses <- ggplot(ff, aes(x = iteration, y = value)) +
        geom_point() +
        geom_smooth(method="lm", se=TRUE, fullrange=TRUE, level=0.95) +
        facet_wrap(~ param, scales = "free"))
    ff1 <- ff[, list(vals = {
      a <- lm(value ~ iteration)
      b <- summary(a)
      coefficients(b)[2,]
    }), by = param]
    ff1[, type := rep(c("mean", "se", "tvalue", "pvalue"), length.out = NROW(ff1))]
    ff1[type == "pvalue"]
    
    aa <- loadFromCache(cacheId = tail(bb$cacheId,1), cachePath = Paths$cachePath)
    bests <- lapply(seq(aa$member$lower), 
                    function(x) MASS::fitdistr(aa$member$pop[,x], densfun = "normal"))
    fittedBest <- do.call(rbind, lapply(bests, function(x) x$estimate))
    rownames(fittedBest) <- names(aa$member$lower)
    browser() # TM [11JUN20]: Brought this from Eliot's code. Need to know his intentions with it
}