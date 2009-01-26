# this is a utility function that is called from contrast.lm
testStatistic <- function(fit, designMatrix, critVal, params=getCoefficients(fit), covMatrix=vcov(fit))
{
   est <- drop(designMatrix %*% params)
   v <- drop((designMatrix %*% covMatrix) %*% t(designMatrix))
   ndf <- if (is.matrix(v)) nrow(v) else 1
   se <- if (ndf == 1) sqrt(v) else sqrt(diag(v))
   testStat <- est / se

   # this is inconsistent theoretically, but consistent with each R model
   df <- switch(
      class(fit)[1],
      lm =, glm = fit$df.residual,
      gls = fit$dims$N -  fit$dims$p,
      lme = fit$dims$N -  length(params) - ncol(fit$apVar),      
      geese = NA)
   
   P <- switch(
      class(fit)[1],
      lm =, glm = 2 * (1 - pt(abs(testStat), df)),
      gls = 2 * (1 - pt(abs(testStat), df)),
      lme = 2 * (1 - pt(abs(testStat), df)),      
      geese = 2 * (1 - pnorm(abs(testStat))))

   list(Contrast=est,
        SE=se,
        Lower=est - critVal * se,
        Upper=est + critVal * se,
        testStat=testStat,
        df = df,
        Pvalue=P,
        var=v,
        X=designMatrix)
}
