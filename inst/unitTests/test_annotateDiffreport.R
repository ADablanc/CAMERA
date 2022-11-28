

library(faahKO)

test_data.xs.fill <- fillPeaks(group(faahko))


test.annotateDiffreport.calcIso <- function() {

  calcIsoFALSE <- annotateDiffreport(test_data.xs.fill, calcIso=FALSE, calcCiS=TRUE, calcCaS=FALSE, BPPARAM=SerialParam())
  calcIsoTRUE  <- annotateDiffreport(test_data.xs.fill, calcIso=TRUE,  calcCiS=TRUE, calcCaS=FALSE, BPPARAM=SerialParam())

  checkTrue(
    !identical(calcIsoTRUE[,"pcgroup"], calcIsoFALSE[,"pcgroup"]),
    "The iso was not successfully computed with calcIso=TRUE flag."
  )
}

test.annotateDiffreport.pval <- function() {

  checkException(annotateDiffreport(test_data.xs.fill, pval=-0.5, BPPARAM=SerialParam()))
  checkException(annotateDiffreport(test_data.xs.fill, pval=1.5, BPPARAM=SerialParam()))

  ## test that pval = 0 works
  checkTrue(
    nrow(annotateDiffreport(test_data.xs.fill, pval=0, BPPARAM=SerialParam())) != 0,
    "annotateDiffreport with pval=0 failed."
  )
  ## test that pval = 1 works
  checkTrue(
    nrow(annotateDiffreport(test_data.xs.fill, pval=1, BPPARAM=SerialParam())) != 0,
    "annotateDiffreport with pval=1 failed."
  )

  lower <- 0
  upper <- 0.6
  middle <- 0.1

  lower_result <- annotateDiffreport(test_data.xs.fill, pval=lower, BPPARAM=SerialParam())
  middle_result <- annotateDiffreport(test_data.xs.fill, pval=middle, BPPARAM=SerialParam())
  upper_result  <- annotateDiffreport(test_data.xs.fill, pval=upper, BPPARAM=SerialParam())

  checkTrue(
    !identical(upper_result[,"pcgroup"], lower_result[,"pcgroup"]),
    paste0("Got the same results with pval = ", upper, " and pval = ", lower, " .")
  )

  checkTrue(
    !identical(middle_result, upper_result),
    paste0("Got the same results with pval = ", middle, " and pval = ", upper, " .")
  )

  checkTrue(
    !identical(middle_result[,"pcgroup"], lower_result[,"pcgroup"]),
    paste0("Got the same results with pval = ", middle, " and pval = ", lower, " .")
  )

}

test.annotateDiffreport.graphMethod <- function() {

  graphMethod_hcs <- annotateDiffreport(test_data.xs.fill, graphMethod="hcs", BPPARAM=SerialParam())
  graphMethod_lpc  <- annotateDiffreport(test_data.xs.fill, graphMethod="lpc", BPPARAM=SerialParam())

  checkTrue(
    !identical(graphMethod_lpc[,"pcgroup"], graphMethod_hcs[,"pcgroup"]),
    "Got the same results with graphMethod = hcp and graphMethod = lcp."
  )

}

test.annotateDiffreport.calcCiS <- function() {

  checkException(annotateDiffreport(test_data.xs.fill, calcCiS=FALSE, BPPARAM=SerialParam()))

  calcCiSFALSE <- annotateDiffreport(test_data.xs.fill, calcCiS=FALSE, calcCaS=TRUE, BPPARAM=SerialParam())
  calcCiSTRUE  <- annotateDiffreport(test_data.xs.fill, calcCiS=TRUE, BPPARAM=SerialParam())

  checkTrue(
    !identical(calcCiSTRUE[,"pcgroup"], calcCiSFALSE[,"pcgroup"]),
    "Got the same results with calcCis = true and calcCis = false."
  )

}

test.annotateDiffreport.calcCaS <- function() {

  checkException(annotateDiffreport(test_data.xs.fill, calcCaS=FALSE, calcCis=FALSE, BPPARAM=SerialParam()))

  calcCaSFALSE <- annotateDiffreport(test_data.xs.fill, calcCaS=FALSE, calcCiS=TRUE, BPPARAM=SerialParam())
  calcCaSTRUE  <- annotateDiffreport(test_data.xs.fill, calcCaS=TRUE, BPPARAM=SerialParam())

  checkTrue(
    !identical(calcCaSTRUE[,"pcgroup"], calcCaSFALSE[,"pcgroup"]),
    "Got the same results with calcCas = true and calcCas = false"
  )

}

test.annotateDiffreport.intval <- function() {

  intval_into <- annotateDiffreport(test_data.xs.fill, intval="into", calcCaS=TRUE, BPPARAM=SerialParam())
  intval_maxo  <- annotateDiffreport(test_data.xs.fill, intval="maxo", calcCaS=TRUE, BPPARAM=SerialParam())

  checkTrue(
    !identical(intval_into, intval_maxo),
    "Got the same results with intVal = into and intVal = maxo."
  )

}
