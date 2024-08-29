

#' @title .addFisherTestToCodeCounts
#' @description performs an Fisher test in each row of the case_controls_counts table
#' @param tibbleWithCodeCounts table obtained from `getCodeCounts`.
#' @return inputed table with appened columsn :
#' - p : for the p-value
#' - OR; for the ods ratio
#' @export
#' @importFrom dplyr setdiff bind_cols select mutate if_else
#' @importFrom purrr pmap_df
.addTestToCodeCounts <- function(tibbleWithCodeCounts){

  missingCollumns <- dplyr::setdiff(c("nCasesYes","nCasesNo","nControlsYes","nControlsNo"), names(tibbleWithCodeCounts))
  if(length(missingCollumns)!=0){
    stop("case_controls_counts is missing the following columns: ", missingCollumns)
  }

  tibbleWithCodeCounts <- tibbleWithCodeCounts |>
    dplyr::bind_cols(
      tibbleWithCodeCounts |>
        dplyr::select(nCasesYes,nCasesNo,nControlsYes,nControlsNo) |>
        purrr::pmap_df( ~.binaryTest(..1,..2,..3,..4))
    )

  return(tibbleWithCodeCounts)

}

.binaryTest <- function(a,b,c,d, fiserLimit=10){
  data <-matrix(c(a,b,c,d),ncol=2)
  if(a<fiserLimit | b<fiserLimit | c<fiserLimit | d<fiserLimit){
    results <- stats::fisher.test(data)
    p.value <- results$p.value
    odds.ratio <- results$estimate
    test <- 'Fisher'
  }else{
    results <- chisq.test(data)
    p.value <- results$p.value
    odds.ratio <- (a * d)/(b * c)
    test <- 'Chi-square'
  }
  return(list(
    countsPValue = p.value,
    countsOddsRatio = odds.ratio,
    countsTest = test
  ))
}





.addTestTotibbleWithValueSummary <- function(tibbleWithValueSummary){

  checkmate::assertDataFrame(tibbleWithValueSummary)
  tibbleWithValueSummary |> names() |> checkmate::testSubset(c("nCasesYesWithValue", "meanValueCases", "sdValueCases", "nControlsYesWithValue", "meanValueControls", "sdValueControls"))

  tibbleWithValueSummary <- tibbleWithValueSummary |>
    dplyr::bind_cols(
      tibbleWithValueSummary |>
        dplyr::select(meanValueCases,meanValueControls,sdValueCases,sdValueControls,nCasesYesWithValue,nControlsYesWithValue) |>
        purrr::pmap_df( ~.continuousTest (..1,..2,..3,..4,..5,..6))
    )

  return(tibbleWithValueSummary)

}


# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0.
# equal.variance: whether or not to assume equal variance. Default is FALSE.
.continuousTest <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  # # special case
  # if(n1 <= 10 | n2 <= 10 ){
  #   return(list(
  #     continuousPValue = NA_real_,
  #     continuousStandardError = NA_real_,
  #     continuousTest = "no test, less than 10 samples"
  #   ))
  # }
  # Na input
  if(is.na(m1)||is.na(m2)||is.na(s1)||is.na(s2)){
    return(list(
      continuousPValue = NA_real_,
      continuousStandardError = NA_real_,
      continuousTest = ""
    ))
  }

  # special case
  if(m1==0 & m2==0 & s1==0 & s2==0 ){
    return(list(
      continuousPValue = 1,
      continuousStandardError = 0,
      continuousTest = "Welch Two Sample t-test"
    ))
  }

  # special case
  if(m1==0 & m2==0 & s1==0 & s2==0 ){
    return(list(
      continuousPValue = 1,
      continuousStandardError = 0,
      continuousTest = "Welch Two Sample t-test"
    ))
  }

  if( equal.variance==FALSE )
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
    df <- n1+n2-2
  }
  t <- (m1-m2-m0)/se
  #dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
  #names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(list(
    continuousPValue = 2*pt(-abs(t),df),
    continuousStandardError = se,
    continuousTest = ifelse(is.na(se), '',"Welch Two Sample t-test")
  ))
}























