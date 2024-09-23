

#' @title .addFisherTestToCodeCounts
#' @description performs an Fisher test in each row of the case_controls_counts table
#' @param tibbleWithCodeCounts table obtained from `getCodeCounts`.
#' @return imputed table with append columns :
#' - p : for the p-value
#' - OR; for the odds ratio
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


#' @title .binaryTest
#' @description performs a Fisher or Chi-square test based on the input counts
#' @param a number of cases with the condition
#' @param b number of cases without the condition
#' @param c number of controls with the condition
#' @param d number of controls without the condition
#' @param fisherLimit threshold to decide between Fisher and Chi-square test
#' @return a list with:
#' - `countsPValue`: the p-value of the test
#' - `countsOddsRatio`: the odds ratio
#' - `countsTest`: the type of test performed
.binaryTest <- function(a,b,c,d, fisherLimit=10){
  if(is.na(a) | is.na(b) | is.na(c) | is.na(d)){
    return(list(
      countsPValue = NA_real_,
      countsOddsRatio = NA_real_,
      countsTest = "No test, not enough samples"
    ))
  }
  data <-matrix(c(a,b,c,d),ncol=2)
  if(a<fisherLimit | b<fisherLimit | c<fisherLimit | d<fisherLimit){
    results <- stats::fisher.test(data)
    p.value <- results$p.value |> as.numeric()
    odds.ratio <- results$estimate |> as.numeric()
    test <- 'Fisher'
  }else{
    results <- chisq.test(data)
    p.value <- results$p.value |> as.numeric()
    odds.ratio <- (a * d)/(b * c)
    test <- 'Chi-square'
  }
  return(list(
    countsPValue = p.value,
    countsOddsRatio = odds.ratio,
    countsTest = test
  ))
}

#' @title .addTestTotibbleWithValueSummary
#' @description performs a continuous test in each row of the tibbleWithValueSummary table
#' @param tibbleWithValueSummary table containing summary statistics for cases and controls.
#' @return input table with appended columns:
#' - continuousPValue: the p-value of the test
#' - continuousOddsRatio: the standard error of the test
#' - continuousTest: the type of test performed
#' @export
#' @importFrom dplyr setdiff bind_cols select mutate if_else
#' @importFrom purrr pmap_df
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

#' @title .continuousTest
#' @description performs a Welch Two Sample t-test or a t-test with equal variance based on the input parameters
#' @param m1 the sample mean of the first group
#' @param m2 the sample mean of the second group
#' @param s1 the sample standard deviation of the first group
#' @param s2 the sample standard deviation of the second group
#' @param n1 the sample size of the first group
#' @param n2 the sample size of the second group
#' @param m0 the null value for the difference in means to be tested for. Default is 0.
#' @param equal.variance whether or not to assume equal variance. Default is FALSE.
#' @return a list with:
#' - continuousPValue: the p-value of the test
#' - continuousOddsRatio: odds ration calculated as meanValueCases/meanValueControls
#' - continuousTest: the type of test performed
.continuousTest <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  # # special case
  # if(n1 <= 10 | n2 <= 10 ){
  #   return(list(
  #     continuousPValue = NA_real_,
  #     continuousOddsRatio = NA_real_,
  #     continuousTest = "no test, less than 10 samples"
  #   ))
  # }
  # Na input
  if(is.na(m1)||is.na(m2)||is.na(s1)||is.na(s2)){
    return(list(
      continuousPValue = NA_real_,
      continuousOddsRatio = NA_real_,
      continuousTest = "No test, not enough samples"
    ))
  }

  # special case
  if(m1==0 & m2==0 & s1==0 & s2==0 ){
    return(list(
      continuousPValue = 1,
      continuousOddsRatio = 0,
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

  continuousOddsRatio <- exp(t * sqrt(1/n1 + 1/n2))
  #continuousOddsRatio = m1/m2

  #dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
  #names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(list(
    continuousPValue = 2*pt(-abs(t),df),
    continuousOddsRatio = continuousOddsRatio,
    continuousTest = ifelse(is.na(se), '',"Welch Two Sample t-test")
  ))
}






