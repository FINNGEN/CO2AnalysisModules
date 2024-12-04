
test_that(".continuousTest works as expected", {

  # Test 1: Standard case with unequal variances
  result <- .continuousTest(m1 = 5, m2 = 3, s1 = 1, s2 = 2, n1 = 30, n2 = 30, equal.variance = FALSE)
  expect_type(result, "list")
  expect_equal(result$continuousTest, "Welch Two Sample t-test")
  expect_true(result$continuousPValue < 1)
  expect_true(result$continuousOddsRatio > 0)

  # Test 2: Standard case with equal variances
  result <- .continuousTest(m1 = 5, m2 = 3, s1 = 1, s2 = 1, n1 = 30, n2 = 30, equal.variance = TRUE)
  expect_type(result, "list")
  expect_equal(result$continuousTest, "Welch Two Sample t-test")
  expect_true(result$continuousPValue < 1)
  expect_true(result$continuousOddsRatio > 0)

  # Test 3: Edge case with means and variances being zero
  result <- .continuousTest(m1 = 0, m2 = 0, s1 = 0, s2 = 0, n1 = 30, n2 = 30)
  expect_equal(result$continuousPValue, 1)
  expect_equal(result$continuousOddsRatio, 0)

  # Test 4: NA input
  result <- .continuousTest(m1 = NA, m2 = 3, s1 = 1, s2 = 2, n1 = 30, n2 = 30)
  expect_equal(result$continuousPValue, NA_real_)
  expect_equal(result$continuousOddsRatio, NA_real_)
  expect_equal(result$continuousTest, "No test, not enough samples")
})

test_that(".binaryTest works as expected", {

  # Test 1: Standard case for Chi-square test
  result <- .binaryTest(a = 20, b = 30, c = 15, d = 35, fisherLimit = 10)
  expect_type(result, "list")
  expect_equal(result$countsTest, "Chi-square")
  expect_true(result$countsOddsRatio > 0)
  expect_true(result$countsPValue < 1)

  # Test 2: Standard case for Fisher's test
  result <- .binaryTest(a = 2, b = 8, c = 3, d = 7, fisherLimit = 10)
  expect_type(result, "list")
  expect_equal(result$countsTest, "Fisher")
  expect_true(result$countsPValue <= 1)
  expect_true(result$countsOddsRatio > 0)

  # Test 3: Edge case with all counts being zero
  result <- .binaryTest(a = 0, b = 0, c = 0, d = 0, fisherLimit = 10)
  expect_equal(result$countsPValue, 1)  # P-value should be 1 when no events are observed
  expect_equal(result$countsOddsRatio, 0)  # Odds ratio should be 0 when no events are observed
  expect_equal(result$countsTest, "Fisher")  # Defaults to Fisher's test

  # Test 4: Edge case just below Fisher's limit
  result <- .binaryTest(a = 5, b = 5, c = 1, d = 1, fisherLimit = 10)
  expect_type(result, "list")
  expect_equal(result$countsTest, "Fisher")  # Should use Fisher's test

  # Test 5: Edge case just above Fisher's limit
  result <- .binaryTest(a = 10, b = 10, c = 10, d = 10, fisherLimit = 10)
  expect_type(result, "list")
  expect_equal(result$countsTest, "Chi-square")  # Should use Chi-square test

  # Test 6: All counts are equal
  result <- .binaryTest(a = 10, b = 10, c = 10, d = 10, fisherLimit = 5)
  expect_type(result, "list")
  expect_equal(result$countsTest, "Chi-square")  # Chi-square should still apply

  # Test 7: Counts resulting in a very small p-value
  result <- .binaryTest(a = 10, b = 100, c = 1, d = 10000, fisherLimit = 10)
  expect_true(result$countsPValue < 0.05)  # Expect a small p-value

})


