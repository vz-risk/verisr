vr <- json2veris("./data")

test_that("a new verisr object has the correct number of records", {
  expect_equal( nrow( vr), 100)
})

test_that("a new verisr object has the correct classes", {
  expect_equal(class(vr), c("verisr", "data.table", "data.frame"))
})

test_that("a new verisr object has these columns", {
  # Note: not testing every column, just some key ones that have been added
  # also should probably change this to a for loop or whatever R thinks
  # you should use
  expect_equal(("victim.industry.name" %in% names(vr)), TRUE)
  expect_equal(("victim.industry2" %in% names(vr)), TRUE)
  expect_equal(("victim.orgsize.Small" %in% names(vr)), TRUE)
  expect_equal(("victim.orgsize.Large" %in% names(vr)), TRUE)
  expect_equal(("victim.industry3" %in% names(vr)), TRUE)
  expect_equal(("actor.partner.industry2" %in% names(vr)), TRUE)
  expect_equal(("pattern" %in% names(vr)), TRUE)
})

test_that("a verisr object counts actors properly: getenum(\"actor\"", {
  # Question: Do we need to test every enumeration that getenum might 
  # see?
  actors <- getenum(vr, "actor")
  expect_equal(actors[actors$enum=="External",]$x, 52)
  expect_equal(actors[actors$enum=="Internal",]$x, 42)
  expect_equal(actors[actors$enum=="Partner",]$x, 6)
  expect_equal(actors[actors$enum=="Unknown",]$x, 1)
})

test_that("post.proc() properly calculates which records are small and large victims", {
  expect_equal(sum(vr$victim.orgsize.Small), 22)
  expect_equal(sum(vr$victim.orgsize.Large), 41)
})