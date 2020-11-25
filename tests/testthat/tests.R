vr <- json2veris("./data", schema = "https://raw.githubusercontent.com/vz-risk/VCDB/master/vcdb-merged.json")

test_that("a new verisr object has the correct number of records", {
  expect_equal( nrow( vr), 100)
})

test_that("a new verisr object has the correct classes", {
  # expect_equal(class(vr), c("verisr", "data.table", "data.frame")) # changed to match updated json2veris. 17-01-17 GDB
  expect_equal(class(vr), c("verisr", "data.frame"))
})

test_that("a new verisr object has these columns", {
  # Note: not testing every column, just some key ones that have been added
  # also should probably change this to a for loop or whatever R thinks
  # you should use
  expect_equal(("victim.industry.name" %in% names(vr)), TRUE)
  expect_equal(("victim.industry2" %in% names(vr)), TRUE)
  expect_equal(("victim.orgsize.Small" %in% names(vr)), TRUE)
  expect_equal(("victim.orgsize.Large" %in% names(vr)), TRUE)
  expect_equal(("victim.orgsize.Unknown" %in% names(vr)), TRUE)
  expect_equal(("victim.industry3" %in% names(vr)), TRUE)
  expect_equal(("actor.partner.industry2" %in% names(vr)), TRUE)
})

test_that("a verisr object counts actors properly: getenum(\"actor\"", {
  # Question: Do we need to test every enumeration that getenum might 
  # see?
  actors <- getenum(vr, "actor")
  expect_equal(actors[actors$enum=="External",]$x, 50)
  expect_equal(actors[actors$enum=="Internal",]$x, 45)
  expect_equal(actors[actors$enum=="Partner",]$x, 5)
  expect_equal(actors[actors$enum=="Unknown",]$x, 4)
})

test_that("post.proc() properly calculates which records are small and large victims", {
  expect_equal(sum(vr$victim.orgsize.Small), 24)
  expect_equal(sum(vr$victim.orgsize.Large), 39)
})