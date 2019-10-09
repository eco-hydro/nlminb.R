test_that("multiplication works", {
  expect_true({
      x <- rnbinom(100, mu = 10, size = 10)
      hdev <- function(par) -sum(dnbinom(x, mu = par[1], size = par[2], log = TRUE))

      all.equal(nlminb2(c(9, 12), hdev), nlminb(c(9, 12), hdev))
  })
})

#
# microbenchmark::microbenchmark(
#     nlminb(c(9, 12), hdev),
#     nlminb2(c(9, 12), hdev)
# )

x <- rnbinom(100, mu = 10, size = 10)
hdev <- function(par) -sum(dnbinom(x, mu = par[1], size = par[2], log = TRUE))

fn = function(x) x - cos(x)

bench::mark(
    optim(0, fn, method='BFGS')
)

