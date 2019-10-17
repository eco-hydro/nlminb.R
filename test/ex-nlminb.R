devtools::load_all()

x <- rnbinom(100, mu = 10, size = 10)
hdev <- function(par)
    -sum(dnbinom(x, mu = par[1], size = par[2], log = TRUE))
# nlminb(c(9, 12), hdev)
nlminb2(c(9, 12), hdev, control = list(eval.max = 1000, iter.max = 1000))

bench::mark(
    nlminb(0, function(x) x - cos(x))
)

library(phenofit)
library(JuliaCall)
julia <- julia_setup()
julia_library("nlminb")



par0 = c(0.05, 0.6, 45, 0.1, 200, 0.2)
par  = c(0.1 , 0.7, 50, 0.1, 250, 0.1)

t = seq(1, 366, 8)
y = doubleLog_Beck(par, t)

## Try about parallel
{
    b_r <- bench::mark({
        ypred = doubleLog_Beck(par0, t)
        opt_nlminb(par0, f_goal, fun = doubleLog_Beck, y = y, t = t)
    }, min_time = 10)

    b_julia <- bench::mark({
        julia_assign("par0", par0)
        julia_assign("t", t)
        julia_assign("y", y)
        julia_assign("ypred", ypred)

        cmd <- "opt_par = optim_nlminb(par0, goal!, doubleLog_Beck!, y, t, ypred, verbose = false,
                              eval_max = 1000, iter_max = 1000)"
        julia_eval(cmd)
    }, min_time = 10)

    b_r$median/b_julia$median
}
