#' Optimization using PORT routines
#'
#' Unconstrained and box-constrained optimization using PORT routines. For historical compatibility.
#'
#' @param start numeric vector, initial values for the parameters to be optimized.
#' @param objective Function to be minimized.  Must return a scalar value.  The first
#' argument to `objective` is the vector of parameters to be
#' optimized, whose initial values are supplied through `start`.
#' Further arguments (fixed during the course of the optimization) to
#' `objective` may be specified as well (see `\dots`).
#' @param gradient Optional function that takes the same arguments as `objective` and
#' evaluates the gradient of `objective` at its first argument.  Must
#' return a vector as long as `start`.
#' @param hessian
#' Optional function that takes the same arguments as `objective` and
#' evaluates the hessian of `objective` at its first argument.  Must
#' return a square matrix of order `length(start)`.  Only the
#' lower triangle is used.
#'
#' @param ... Further arguments to be supplied to `objective`
#' @param scale See PORT documentation (or leave alone).
#' @param control A list of control parameters. See below for details.
#' @param lower,upper vectors of lower and upper bounds, replicated to be as long as
#' `start`.  If unspecified, all parameters are assumed to be unconstrained.
#'
#' @details
#' Any names of `start` are passed on to `objective` and where
#' applicable, `gradient` and `hessian`.  The parameter vector
#' will be coerced to double.
#' %% The PORT documentation is at
#' %% <http://netlib.bell-labs.com/cm/cs/cstr/153.pdf>.
#'
#' If any of the functions returns `NA` or `NaN` this is an
#' error for the gradient and Hessian, and such values for function
#' evaluation are replaced by `+Inf` with a warning.
#'
#' @example man/examples/ex-nlminb.R
#'
#' @export
nlminb <- function (start, objective, gradient = NULL, hessian = NULL,
                    ..., scale = 1, control = list(), lower = -Inf, upper = Inf)
{
    par <- setNames(as.double(start), names(start))
    n   <- length(par)
    iv  <- integer(78 + 3 * n)
    v   <- double(130 + (n * (n + 27))/2)

    .Call("port_ivset", 2, iv, v)

    if (length(control)) {
        nms <- names(control)
        if (!is.list(control) || is.null(nms))
            stop("'control' argument must be a named list")
        pos <- pmatch(nms, names(port_cpos))
        if (any(nap <- is.na(pos))) {
            warning(sprintf(ngettext(length(nap), "unrecognized control element named %s ignored",
                                     "unrecognized control elements named %s ignored"),
                            paste(sQuote(nms[nap]), collapse = ", ")),
                    domain = NA)
            pos <- pos[!nap]
            control <- control[!nap]
        }
        ivpars <- pos <= 4
        vpars <- !ivpars
        if (any(ivpars))
            iv[port_cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
        if (any(vpars))
            v[port_cpos[pos[vpars]]] <- as.double(unlist(control[vpars]))
    }

    obj <- quote(objective(.par, ...))
    rho <- new.env(parent = environment())
    assign(".par", par, envir = rho)
    grad <- hess <- low <- upp <- NULL
    if (!is.null(gradient)) {
        grad <- quote(gradient(.par, ...))
        if (!is.null(hessian)) {
            if (is.logical(hessian))
                stop("logical 'hessian' argument not allowed.  See documentation.")
            hess <- quote(hessian(.par, ...))
        }
    }
    if (any(lower != -Inf) || any(upper != Inf)) {
        low <- rep_len(as.double(lower), length(par))
        upp <- rep_len(as.double(upper), length(par))
    }
    else low <- upp <- numeric()

    # browser()
    .Call("port_nlminb", obj, grad, hess, rho, low, upp,
        d = rep_len(as.double(scale), length(par)), iv, v)

    iv1 <- iv[1L]
    list(par = get(".par", envir = rho),
         objective = v[10L],
         convergence = (if (iv1 %in% 3L:6L) 0L else 1L),
         iterations = iv[31L],
         evaluations = c(`function` = iv[6L], gradient = iv[30L]),
         message = if (19 <= iv1 && iv1 <= 43) {
            if (any(B <- iv1 == port_cpos))
                sprintf("'control' component '%s' = %g, is out of range", names(port_cpos)[B], v[iv1])
            else sprintf("V[IV[1]] = V[%d] = %g is out of range (see PORT docu.)", iv1, v[iv1])
         } else port_msg(iv1))
}


## used here and in nls(... algorithm = "port")
port_msg <- function(iv1) {
    switch(as.character(iv1),
       "3" = "X-convergence (3)",
       "4" = "relative convergence (4)",
       "5" = "both X-convergence and relative convergence (5)",
       "6" = "absolute function convergence (6)",

       "7" = "singular convergence (7)",
       "8" = "false convergence (8)",
       "9" = "function evaluation limit reached without convergence (9)",
       "10" = "iteration limit reached without convergence (10)",
       "14" = "storage only has been allocated (14)",

       "15" = "LIV too small (15)",
       "16" = "LV too small (16)",

       "63" = "fn cannot be computed at initial par (63)",
       "65" = "gr cannot be computed at initial par (65)",

       "300" = "initial par violates constraints",
       ## otherwise:
       sprintf("See PORT documentation.  Code (%d)", iv1))
}

## PORT  iv[] and v[] indices for setting and getting info :
port_cpos <-
    c(## iv[]:
      ## MXFCAL       MXITER         OUTLEV  (port.c)
      eval.max = 17L, iter.max = 18L, trace = 19L,
                      maxiter  = 18L,
      ##  v[]:
      ## AFCTOL      RFCTOL         XCTOL        XFTOL
      abs.tol = 31L, rel.tol = 32L, x.tol = 33L, xf.tol = 34L,
      ## LMAX0        LMAXS           SCTOL
      step.min = 35L, step.max = 36L, sing.tol = 37L,
      ## DINIT          ETA0 (for nlminb *only*)
      scale.init = 38L, diff.g = 42L)
## NB: until R 2.12.1, "step.min" was 34 instead of 35

## and for "output" v[]: see in ../src/port.c, also for NITER = 31 (below):
port_v_nms <-
    c(NREDUC = 6L, PREDUC = 7L, F = 10L, FDIF = 11L,
      FLSTGD = 12L, GTSLST = 14L,
      PLSTGD = 15L, RADFAC = 16L, DSTSAV = 18L)
port_get_named_v <- function(v) {
    setNames(v[port_v_nms], names(port_v_nms))
}

# % see PR#15052.
# \value{
#   A list with components:
#   \item{par}{The best set of parameters found.}
#   \item{objective}{The value of \code{objective} corresponding to \code{par}.}
#   \item{convergence}{An integer code. \code{0} indicates successful
#     convergence.
#   }
#   \item{message}{
#     A character string giving any additional information returned by the
#     optimizer, or \code{NULL}. For details, see PORT documentation.
#   }
#   \item{iterations}{Number of iterations performed.}
#   \item{evaluations}{Number of objective function and gradient function evaluations}
# }
# \section{Control parameters}{
#   Possible names in the \code{control} list and their default values
#   are:
#   \describe{
#     \item{\code{eval.max}}{Maximum number of evaluations of the objective
#       function allowed.  Defaults to 200.}% MXFCAL
#     \item{\code{iter.max}}{Maximum number of iterations allowed.
#       Defaults to 150.}% MXITER
#     \item{\code{trace}}{The value of the objective function and the parameters
#       is printed every trace'th iteration.  Defaults to 0 which
#       indicates no trace information is to be printed.}
#     \item{\code{abs.tol}}{Absolute tolerance.  Defaults
#       to 0 so the absolute convergence test is not used.  If the objective
#       function is known to be non-negative, the previous default of
#       \code{1e-20} would be more appropriate.}% AFCTOL  31
#     \item{\code{rel.tol}}{Relative tolerance.  Defaults to
#       \code{1e-10}.}% RFCTOL  32
#     \item{\code{x.tol}}{X tolerance.  Defaults to \code{1.5e-8}.}% XCTOL  33
#     \item{\code{xf.tol}}{false convergence tolerance.  Defaults to
#       \code{2.2e-14}.}% XFTOL  34
#     \item{\code{step.min, step.max}}{Minimum and maximum step size.  Both
#       default to \code{1.}.}% LMAX0 35  /  LMAXS 36
#     \item{sing.tol}{singular convergence tolerance; defaults to
#       \code{rel.tol}.}% SCTOL  37
#     \item{scale.init}{...}% DINIT  38
#     \item{diff.g}{an estimated bound on the relative error in the
#       objective function value.}% ETA0  42
#   }
# }
# source{
#   \url{http://www.netlib.org/port/}
# }
# \references{
#   David M. Gay (1990),
#   Usage summary for selected optimization routines.
#   Computing Science Technical Report 153, AT&T Bell Laboratories, Murray
#   Hill.
# }
# \author{
#   \R port: Douglas Bates and Deepayan Sarkar.

#   Underlying Fortran code by David M. Gay
# }
# \seealso{
#   \code{\link{optim}} (which is preferred) and \code{\link{nlm}}.

#   \code{\link{optimize}} for one-dimensional minimization and
#   \code{\link{constrOptim}} for constrained optimization.
# }
# % Lots of near-zeros, platform-specific results.
# \examples{\donttest{
# }}
# \keyword{optimize}
