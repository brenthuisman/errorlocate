#' errorlocate
#'
#' Find errors in data given a set of validation rules.
#' The \code{errorlocate} helps to identify obvious errors in raw datasets.
#'
#' It works in tandem with the package \code{\link{validate}}.
#' With \code{validate} one can formulate data validation rules to which te data must comply.
#' For example that age cannot be negative.
#' While \code{validate} can identify if a record is valid or not, in general it does not identify
#' which of the variables are responsible for the invalidation: a set of validation rules form a web
#' of dependent variables: changing the value of an invalid record to repair for rule 1, may invalidate
#' the record for rule 2.
#'
#' Errorlocate provides a small framework for record based error detection and implements the Felligi Holt
#' algorithm. This algorithm assumes there is no other information available then the values of a record
#' and a set of validation rules. The Feligi Holt algorithm minimizes the (weighted) number of values that need
#' to be adjusted to remove the invalidation. Framed differently:
#'
#' The \code{errorlocate} package translates the validation
#' @name errorlocate-package
#' @import methods validate
#' @importFrom stats runif setNames
#' @docType package
#' @references
#'  T. De Waal (2003) Processing of Erroneous and Unsafe Data. PhD thesis, University of Rotterdam.
#'
#'  Van der Loo, M., de Jonge, E, Data Cleaning With Applications in R
#'  E. De Jonge and Van der Loo, M. (2012) Error localization as a mixed-integer program in
#'  editrules.
#'
#'  lp_solve and Kjell Konis. (2011). lpSolveAPI: R Interface for
#'  lp_solve version 5.5.2.0. R package version 5.5.2.0-5.
#'  http://CRAN.R-project.org/package=lpSolveAPI

NULL
