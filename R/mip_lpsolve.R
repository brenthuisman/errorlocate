# provides an interface to mip solvers.
# currently only lpSolveAPI, should be workable for glpt

#' translate linear rules into an lp problem
#' @param  rules mip rules
#' @param objective function
#' @param eps accuracy for equality/inequality
#' @param ... additional \code{\link{lp.control}} parameters that are set for the mip problem
translate_mip_lp <- function( rules
                            , objective=NULL
                            , eps = 1e-3
                            , ...
                            ){
  lc <- get_mr_matrix(rules)
  type <- get_mr_type(rules)

  mat <- lc$A
  nvar <- ncol(mat)

  dir <- lc$operator
  dir[strict <- dir=="<"] <- "<="

  rhs <- ifelse(strict, lc$b - eps, lc$b)

  is_binary <- type  == "binary"
  types = ifelse(is_binary, "B", "C")

  is_double <- types == "C"
  bounds <- list()
  if (any(is_double)){
    columns <- type[is_double]
    columns <- match(names(columns), colnames(mat))
    bounds <- list( lower = list(ind = columns, val = rep(-Inf, length(columns))))
  }

  # should improve performance quite a lot: a SOS1 makes bin variables exclusive.
  sos  <- asSOS(colnames(mat))
  if (NROW(sos)){
    mat <- rbind(mat, sos)
    dir <- c(dir, rep("==", nrow(sos)))
    rhs <- c(rhs, rep(1, nrow(sos)))
  }


  if (length(objective)){
    obj <- numeric(nvar)
    m <- match(names(objective), colnames(mat))
    obj[m] <- unname(objective)
  }
  list(
    obj = obj,
    mat = mat,
    dir = dir,
    rhs = rhs,
    bounds = bounds,
    types = types,
    max = FALSE,
    control = list(presolve = FALSE, verbose = FALSE, ...)
  )
}

# splits category names (<variable>:<category>) into variable column groups needed
# for SOS1 constraints
asSOS <- function(vars){
  CAT <- ":.+"

  idx <- grepl(CAT, vars)
  var <- sub(CAT, "", vars)

  sosname <- unique(var[idx])
  if (length(sosname) == 0){
    return(NULL)
  }

  t(sapply(sosname, function(sos){
    as.integer(var == sos)
  }))
}

### testing

# v <- validator( a>1, b+4 > c-z, A %in% "a")
# rules <- lin_as_mip_rules(v)
# translate_mip_lp(c(rules, cat_as_mip_rules(v)))
