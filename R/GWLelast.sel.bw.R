#' GWLelast.sel.bw
#'
#' Bandwidth selection forgeographically weighted logistic elastic net regression
#'
#' @param x Covariates.
#' @param y Outcome binary variable.
#' @param coords 2 columns matrix including "longitude" and "latitude".
#' @param alpha The elasticnet mixing parameter [0,1] in glmnet package.
#' @param lambda Optional user-supplied lambda sequence in glmnet package.
#' @param nlambda The number of lambda values in glmnet package.
#' @param gweight geographical kernel function in spgwr package.
#' @param longlat Indicate if the coords parameter are sperically calculated.
#' @param lower.bw Lower limit of bandwidth in geographical kernel.
#' @param upper.bw Upper limit of bandwidth in geographical kernel.
#' @param D Distance matrix.
#'
#' @importFrom stats dist
#' @importFrom stats optimize
#'
#' @return optimal.bw Optimal bandwidth.
#' @export
#'
#' @examples
#' ######################
#' # Need to add
#  ######################



GWLelast.sel.bw = function (x, y, coords, D = NULL, alpha =1, lambda = NULL, nlambda = NULL,
                            gweight = gweight, longlat = TRUE, lower.bw = NULL, upper.bw = NULL
) {

  if(is.null(D) & longlat){
    D = distm(coords)
  }else if(is.null(D) & !longlat){
    D = dist(coords)
  }

  if(is.null(lower.bw) | is.null(upper.bw)){

    box = cbind(range(coords[, 1]), range(coords[, 2]))
    dif.min = spDistsN1(box, box[2, ], longlat)[1]

    lower.bw = dif.min/1000
    upper.bw = dif.min*100
  }

  opt <- optimize(GWLelast.cv.bw, lower = lower.bw, upper = upper.bw,
                  maximum = FALSE, x = x, y = y, D = D, coords = coords, alpha=alpha, lambda = lambda, nlambda = nlambda,
                  gweight = gweight, longlat = longlat,
  )

  optimal.bw <- opt$minimum
  return(optimal.bw)
}
