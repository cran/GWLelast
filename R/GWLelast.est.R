#' GWLelast.est
#'
#' Fitting geographically weighted logistic elastic net regression
#'
#' @param x Covariates.
#' @param y Outcome binary variable.
#' @param coords 2 columns matrix including "longitude" and "latitude".
#' @param alpha The elasticnet mixing parameter [0,1] in glmnet package.
#' @param lambda Optional user-supplied lambda sequence in glmnet package.
#' @param nlambda The number of lambda values in glmnet package.
#' @param gweight geographical kernel function in spgwr package.
#' @param longlat Indicate if the coords parameter are sperically calculated.
#' @param bw bandwidth of geographical kernel function.
#' @param D Distance matrix.
#'
#' @importFrom stats dist
#'
#' @return model: Fitted model at location i.
#' @return error: Cross validation error.
#' @export

GWLelast.est = function(x, y, coords, D = NULL, alpha = 1, lambda = NULL,
                        nlambda = NULL, gweight = c("gwr.Gauss","gwr.bisquare"),
                        longlat = TRUE, bw = bw
) {


  if(is.null(D) & longlat){
    D = distm(coords)
  }else if(is.null(D) & !longlat){
    D = dist(coords)
  }

  if(is.null(bw)){
    cat("Please select a bandwidth parameter (bw)!")
  }

  W = gweight(D, bw)
  result = list()

  result = GWLelast.inner(x=x, y = y, coords = coords, W = W, lambda = lambda, alpha = alpha, nlambda = nlambda)

  return(result)
}
