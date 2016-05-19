#' Function to find marginal effects
#'
#' @noRd

me_one <- function(term1_, int_term_, fitted2_,
                   beta_hat, cov, ci_, obj_)
{
    if (!missing(int_term_)) {
        # Point estimates
        dy_dx <- beta_hat[term1_] + beta_hat[int_term_] * fitted2_

        # Standard error
        se_dy_dx <- sqrt(cov[term1_, term1_] +
                             fitted2_ ^ 2 * cov[int_term_, int_term_] +
                             2 * fitted2_ *cov[term1_, int_term_])
    }

    else if (missing(int_term_)) {
        fitted2_ <- 1
        # Point estimates
        dy_dx <- beta_hat[term1_]

        # Standard error
        all_b_se <- coef(summary(obj_))[, 'Std. Error']
        se_dy_dx <- all_b_se[term1_]
    }

    parts_temp <- data.frame(cbind(fitted2_, dy_dx, se_dy_dx))
    names(parts_temp) <- c('fitted2', names(parts_temp[-1]))
    return(parts_temp)
}
