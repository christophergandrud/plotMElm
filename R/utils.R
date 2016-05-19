#' Function to find marginal effects
#'
#' @noRd

me_one <- function(term1_, int_term_, fitted2_,
                   beta_hat, cov, ci_, term2_0 = FALSE, obj_)
{
    if (!isTRUE(term2_0)) {
        dy_dx <- beta_hat[term1_] + beta_hat[int_term_] * fitted2_

        # Standard error
        se_dy_dx <- sqrt(cov[term1_, term1_] +
                             fitted2_ ^ 2 * cov[int_term_, int_term_] +
                             2 * fitted2_ *cov[term1_, int_term_])
    }

    else if (isTRUE(term2_0)) {
        dy_dx <- beta_hat[term1_]
        all_b_se <- coef(summary(obj_))[, 'Std. Error']
        se_dy_dx <- all_b_se[term1_]
    }

    # Confidence interval
    z <- qnorm(ci_ / 100)
    upper <- dy_dx + z * se_dy_dx
    lower <- dy_dx - z * se_dy_dx

    parts_temp <- data.frame(cbind(fitted2_, dy_dx, lower, upper))
    return(parts_temp)
}
