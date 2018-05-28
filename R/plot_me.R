#' Plot marginal effects from two-way interactions in linear regressions
#'
#' @param obj fitted model object from \code{lm}.
#' @param term1 character string of the first constitutive term of the
#' interaction's variable name.
#' @param term2 character string of the second constitutive term of the
#' interaction's variable name.
#' @param fitted2 numeric vector of fitted values of \code{term2} to plot for.
#' If unspecified, then all unique observed values are used.
#' @param ci numeric. confidence interval level, expressed on the ]0, 100[
#' interval. The default is \code{95}.
#' @param ci_type character string specifying the type of confidence interval
#' to find and plot. If \code{'standard'} then standard confidence intervals
#' (e.g. those suggested by Brambor, Clark, and Golder 2006) are found.
#' If \code{fdr} then confidence intervals are found using critical t-statistics
#' to limit the false discovery rate (limit over confidence).
#' @param t_statistic numeric. Custom t-statistic for finding the confidence interval.
#' May be useful if the user want to use a funciton like \code{findMultiLims}
#' to find the t-statistic.
#' @param plot boolean. return plot if \code{TRUE}; return \code{data.frame} of
#' marginal effects estimates if \code{FALSE}.
#'
#' @return a \code{gg} class ggplot2 object
#'
#' @examples
#' ## Continuous Term1 and Term2
#' # Estimate model
#' states <- as.data.frame(state.x77)
#' m1 <- lm(Murder ~ Income * Population, data = states)
#'
#' # Plot marginal effect of Income across the observed range of Population
#' # on the Murder rate
#' plot_me(m1, 'Income', 'Population', ci = 95)
#'
#' # CI created using false discovery rate limiting t-statistic
#' plot_me(m1, 'Income', 'Population', ci_type = 'fdr')
#'
#' # Return marginal effects as a data frame
#' plot_me(m1, 'Income', 'Population', plot = FALSE)
#'
#' ## Term 2 with <= 5 unique values
#' # Estimate model
#' m2 <- lm(mpg ~ wt * cyl, data = mtcars)
#'
#' # Plot marginal effect of Weight across the Number of Cylinders (continuous)
#' plot_me(m2, 'wt', 'cyl')
#'
#' ## Categorical (factor) Term2
#' # Set Term 2 as a factor variable
#' mtcars$cyl <- factor(mtcars$cyl,
#'                  labels = c('4 Cyl', '6 Cyl', '8 Cyl'))
#'
#' # Estimate model
#' m3 <- lm(mpg ~ wt * cyl, data = mtcars)
#'
#' # Plot marginal effect of Weight across the Number of Cylinders (factor)
#' plot_me(m3, 'wt', 'cyl')
#'
#' @source Inspired by:
#' \url{http://www.statsblogs.com/2013/08/27/creating-marginal-effect-plots-for-linear-regression-models-in-r/}
#'
#' @importFrom stats coef qnorm vcov
#' @importFrom interactionTest fdrInteraction
#' @import ggplot2
#'
#'
#' @source Benjamini, Yoav, and Yosef Hochberg. 1995. "Controlling the False
#' Discovery Rate: A Practical and Powerful Approach to Multiple Testing".
#' Journal of the Royal Statistical Society, Series B 57(1): 289--300.
#'
#' Brambor, Thomas, William Roberts Clark, and Matt Golder.
#' "Understanding interaction models: Improving empirical analyses". Political
#' Analysis 14.1 (2006): 63-82.
#'
#' Esarey, Justin, and Jane Lawrence Sumner. 2015. "Marginal Effects in
#' Interaction Models: Determining and Controlling the False Positive Rate".
#' URL: \url{http://jee3.web.rice.edu/interaction-overconfidence.pdf}.
#' @export

plot_me <- function(obj, term1, term2, fitted2, ci = 95, ci_type = 'standard',
                    t_statistic, plot = TRUE)
{
    dy_dx <- lower <- upper <- NULL

        # Sanity checks
    if (class(obj) != 'lm') stop('Only lm model objects can be used.',
            call. = FALSE)

    if (is.factor(obj$model[, term1])) stop('term1 cannot be a factor variable.',
                               call. = FALSE)

    ci_type <- tolower(ci_type)
    if (!(ci_type %in% c('standard', 'fdr', 'boot')))
        stop(sprintf("ci_type '%s' not supported.", ci_type), call. = FALSE)

    beta_hat_ <- coef(obj)
    cov_ <- vcov(obj)

    # Determine if term2 is categorical
    term2_dist <- obj$model[, term2]
    factor_term2 <- is.factor(term2_dist)
    if (isTRUE(factor_term2)) {
        term2_original <- term2
        term2 <- sprintf('%s%s', term2, levels(term2_dist)[-1])
    }

    int_term12 <- sprintf('%s:%s', term1, term2)
    int_term21 <- sprintf('%s:%s', term2, term1)

    if (!(term1 %in% names(beta_hat_))) stop(sprintf('%s not found.', term1),
                                            call. = FALSE)
    if (all(!(c(int_term12, int_term21) %in% names(beta_hat_)))) {
        stop('Interaction term not found.', call. = FALSE)
    }
    if (any(int_term12 %in% names(beta_hat_))) int_term <- int_term12
    else int_term <- int_term21

    # For rug plot
    term2_dist <- data.frame(term2 = term2_dist)

    if (missing(fitted2)) {
        if(!isTRUE(factor_term2)) {
            fitted2 <- sort(unique(term2_dist$term2))
        }
        else if (isTRUE(factor_term2)) {
            fitted2 <- levels(term2_dist$term2)
        }
    }

    if (length(int_term) == 1) parts <- me_one(term1_ = term1,
                                               int_term_ = int_term,
                                               fitted2_ = fitted2,
                                               beta_hat = beta_hat_,
                                               cov = cov_)
    else {
        parts <- data.frame()
        for (i in 1:length(fitted2)) {
            if (i == 1) {
                one_level <- me_one(term1_ = term1, beta_hat = beta_hat_,
                                    obj_ = obj)
            }
            else {
                one_level <- me_one(term1_ = term1, int_term_ = int_term[i-1],
                                    fitted2_ = 1,  beta_hat = beta_hat_,
                                    cov = cov_)
            }
            one_level$fitted2 <- fitted2[i]
            parts <- rbind(parts, one_level)
        }
        parts$fitted2 <- factor(parts$fitted2, labels = fitted2,
                                levels = parts$fitted2[1:length(parts$fitted2)])
    }

    # Find confidence intervals
    if (missing(t_statistic)) {
        if (ci_type == 'standard') {
            t_statistic <- qnorm(ci / 100)
        }
        else if (ci_type == 'fdr') {
            ci <- ci / 100
            t_statistic <- fdrInteraction(me.vec = parts$dy_dx, me.sd.vec = parts$se_dy_dx,
                                df = obj$df, level = ci)
            message(sprintf('t-statistic used: %s', round(t_statistic,
                                                          digits = 3)))
        }
        else if (ci_type == 'boot') {
            stop('boot not supported yet . . .', call. = FALSE)
        }
    }
    else if (!missing(t_statistic)) {
        message('Using custom t-statistic (ignoring ci_type argument).')
    }

    parts$upper <- parts$dy_dx + t_statistic * parts$se_dy_dx
    parts$lower <- parts$dy_dx - t_statistic * parts$se_dy_dx


    if (plot) {
        if (length(parts$fitted2) > 5) {
            ggplot(parts, aes(fitted2, dy_dx)) +
                geom_rug(data = term2_dist, aes(x = term2),
                         sides = 'b', alpha = 0.5, inherit.aes = FALSE) +
                geom_hline(yintercept = 0, linetype = 'dotted') +
                geom_line() +
                geom_ribbon(data = parts, aes(ymin = lower, ymax = upper),
                            alpha = 0.1) +
                xlab(sprintf('\n%s', term2)) +
                ylab(sprintf('Marginal effect of\n%s\n', term1)) +
                theme_bw()
        }
        else if (length(parts$fitted2) <= 5 || is.factor(parts$fitted2)) {
            p <- ggplot(parts, aes(fitted2, dy_dx, ymin = lower,
                                   ymax = upper)) +
                        geom_pointrange() +
                        geom_hline(yintercept = 0, linetype = 'dotted') +
                        ylab(sprintf('Marginal effect of\n%s\n', term1)) +
                        theme_bw()
            if (!is.factor(parts$fitted2)) {
                    p + geom_line(alpha = 0.3) +
                        scale_x_continuous(breaks = parts$fitted) +
                        xlab(sprintf('\n%s', term2))
            }
            else if (is.factor(parts$fitted2)) {
                p + scale_x_discrete(labels = parts$fitted2) +
                    xlab(sprintf('\n%s', term2_original))
            }
        }

    } else {
        return(parts)
    }
}
