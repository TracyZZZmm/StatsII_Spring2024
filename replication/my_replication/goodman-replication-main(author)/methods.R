library(methods)


setClass(
  "Method",
  slots=c(
    name="character",
    str="character",
    color="character",
    decision_function="ANY",
    uses_mpsd="logical",
    uses_alpha="logical"
  ),
  prototype=list(
    name=NA_character_,
    str=NA_character_,
    color=NA_character_,
    decision_function=function(){return(NA)},
    uses_mpsd=NA,
    uses_alpha=NA
  )
)

setGeneric(
  "getDecision",
  function(method, x, mu_0, mpsd=NULL, alpha=NULL, ...) {
    standardGeneric("getDecision")
  }
)

setMethod(
  "getDecision",
  "Method",
  function(method, x, mu_0, mpsd=NULL, alpha=0.05, ...){
    if (method@uses_mpsd & method@uses_alpha) {
      return(method@decision_function(x=x, mu_0=mu_0, mpsd=mpsd, alpha=alpha, ...))
    } else if (method@uses_mpsd) {
      return(method@decision_function(x=x, mu_0=mu_0, mpsd=mpsd, ...))
    } else if (method@uses_alpha) {
      return(method@decision_function(x=x, mu_0=mu_0, alpha=alpha, ...))
    } else {
      return(method@decision_function(x=x, mu_0=mu_0, ...))
    }
  }
)


Method = function(name, decision_function, color="#000000"){
  decision_function_args = formalArgs(decision_function) 
  
  methods::new(
    "Method",
    name=name,
    str=gsub("\\W", "_", tolower(name)),
    color=color,
    decision_function=decision_function,
    uses_mpsd = "mpsd" %in% decision_function_args,
    uses_alpha = "alpha" %in% decision_function_args
  )
}


################################################################################
################################################################################
# Methods 1: the traditional t test method #####################################
# The method performs a traditional two-sided t test to decide 
# whether to reject the null hypothesis based on a given significance level α.
# If the calculated p value is less than or equal to α, the null hypothesis is rejected.
conventional_decision_function = function(x, mu_0, alpha=0.05) {
  #' Conventional: two tailed t-test with alpha
  #' same as t.test(x, mu=mu_0, alternative="two.sided")$p.value < alpha
  t = (mean(x) - mu_0) / sd(x) * sqrt(length(x))
  p = 2 * pt(abs(t), df=length(x)-1, lower.tail=FALSE)
  return(p <= alpha)
}
################################################################################

################################################################################
# Method 2: T-test method for small alpha variants##############################
# This is a two-sided T-test for a small alpha variant, $
# where the value of alpha is set to 0.005 instead of the traditional 0.05.
# If the calculated p-value is less than or equal to 0.005, the null hypothesis is rejected.
small_alpha_decision_function = function(x, mu_0) {
  #' Small alpha: two tailed t-test with alpha=0.005
  #' same as t.test(x, mu=mu_0, alternative="two.sided")$p.value < 0.005
  #' Small alpha: Two-tailed t test, alpha=0.005
  #' is the same as t.est (x, mu=mu_0, alternative=" two-.sided ")$p. Value & lt; 0.005
  return(conventional_decision_function(x, mu_0, alpha=0.005))
}
################################################################################



################################################################################
# Method 3: Distance method：###################################################
# This method is based on a set minimum practical significant distance (MPSD),
# If the distance between the sample mean and the hypothesis is greater than or equal to MPSD, 
# the null hypothesis is rejected.
distance_only_decision_function = function(x, mpsd, mu_0) {
  #' reject if empirical mean is in the thick null
  return(abs(mean(x)-mu_0) >= mpsd)
}
################################################################################

################################################################################
# Method 4: Interval estimation method：########################################
# This method is a confidence interval based decision making method.
# It determines whether to reject the null hypothesis by comparing the difference 
# between the sample mean and the assumed mean and the length of the confidence interval.
# If the difference exceeds the length of the confidence interval plus the thick null hypothesis, 
# the null hypothesis is rejected; Otherwise, accept the null hypothesis.
interval_based_decision_function = function(x, mpsd, mu_0, alpha=0.05) {
  #' reject if confidence interval and thick null don't overlap
  #' 
  #' important: We use the confidence interval that assumes the t statistic we 
  #' calculated is t-distributed while
  #' GSK use the confidence interval that assumes that the t statistic is normal
  #' distributed
  #' -> For small n, our confidence interval will be bigger
  #' e.g. for minimal n = 5 our CI will be 2.77/1.96 = 1.41 times the size of GSK
  #' -> Our test is less likely to reject H0
  return(abs(mean(x) - mu_0) > sd(x) / sqrt(length(x)) * qt(1 - alpha/2, length(x)-1) + mpsd)
}
################################################################################

################################################################################
# Method 5: Thick t test method：###############################################
# The method calculates the expected point p value under the thick zero hypothesis.
# It considers the possible distribution of the sample mean under the thick zero hypothesis, 
# and then calculates the expected point p value.
# If the expected point p value is less than or equal to the given significance level α, 
# the null hypothesis is rejected.
# Methods that are not in GSK:
thick_t_test_decision_function = function(x, mpsd, mu_0, alpha=0.05) {
  #' We calculate the expected point-p-value under the thick null hypothesis,
  #' where the point-p-value is the probability to get a more extreme result with respect 
  #' to mu_0 given that mu is really equal to some specific value mu* in [mu_0-mpsd,mu_0+mpsd]:
  #' 
  #' p = P(|mean(X)-mu_0| > |mean(x)-mu_0| | mu in [mu_0-mpsd,mu_0+mpsd])
  #'   = E(P(|mean(X)-mu_0| > |mean(x)-mu_0| | mu = mu*) | mu* in [mu_0-mpsd,mu_0+mpsd])
  #'   = int_{mu* in [mu_0-mpsd,mu_0+mpsd]} P(|mean(X)-mu_0| > |mean(x)-mu_0| | mu=mu*) f_{H_0}(mu*) dmu*
  #' 
  #' with
  #' 
  #' P(|mean(X)-mu_0| > |mean(x)-mu_0| | mu=mu*)
  #' = P(mean(X)-mu_0 > |mean(x)-mu_0| | mu=mu*) + P(mean(X)-mu_0 < -|mean(x)-mu_0| | mu=mu*)
  #' = P((mean(X)-mu*)/sd(x)*sqrt(n) > (mu_0 + |mean(x)-mu_0| - mu*)/sd(x)*sqrt(n) | mu=mu*)
  #'   + P((mean(X)-mu*)/sd(x)*sqrt(n) < (mu_0 - |mean(x)-mu_0| - mu*)/sd(x)*sqrt(n) | mu=mu*)
  #' = pt((mu_0 + |mean(x)-mu_0| - mu*)/sd(x)*sqrt(n), df=length(x)-1, lower.tail=FALSE)
  #'   + pt((mu_0 - |mean(x)-mu_0| - mu*)/sd(x)*sqrt(n), df=length(x)-1, lower.tail=TRUE)
  #' 
  #' where pt is the cdf of the t-distribution.
  #' 
  #' 
  #' To calculate this we need the distribution of mu under the thick null f_{H_0}.
  #' Usually we don't have that, we need to assume some distribution, that's what's
  #' Bayesian about the method.
  #' Since we are doing a simulation, we know that mu ~ U(mu-mpsd, mu+mpsd) where
  #' U is the uniform distribution on the integers in the thick null interval. Hence:
  #' 
  #' p = sum_{mu* in [mu_0-mpsd,mu_0+mpsd]} P(|mean(X)-mu_0| > |mean(x)-mu_0| | mu=mu*) / (2mpsd+1)
  #' 
  #' We reject H_0 if p < alpha
  #' 
  #' Motivation:
  #' If our assumption about the distribution of mu is correct, this test 
  #' guarantees that the probability to make a type one error is exactly alpha
  
  mu_point = (mu_0 - mpsd):(mu_0 + mpsd)
  
  t_right  = (mu_0 + abs(mean(x) - mu_0) - mu_point) / sd(x) * sqrt(length(x))
  t_left   = (mu_0 - abs(mean(x) - mu_0) - mu_point) / sd(x) * sqrt(length(x))
  p = pt(t_right, df=length(x)-1, lower.tail=FALSE) + pt(t_left, df=length(x)-1, lower.tail=TRUE)
  p_exp = mean(p)

  return(p_exp <= alpha)
}
################################################################################
################################################################################

################################################################################
# Method 6: MESP method：#######################################################
# MESP method is a criterion of mixed effect size plus p value proposed by the authors. 
# It combines a minimum effect size threshold and a P-value threshold.
# If two conditions (p value less than or equal to α and distance greater than or equal to MPSD) are met, 
# then the null hypothesis is rejected.
mesp_decision_function = function(x, mpsd, mu_0, alpha=0.05) {
  #' Minimum effect size plus p-value
  #' proposed by GSK
  return(conventional_decision_function(x, mu_0, alpha) &
           distance_only_decision_function(x, mpsd, mu_0))
}
################################################################################











# 这个函数假设在厚零假设下，mu的分布是均匀分布（Uniform Distribution），
# 即假设mu在[mu_0 - mpsd, mu_0 + mpsd]区间内等概率分布。
# 因此，这个函数计算了在这个区间内mu的密度函数（PDF），
# 然后将其与样本观测值进行积分，得到期望点p值。
# 这个函数的优势在于它使用了更接近实际情况的假设分布。
thick_t_test_flat_decision_function = function(x, mpsd, mu_0, alpha=0.05) {
  #' continuous version of thick_t_test_decision_function
  
  integrand = function (mu_point, n, s, m, mpsd) {
    t_right  = (mu_0 + abs(m - mu_0) - mu_point) / s * sqrt(n)
    t_left   = (mu_0 - abs(m - mu_0) - mu_point) / s * sqrt(n)
    p = pt(t_right, df=n-1, lower.tail=FALSE) + pt(t_left, df=n-1, lower.tail=TRUE)
    f0 = 1 / (2*mpsd)
    return(p * f0)
  }
  p_exp = integrate(integrand, mu_0-mpsd, mu_0+mpsd, n=length(x), s=sd(x), m=mean(x),mpsd=mpsd, abs.tol=0.0001)$value
  return(p_exp <= alpha)
}


thick_t_test_normal_decision_function = function(x, mpsd, mu_0, alpha=0.05) {
  # 假设分布在H0下的dnorm(mu_0,50 /sqrt(12))在H0区间的边缘截断
  # 对于这个大的方差，正态分布在mpsd区间内是如此平坦，以至于我们没有看到有意义的差异
  # 与ROC曲线上的平厚t检验相比较。如果我们选择一个较小的方差(例如sd=50/sqrt(12)/4)，我们可以
  # ROC曲线接近传统t检验的曲线
  #' Assumed distribution under H0 ist dnorm(mu_0, 50/sqrt(12)) truncated at the edges of the H0 interval
  #' For this big variance the normal distribution is so flat in the mpsd interval that we se no meaningfull difference
  #' to the flat thick t-test in the ROC curve. If we choose a smaller variance (e.g. sd=50/sqrt(12)/4) we can
  #' see that the ROC curve approaches that of the conventional t-test
  integrand = function (mu_point, n, s, m, mpsd) {
    t_right  = (mu_0 + abs(m - mu_0) - mu_point) / s * sqrt(n)
    t_left   = (mu_0 - abs(m - mu_0) - mu_point) / s * sqrt(n)
    p = pt(t_right, df=n-1, lower.tail=FALSE) + pt(t_left, df=n-1, lower.tail=TRUE)
    f0 = dnorm(mu_point, mu_0, 50/sqrt(12)) / (1 - 2 * pnorm(mu_0 - mpsd, mu_0, 50 / sqrt(12)))  # density of the truncated normal dist. 
    return(p * f0)
  }
  p_exp = integrate(integrand, mu_0-mpsd, mu_0+mpsd, n=length(x), s=sd(x), m=mean(x), mpsd=mpsd, abs.tol=0.0001)$value
  return(p_exp <= alpha)
}


conventional = Method("Conventional", conventional_decision_function, "#02b0f3")
small_alpha = Method("Small-alpha", small_alpha_decision_function, "#bf8f00")
mesp = Method("MESP", mesp_decision_function, "#702da0")
distance_only = Method("Distance-only", distance_only_decision_function, "#fdc100")
interval_based = Method("Interval-based", interval_based_decision_function, "#538136")
thick_t_test = Method("Thick t-test", thick_t_test_decision_function, "#999999")


thick_t_test_flat = Method("Thick t-test flat", thick_t_test_flat_decision_function, "#777777")
thick_t_test_normal = Method("Thick t-test normal", thick_t_test_normal_decision_function, "#444444")


# Methods in GSK
# GSK_METHODS 包含了 GSK（GSK公司）提出的五种方法，
# 分别是 conventional、small_alpha、mesp、distance_only 和 interval_based
GSK_METHODS = c(conventional, small_alpha, mesp, distance_only, interval_based)

# METHODS 是基于 GSK_METHODS 扩展而来的集合，增加了一个名为 thick_t_test 的方法
METHODS = c(GSK_METHODS, thick_t_test)

# EXTENDED_METHODS 是在 METHODS 的基础上再扩展了两种方法，
# 分别是 thick_t_test_flat 和 thick_t_test_normal
EXTENDED_METHODS = c(METHODS, thick_t_test_flat, thick_t_test_normal)

