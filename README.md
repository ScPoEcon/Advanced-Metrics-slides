# **Advanced Econometrics at SciencesPo**

This is the syllabus for the 2nd year course *advanced econometrics* offered by the department of economics at SciencesPo Paris.

## Prerequisites

You need to have taken the course [**Introduction to Econometrics with R**](https://scpoecon.github.io/ScPoEconometrics/) at SciencesPo. Alternatively, if you are an exchange student, you need to have taken a course that covers the same syllabus. You will have to make sure that you are familiar with `R` knowledge we accumulate in that course as well.

## Overview

We continue with our practical approach to learning Econometrics and R where we left it in the introductory part “Introduction to Econometrics with R”. We will learn about an important method to establish casual relationships in non-experimental data, called “Instrumental Variables”. We will learn about panel data, that is, data which tracks individuals over time. We will look at situations when our outcome data is discrete in nature, like “subject i chose option A (and not B)”. And we will look at a range of simple machine learning methods which are helpful for classification and prediction tasks. 


## Syllabus and Slides

Notice that several of those lectures extend over two sessions. 

Lecture 1: Introduction, Logistics and Recap 1 from intro course. Uncertainty in regression estimates, orthogonality of error, BLUE property.  [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/01-recap/recap1.html)]

Lecture 2: Recap 2 from intro course. What's a *model*, omitted variable bias, interpreting coefficients, the log transformation [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/02-recap/recap2.html)]

Lecture 3: intro to the `data.table` package [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/03-datatable/03-datatable.html)]

Lecture 4: Instrumental Variables and Causality 1. John Snow's Cholera Experiment as a motivation for the IV estimator, using a DAG to think about the exclusion restriction, the Wald estimator. [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/04-IV/04-IV.html)]

Lecture 5: Instrumental Variables and Causality 2. 2SLS, returns to schooling and ability bias, replicating Angrist and Krueger (1991), IV mechanics, identification and inference, weak instruments [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/05-IV2/05-IV2.html)]

Lecture 6: Panel Data 1: What, How and Why? Application to crime rates at county level, within and between variation, the within transformation, running panel regressions in R [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/06-panel/06-panel.html)]

Lecture 7: Discrete Outcomes: Logit and Probit. Bernoulli reminder, Mroz dataset, the linear probability model, the saturated LPM, logit and probit marginal effects, Goodness of fit in non-linear binary response models. [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/07-probit/07-probit.html)]

Lecture 8: Intro to Statistical (or *Machine*) Learning 1: the bias-variance-tradeoff, taxonomy of methods, parametric vs non-parametric, linear vs nonlinear, relationship between variance, bias and MSE [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/08-learning/08-learning.html)]

Lecture 9: Intro to Statistical (or *Machine*) Learning 2: Subset selection (Lasso and Ridge regressions), unsupervised learning (PCA and K-means clustering) [[HTML](https://raw.githack.com/ScPoEcon/Advanced-Metrics-slides/master/lectures/09-unsupervised/09-unsupervised.html)]

## Legal

You can copy and adapt this material for your purposes, as long as you give appropriate credit and share the work yourself  under same terms. Of course you can use the slides to teach in your classrooms. *Appropriate Credit* means that somewhere in your slides there is link back to this repo, indicating that this is what you are building upon. Click on the icon for details.

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.

## How to build

* These are simply Rmarkdown slides.
* So, in Rstudio, just click *knit*.
* To print slides as PDF, do 
```decktape chapter1.html chapter1.pdf --chrome-arg=--disable-web-security```
using the awesome [decktape](https://github.com/astefanutti/decktape)
