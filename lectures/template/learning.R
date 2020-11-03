rm(list=ls())
library(ggplot2)
library(splines)
# 
# tfun <- function(x) {
#     x + 0.1 * x^2 - 0.002 * x^3 + 0.00001 *x^4
# }

# sin(x-2) + x-0.05*x^2
simdata <- function(fun = function(x) {x*sin(x-2) + 0.2*x},n=90,eps = 1,df1=5, df2=20, df3=60, ub = 5,nnew = 200){
    set.seed(1234)
    
    r = data.frame(x = seq(0,ub,length.out = n))
    r$truth = fun(r$x) 
    r$epsi = rnorm(n,mean = 0, sd = eps)
    r$y = r$truth + r$epsi
    # browser()
    mods = list()
    mods$lm = lm(y ~ x,data = r)
    mods$low = smooth.spline(r$x,r$y,df = df1)
    mods$hi = smooth.spline(r$x,r$y, df = df2)
    mods$hi2 = smooth.spline(r$x,r$y, df = df3)
    # mods$ns5 = gam::gam(formula = as.formula(paste0("y ~ s(x,",df1,")")),data = r)
    # mods$ns5 = mgcv::gam(formula = as.formula(paste0("y ~ ns(x,df = ",df1,")")),data = r)
    # mods$ns20 = gam::gam(formula = as.formula(paste0("y ~ ns(x,df = ",df2,")")),data = r)
    # mods$ns20 = mgcv::gam(formula = as.formula(paste0("y ~ ns(x,df = ",df2,")")),data = r)
    
    names(mods)[2:4] <- paste0("s",c(df1,df2,df3))
    
    
    # mses
    mses = list(
        train = colMeans(sapply(mods,residuals)^2)
    )
    
    # predictions
    preds = data.frame(x = seq(0,ub, length.out = nnew))
    preds = cbind(preds, lm = predict(mods$lm,newdata = preds))
    preds = cbind(preds,sapply(mods[2:4],function(x) predict(x,preds$x)$y))
    preds$truth = fun(preds$x) #+ rnorm(nnew,mean = 0, sd = eps)
    mnames = names(preds)[-1]
    preds$testdata = preds$truth + rnorm(nnew,mean = 0, sd = eps)
    
    # test mses
    mses$test <- colMeans((preds[,names(mods)] - preds[,"testdata"])^2)

    mp = reshape2::melt(preds[,-ncol(preds)], id.vars = "x")
    names(mp)[2] <- "model" 

    # make plots
    p1 = ggplot() + theme_bw()
    # points
    p1 = p1 + geom_point(data = r,aes(x = x, y = y),shape = 1,size = 2)
    p1 = p1 +  geom_line(data=mp, aes(x = x, y = value, color = model), size = 1)
    # 
    # color scale
    cnames = c("orange", "blue" , "green","purple","black")
    names(cnames) <- mnames
    p1 = p1 + scale_color_manual(values = cnames)
   
    # flexilibty plot
    d2 = data.frame(flexibility = c(2,df1,df2,df3),model = c("lm",paste0(df1," df"),paste0(df2," df"),paste0(df3," df")), train = mses$train, test = mses$test)
    d2 = reshape2::melt(d2, id.vars = c("flexibility","model"))
    names(d2)[3] <- "type"
    p2 = ggplot(data = d2, aes(x = flexibility, y = value,linetype = type)) + geom_path(color = "black") + scale_y_continuous(name = "MSE")
    cnames = c("orange", "blue" , "green","purple")
    names(cnames) <- c("lm",paste0(df1," df"),paste0(df2," df"),paste0(df3," df"))
    p2 = p2 + geom_point(data = d2, aes(color = model), size = 3, shape = 15) + scale_color_manual(values = cnames) 
    # add minimal test MSE
    p2 = p2 + geom_hline(yintercept = eps^2, linetype = "dashed", color = "grey")
    cowplot::plot_grid(p1,p2)
    # list(p1,p2)
}

print(simdata())

# 
# z <- simdata()
# 
# gp1 <- function(z) {
#     p1 = ggplot(data = z,aes(x = x, y = y)) + theme_bw()
#     # points
#     p1 = p1 + geom_point(shape = 1,size = 2)
#     # lm
#     p1 = p1 + geom_smooth(method = "lm", se = FALSE, aes(color = "lm"))
#     # smooth df=5
#     p1 = p1 + geom_smooth(method = "mgcv::gam", se = FALSE, formula = y ~ ns(x, df = 5), aes(color = "spline 5 df"))
#     # smooth df=5
#     p1 = p1 + geom_smooth(method = "mgcv::gam", se = FALSE, formula = y ~ ns(x, df = 12), aes(color = "spline 12 df"))
#     # p1
#     # truth
#     p1 = p1 + stat_function(data = data.frame(x = c(0,100)),aes(x=x,y = NULL,color = "truth"),fun = tfun, size = 1)
#     
#     # color scale
#     p1 + scale_color_manual(values = c("lm" = "orange", "spline 5 df" = "blue" , "spline 12 df" = "green", "truth" = "black" ))
# }
# 
# 
