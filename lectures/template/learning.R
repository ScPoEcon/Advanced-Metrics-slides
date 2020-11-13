rm(list=ls())
library(ggplot2)
library(splines)
library(dplyr)
# 
# tfun <- function(x) {
#     x + 0.1 * x^2 - 0.002 * x^3 + 0.00001 *x^4
# }

# sin(x-2) + x-0.05*x^2

getmodels <- function(x,y,newx,dfs = 2:20){
    r = data.frame(x=x,y=y)
    o = data.frame(x=newx)
    s = list()
    # browser()
    for (i in 1:length(dfs)){
        if (dfs[i] == 2){
            s[[i]] <- lm(y~x,r)
            o = cbind(o, predict(s[[i]], newdata = o))
        } else {
            s[[i]] <- smooth.spline(x,y,df = dfs[i])
            o = cbind(o, predict(s[[i]], o$x)$y)
        }
    }
    names(o)[-c(1)] <- paste0("df",dfs)
    names(s) <- paste0("df",dfs)
    list(models = s, pred = o)
}

datafig2.12 <- function(fun = function(x) {x*sin(x-2) + 0.2*x},n=90,eps = 1,df1=5, df2=40, ub = 5,nnew = 200){
    set.seed(1234)
    
    r = data.frame(x = seq(0,ub,length.out = n))
    r$truth = fun(r$x) 
    r$epsi = rnorm(n,mean = 0, sd = eps)
    r$y = r$truth + r$epsi
    # browser()

    mods = getmodels(r$x,r$y,seq(0,ub, length.out = nnew))
    # add test data to predictions
    mods$pred$truth = fun(mods$pred$x)
    mods$pred$testdata = mods$pred$truth + rnorm(nnew,mean = 0, sd = eps)
    # mses and bias
    mses = list(
        train = colMeans(sapply(mods$models,residuals)^2)
    ) # test mses
    mses$test <- colMeans((mods$pred[,names(mods$models)] - mods$pred[,"testdata"])^2)
    
    # bias
    mses$bias <- colMeans((mods$pred[,names(mods$models)] - mods$pred[,"truth"])^2)
    mses$var <- diag(var(mods$pred[,names(mods$models)]))
    list(mods,mses)
    
}
x = datafig2.12()

plotfig2.12 <- function(d) {
    stopifnot(is.list(d))
    m = data.frame(d)
    m$x = 2:(nrow(m)+1)
    m = reshape2::melt(m,id.vars = "x")
    m %>% 
        rename(model = variable) %>%
        ggplot(aes(x=x,y = value, color = model)) + geom_point()
        
}
plot(plotfig2.12(x[[2]]))

smes1 <- function(fun = function(x) {x*sin(x-2) + 0.2*x},n=90,eps = 1,df1=5, df2=40, ub = 5,nnew = 200){
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
    # mods$ns5 = gam::gam(formula = as.formula(paste0("y ~ s(x,",df1,")")),data = r)
    # mods$ns5 = mgcv::gam(formula = as.formula(paste0("y ~ ns(x,df = ",df1,")")),data = r)
    # mods$ns20 = gam::gam(formula = as.formula(paste0("y ~ ns(x,df = ",df2,")")),data = r)
    # mods$ns20 = mgcv::gam(formula = as.formula(paste0("y ~ ns(x,df = ",df2,")")),data = r)
    
    names(mods)[2:3] <- paste0("s",c(df1,df2))
    
    
    # mses and bias
    mses = list(
        train = colMeans(sapply(mods,residuals)^2)
    )
    
    # predictions
    preds = data.frame(x = seq(0,ub, length.out = nnew))
    preds = cbind(preds, lm = predict(mods$lm,newdata = preds))
    preds = cbind(preds,sapply(mods[2:3],function(x) predict(x,preds$x)$y))
    preds$truth = fun(preds$x) #+ rnorm(nnew,mean = 0, sd = eps)
    mnames = names(preds)[-1]
    preds$testdata = preds$truth + rnorm(nnew,mean = 0, sd = eps)
    
    # test mses
    mses$test <- colMeans((preds[,names(mods)] - preds[,"testdata"])^2)
    
    # bias
    mses$bias <- colMeans((preds[,names(mods)] - preds[,"truth"])^2)
    mses$var <- diag(var(preds[,names(mods)]))

    mp = reshape2::melt(preds[,-ncol(preds)], id.vars = "x")
    names(mp)[2] <- "model" 
    
    # make plots
    p1 = ggplot() + theme_bw()
    # points
    p1 = p1 + geom_point(data = r,aes(x = x, y = y),shape = 1,size = 2)
    p1 = p1 +  geom_line(data=mp, aes(x = x, y = value, color = model), size = 1)
    # 
    # color scale
    cnames = c("orange", "blue" , "green","black")
    names(cnames) <- mnames
    p1 = p1 + scale_color_manual(values = cnames)
    
    # flexilibty plot
    d2 = data.frame(flexibility = c(2,df1,df2),model = c("lm",paste0(df1," df"),paste0(df2," df")), train = mses$train, test = mses$test)
    d2 = reshape2::melt(d2, id.vars = c("flexibility","model"))
    names(d2)[3] <- "type"
    p2 = ggplot(data = d2, aes(x = flexibility, y = value,linetype = type)) + geom_path(color = "black") + scale_y_continuous(name = "MSE")
    cnames = c("orange", "blue" , "green")
    names(cnames) <- c("lm",paste0(df1," df"),paste0(df2," df"))
    p2 = p2 + geom_point(data = d2, aes(color = model), size = 3, shape = 15) + scale_color_manual(values = cnames) 
    # add minimal test MSE
    p2 = p2 + geom_hline(yintercept = eps^2, linetype = "dashed", color = "grey")+ theme_bw()
    cowplot::plot_grid(p1,p2)
    # list(p1,p2)
}

smes2 <- function(fun = function(x) {x*sin(x-2) + 0.2*x},n=90,eps = 1,df1=5, df2=20, df3=60, ub = 5,nnew = 200){
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
    p2 = p2 + geom_point(data = d2, aes(color = model), size = 3, shape = 15) + scale_color_manual(values = cnames) + theme_bw()
    # add minimal test MSE
    p2 = p2 + geom_hline(yintercept = eps^2, linetype = "dashed", color = "grey")
    cowplot::plot_grid(p1,p2)
    # list(p1,p2)
}

# running
# figure 2.9
# print(smes1())
# figure 2.10
# print(smes1(fun = function(x){x^1.2}))
# figure 2.11
# print(smes1(fun = function(x){4*x*sin(x) - 0.5*x^2}))

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
