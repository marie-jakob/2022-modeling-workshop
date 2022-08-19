library(WienR)

sample =sampWiener(N=100, a=1, v=1, w=.5, t0=.1, sv=.2, sw=.1, st0=.2)
sample
sample$q
??WienR


data = data.frame(cnd = 1, 
                   resp =ifelse(sample$response == "upper", 1, 0),
                   rt = sample$q)
data

plot(data$rt)
     

ld = WienerPDF(data$rt, data$resp+1, a=1, v=1, w=.5, t0=.1, sv=.2, sw=.1, st0=.2)
typeof(ld)
sum(unlist(ld))



likFull <- function(pars, resp, times, debug = FALSE) {
  a <- pars[1]
  v <- pars[2]
  w <- pars[3]
  t0 <- pars[4]
  sv <- pars[5]
  sw <- pars[6]
  st0 <- pars[7] 
  
  ld <- WienerPDF(t = times, response = resp,
                  a = a, v = v, t0 = t0, w = w,
                  sw = sw, sv = sv, st0 = st0, 
                  n.threads = 4)$logvalue # n.threads sind die Anzahl paralleler Threads
  #    print(ld); print(times[i])
  l <- ifelse(is.finite(ld), -2 * ld, 1.0e100)
  
  l <- sum(l)
  message(l)
  
  return(l)
}

c(a=1, v=1, w=.5, t0=.1, sv=.2, sw=.1, st0=.2)
parameters = c(a=1, v=1, w=.5, t0=.1, sv=.2, sw=.1, st0=.2)

min_0 = 1e-6
max_1 = 0.9999

optim(par = parameters, fn = likFull, times=data$rt, resp=data$resp+1,
      method = c("L-BFGS-B"),
      lower = c(min_0, -Inf, min_0, min_0, min_0, min_0), 
      upper = c(Inf, Inf, Inf, Inf, max_1, Inf, max_1),
      control = list(), hessian = FALSE)

#c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")
