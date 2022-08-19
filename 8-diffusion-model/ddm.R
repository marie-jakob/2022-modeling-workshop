## Modell Parameter
alpha = 2		# Boundary separation
beta = .5		# Relativer Starting point
delta = 1		# Drift rate
dt = .0005	# Schrittweite (in Sekunden)

## Initialisiere den Pfad
t = 0
x = beta * alpha
path = x

#set.seed(9)
## Generiere den Diffusionsprozess
repeat {
  t = t+1
  x = x+ delta*dt + sqrt(dt)*rnorm(1)
  path = c(path, x)
  if ((x>alpha) || (x<0)) break;
}
## Plotte den Pfad
plot((0:t)*dt, path, type="l", ylim=c(0,alpha), axes=FALSE, xlab="Time", ylab="")
axis(1)
abline(h=c(0,alpha))
abline(v=0)



## Plots für Präsi
#alpha
#set.seed(42)
plot((0:t)*dt, path, type="l", ylim=c(0,alpha), axes=FALSE, xlab="", ylab="", col="blue")
abline(h=c(0,alpha))
abline(v=0)
abline(h=c(0.5, 1.5), col="red",lty=2, lwd=1)


#set.seed(9)
#beta = .3, .5
path1 = path
t1 = t
dt1 = dt
plot((0:t)*dt, path, type="l", ylim=c(0,alpha), axes=FALSE, xlab="", ylab="", col="blue")
lines((0:t1)*dt1, path1, type="l", ylim=c(0,alpha), axes=FALSE, xlab="", ylab="", col="cadetblue")
abline(h=c(0,alpha))
abline(v=0)



