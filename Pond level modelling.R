par(mfrow=c(1,1))
X
plot(X,main="Montly water levels ,in feet, in a small pond in rural Hampshire", xlab="Time", ylab= "Water levels (feet)")
summary(X)
sd(X)
## fit  a linear trends
tt= 1:length(X)
fit1 = lm(X~tt)
fit1
trend1 = fitted(fit1)
trend1 = ts(trend1, start=start(X), end=end(X), frequency = frequency((X)))

##superimpose on time series plot
lines(trend1, col="blue")
acf(as.numeric(X), main="")

## Find and plot residuals 
resid1 = ts(residuals(fit1), start = start(X), end = end(X), frequency=frequency(X))

plot(resid1, ylab = "Residuals")


## fit seasonal effect 
## create dummy variables for each month 
n= length(X)
jan = as.numeric((1:n %% 12) == 1)
feb = as.numeric((1:n %% 12) == 2)
mar = as.numeric((1:n %% 12) == 3)
apr = as.numeric((1:n %% 12) == 4)
may = as.numeric((1:n %% 12) == 5)
jun = as.numeric((1:n %% 12) == 6)
jul = as.numeric((1:n %% 12) == 7)
aug = as.numeric((1:n %% 12) == 8)
sep = as.numeric((1:n %% 12) == 9)
oct = as.numeric((1:n %% 12) == 10)
nov = as.numeric((1:n %% 12) == 11)
dec = as.numeric((1:n %% 12) == 0)

##fit model to our residuals using dummy variables
fit4 = lm(X~ 0 + jan + feb + mar + apr + may + jun + jul + aug +
            sep + aug + oct + nov + dec)
fit4
seasonal = ts(fitted(fit4), start=start(X), end=end(X), frequency = frequency(X))
lines(seasonal,col= "blue")
Y= X- seasonal
plot(Y,type= "l", main="Residuals after removing the seasonal effect of X")
seasonal

acf(X)
par(mfrow=c(1,2))
acf(as.numeric(Y), lag=600,main="ACF of Y")
pacf(as.numeric(Y), main="PACF of Y")
par(mfrow=c(3,2))
# fit AR(1) model to time series Y 
AR1 =ar(Y, order= 1, aic=F)
plot(AR1$resid)
# below is the correlogram of residuals for AR(1)
acf(as.numeric(AR1$resid), na.action = na.omit, main= " correlogram of AR1 residuals")

## Now repeat last 2# for AR2 

AR2 =ar(Y, order= 2, aic=F)
AR2
plot(AR2$resid)
Z= AR2$resid
Z
acf(as.numeric(AR2$resid), na.action = na.omit, main= " correlogram of AR2 residuals")
## And same for AR(3)
AR3 =ar(Y, order= 3, aic=F)
plot(AR3$resid)
acf(as.numeric(AR3$resid), na.action = na.omit,main= " correlogram of AR3 residuals")
## this is last bullet point to work out alpha 1 , alpha 2 and alpha 3. the results show alpha 3 is very small in AR(3) and the alpha 2 in AR2 is very similar to alpha 2 
## in AR3 so as alpha 3 is so small we should use AR2 as model as it is a simpler model.
acf(as.numeric(Y), plot=F, lag=3)
acf2AR(acf=c(1, 0.902,0.862,0.815))
par(mfrow=c(3,1))
dft.X = fft(X)/sqrt(n)
I.wn = Mod(dft.X)^2
plot(x=(0:(n/2))/n, y = I.wn[1:(n/2+1)], type="h",
     xlab=expression(f[j]), ylab=expression(I(f[j])), 
     main="raw periodogram of X")

dft.Y = fft(Y)/sqrt(n)
I.Y = Mod(dft.Y)^2
plot(x=(0:(n/2))/n, y = I.Y[1:(n/2+1)], type="h",
     xlab=expression(f[j]), ylab=expression(I(f[j])), 
     main="raw periodogram of Y")



dft.Z = fft(Z[3:600])/sqrt(n)
I.Z = Mod(dft.Z)^2
plot(x=(0:(n/2))/n, y = I.Z[1:(n/2+1)],ylim=c(0,50), type="h",
     xlab=expression(f[j]), ylab=expression(I(f[j])), 
     main="raw periodogram of Z")

