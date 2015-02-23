WEEK 2: HOMEWORKS

#HOMEWORK 3

# q1


g = 9.8 ## meters per second

h0 = 56.67

v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function

y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)

X = cbind(1,tt,tt^2)

A = solve(crossprod(X))%*%t(X)

print(-2*(A %*% y)[3])  


# q2:   0.43

B = 100000


g = 9.8 ## meters per second


n = 25


tt = seq(0,3.4,len=n) ##time in secs, t is a base function


X = cbind(1,tt,tt^2)


A = solve(crossprod(X))%*%t(X)


betahat = replicate(B,{


y = 56.67 - 0.5*g*tt^2 + rnorm(n,sd=1)


betahats = -2*A%*%y


return(betahats[3])


})


sqrt(mean( (betahat-mean(betahat) )^2))


# q3: 0.124

library(UsingR)

n = length(y)

N = 50

B=10000

index = sample(n,N)

sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

betahat = replicate(B,{

index = sample(n,N)

sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

lm(y~x)$coef[2]

})

sqrt ( mean( (betahat - mean(betahat) )^2 ))


# q4: and using previous code = 4

Y=father.son$fheight

X=father.son$sheight


# q5: using codes from question 3 = 256.2152

fit = lm(y ~ x)

fit$fitted.values

mean( (Y - mean(Y))*(X-mean(X) ) )


# q6: using codes from question 3 = 11.302749

X = cbind(rep(1,N), x)

solve(t(X) %*% X)


# q7:  using codes from question 3 = 0.1141966

fit = lm(y ~ x)

sigma2 = sum((y - fit$fitted.values)^2) / (N - 2)

sqrt(sigma2 * diag(solve(t(X) %*% X)))

# Note that the standard error estimate is also printed in the second column of:

summary(fit)


# q8 :   ~ day + condition 


# q9: 12

SE = sqrt(var(diff))

var(diff) = (1/nx + 1/ny) ( sum { x_i - mu_x } + sum { y_i - mu_y } ) / (nx + ny - 2) 

X = cbind(rep(1,nx + ny),rep(c(0,1),c(nx, ny)))



XtX = t(X) %*% 

X

Xtx[ 1,1 ]


#q10: 7

t(X)%*%X

#HOMEWORK 4

# Q1 : 0 1 -1

# Q2: 2.45

# Use the lines of code from the Rmd script up to the definition of fitTL

contrast(fitTL,list(leg="L4",type="pull"),list(leg="L2",type="pull"))

# Q3: use codes from previous question 0.0006389179

Sigma[3,5]

# Q4: -3.689 use codes from question 2  

fit = lm(log2friction ~ type + leg + type:leg, data=spider)

summary(fit)

# and read off the t-value for typepush:legL4

# q5: 10.701 use codes from question 2

fit = lm(log2friction ~ type + leg + type:leg, data=spider)

anova(fit)

# and read off the F-value for the type:leg row

# q6: 0.3468125  use codes from question 2

contrast(fit, list(type="pull",leg="L2"), list(type="pull",leg="L1"))

# q7: 0.4464843 use codes from question 2

contrast(fit, list(type="push",leg="L2"), list(type="push",leg="L1"))

# q8: 1.058824

Fs = replicate(1000, {

Y = rnorm(N,mean=42,7)

mu0 = mean(Y)

initial.ss = sum((Y - mu0)^2)

s = split(Y, group)

after.group.ss = sum(sapply(s, function(x) sum((x - mean(x))^2)))

(group.ss = initial.ss - after.group.ss)

group.ms = group.ss / (p - 1)

after.group.ms = after.group.ss / (N - p)

f.value = group.ms / after.group.ms

return(f.value)

})

mean(Fs)

#HOMEWORK 5

# Q1 E

m = matrix(c(1,1,1,1,0,0,1,1,0,1,0,1,0,0,0,1),4,4)
qr(m)$rank

# Q2 11 use pseudocode from question
fitTheRest(1,2)

# q3 2  use codes from previous question
outer(-2:8,-2:8,Vectorize(fitTheRest))
# These capture the best fitted Y values: 1.5, 1.5, 3.5. 3.5, 5.5, 5.5, 7.5, 7.5.

# The residuals are all equal to 0.5, and the sum of squared residuals will be:

# 8 * 0.5^2 = 2

# q4 -0.05954913
fit <- lm(friction ~ type + leg, data=spider)
betahat <- coef(fit)
Y <- matrix(spider$friction, ncol=1)
X <- model.matrix(~ type + leg, data=spider)
R beta = Q^T*Y

Q = qr.Q(qr(X))

# q5 -16.79286
R = qr.R(qr(X))

# q6 -13.79872520
head(crossprod(Q, Y))
