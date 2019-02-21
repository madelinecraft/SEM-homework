lambda <- matrix(c(.7, .7, .7, 0, 0, 0, 0, 0, 0, .7, .7, .7), nrow=6)
lambda
theta <- diag(.51, nrow=6, ncol=6)
theta
beta <- matrix(c(0, .4, 0, 0), nrow=2)
beta
psi <- matrix(c(1.5, 0, 0, .9), nrow=2)
psi
identity <- matrix(c(1, 0, 0, 1), nrow=2)
identity
require('matlib')
sumsigma <- lambda%*%inv(identity-beta)%*%psi%*%inv(identity-t(beta))%*%t(lambda)+theta
sumsigma
.7*.4*1.5*.7
