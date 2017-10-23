#-----------------------------------------------------------------------------
# Option pricing from characteristic function
# Equation (5.6) of The Volatility Surface
# Implements the control variate methodology of Gauthier and Possamai (2010)

callOptionPhi <- function(phi, k, t){
    integrandATM <-  function(u){Re(phi(u - 1i/2, t)/(u^2 + 1/4))};
    integralATM  <- integrate(integrandATM,lower=0,upper=Inf,rel.tol=0.0001,subdivisions=10)$value;
    sqrtW <- sqrt(2*pi)*(1-1/pi*integralATM); # Estimate volatility of ATM option
    w <- sqrtW*sqrtW;
    phiBS <- function(u){exp(-1/2*(u^2+1/4)*w)};
    integrand <-  function(u){Re(exp(-1i*u*k)*(phi(u - 1i/2, t)-phiBS(u))/(u^2 + 1/4))};
    integral <- exp(k/2)/pi*integrate(integrand,lower=0,upper=Inf,rel.tol=0.0000000001,subdivisions=1000)$value;
    cBS <- BSFormula(1, exp(k), t, r=0, sigma=sqrtW/sqrt(t));
    res <- cBS - integral;
    return(res);
}

putOptionPhi <- function(phi, k, t){
    integrandATM <-  function(u){Re(phi(u - 1i/2, t)/(u^2 + 1/4))};
    integralATM  <- integrate(integrandATM,lower=0,upper=Inf,rel.tol=0.0001,subdivisions=10)$value;
    sqrtW <- sqrt(2*pi)*(1-1/pi*integralATM); # Estimate volatility of ATM option
    w <- sqrtW*sqrtW;
    phiBS <- function(u){exp(-1/2*(u^2+1/4)*w)};
    integrand <-  function(u){Re(exp(-1i*u*k)*(phi(u - 1i/2, t)-phiBS(u))/(u^2 + 1/4))};
    integral <- exp(k/2)/pi*integrate(integrand,lower=0,upper=Inf,rel.tol=0.0000000001,subdivisions=1000)$value;
    pBS <- BSFormulaPut(1, exp(k), t, r=0, sigma=sqrtW/sqrt(t));
    res <- pBS - integral;
    return(res);
}


bsvol <- function(phi,k, t){
  ifelse(k>=0,
  BSImpliedVolCall(1, exp(k), t, 0, callOptionPhi(phi,k,t)),
  BSImpliedVolPut(1, exp(k), t, 0, putOptionPhi(phi,k,t)))};
        

