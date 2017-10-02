
#-----------------------------------------------------------------------------
# The Heston formula

callHeston <- function(params){
    
    lambda <- params$lambda;
    rho <- params$rho;
    eta <- params$eta;
    vbar <- params$vbar;
    v <- params$v;
    
    res <- function(k,t){
        fj <- function(u, t, j){

            al <- -u*u/2 - 1i*u/2 + 1i*j*u;
            bet <- lambda - rho*eta*1i*u-rho*eta*j;
            gam <- eta^2/2;
            d <- sqrt(bet*bet - 4*al*gam);
            rp <- (bet + d)/(2*gam);
            rm <- (bet - d)/(2*gam);
            g <- rm / rp;
            D <- rm * (1 - exp(-d*t))/ (1 - g*exp(-d*t));
            C <- lambda * (rm * t - 2/eta^2 * log( (1 - g*exp(-(d*t)))/(1 - g) ) );
            return(exp(C*vbar + D*v));} 
        x <- -k;
        p1 <- 1/2+1/pi*integrate(function(u){Re(fj(u,t,1)*exp(1i*u*x)/(1i*u))},lower=0,upper=Inf)$value;
        p0 <- 1/2+1/pi*integrate(function(u){Re(fj(u,t,0)*exp(1i*u*x)/(1i*u))},lower=0,upper=Inf)$value;
        return(exp(k)*(exp(x)*p1-p0));
    }
    return(res);# Return price of Heston call expressed as percentage of spot.
}

#-----------------------------------------------------------------------------
# Function to generate implied vols. corresponding to Heston model

impvolHestonRaw <- function(params){
    function(k, t){
        BSImpliedVolCall(1, exp(k), t, 0, callHeston(params)(k,t))
        }
    }

# Vectorize this function (over k)
impvolHeston <- function(params){
  function(k,t){sapply(k,function(k){impvolHestonRaw(params)(k,t)})}
}


#callHeston(subBCC)(0,1)
#impvolHeston(subBCC)(0,1)
