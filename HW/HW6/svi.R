# The SVI Parametrization
svi <- function(sviparams,k){
a <- sviparams$a;
b <- sviparams$b;
sig <- sviparams$sig;
rho <- sviparams$rho;
m <- sviparams$m;
return(a + b *(rho*(k-m)+ sqrt((k-m)*(k-m) + sig*sig)));
}
