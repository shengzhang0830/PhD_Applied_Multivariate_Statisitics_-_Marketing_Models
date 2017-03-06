data {
     int<lower=1> N;			// nobs
     int<lower=1> J;			// num individuals
     int<lower=1> p;	   	      	// num dyad covariates
     int<lower=1> nr;	   	       	// num random effects
     int<lower=1, upper=J> rowj[N];    	// indicator for row indiv
     int<lower=1, upper=J> colj[N];    	// indicator for col indiv
     int<lower=1> neqn;

     matrix[N,p] xi;
     matrix[N,p] xj;
     matrix[N,neqn] y;
}

parameters {
    vector[p] beta;
    vector[nr] uvec[J];
    real<lower=0> sigma_a;
    real<lower=0> sigma_b;
    real<lower=-1,upper=1> rho_ab;    
    real<lower=0> sigma_e;
    real<lower=-1,upper=1> rho_e;
}

model {
     
      to_vector(beta) ~ normal(0,5);

      sigma_a ~ cauchy(0., 2.5);
      sigma_b ~ cauchy(0., 2.5);
      sigma_e ~ cauchy(0., 2.5);

      rho_ab ~ uniform(-1.0, 1.0);
      rho_e ~ uniform(-1.0, 1.0);      


      {

	matrix [nr, nr] covAB;	
	vector [nr] meanAB;
	 
	covAB[1,1]<-sigma_a^2;
	covAB[2,2]<-sigma_b^2;
	covAB[1,2]<-rho_ab*sigma_a*sigma_b;
	covAB[2,1]<-covAB[1,2];

	meanAB <- rep_vector(0.0, nr);
	
	for (j in 1:J){
	      uvec[j] ~ multi_normal(meanAB, covAB);
	}
      }

      {
	matrix[N,2] xbeta;

	matrix [neqn, neqn] covE;	

	covE[1,1]<-sigma_e^2;
	covE[2,2]<-sigma_e^2;

	covE[1,2]<-rho_e*sigma_e*sigma_e;
	covE[2,1]<-covE[1,2];


      	for(n in 1:N){
	      xbeta[n,1]<-xi[n]*beta+uvec[rowj[n],1]+uvec[colj[n],2];
        xbeta[n,2]<-xj[n]*beta+uvec[colj[n],1]+uvec[rowj[n],2];
	      y[n] ~ multi_normal(xbeta[n], covE);	  
        }

      }
}
	    
   
