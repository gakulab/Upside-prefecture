function(b,phi,g,f_intervention1,f_intervention2,f_nonintervention)
{
  b_next = b + ((phi + 1) / phi ) * g * b * (1 -  (b ^ phi)  / (phi + 1)) - g * f_intervention1 * b - g * f_intervention2 * b - g * f_nonintervention * b
  bmax = (phi+1)^(1/phi) - 0.1
  return(max(0,min(bmax,b_next)))
}
