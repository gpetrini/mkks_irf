library( sfcr )
library( dplyr )

# Create the system of simultaneous equations using package sfcr
eqs <- sfcr_set(
  #------------------------ ALL SECTORS -------------------------- 
  gss ~ ( 1 + gk ) * (1 + inf) - 1,
  u1 ~ u,                                                         # all sectors' capacity utilisation are equal
  u2 ~ u,
  u3 ~ u,
  nu1 ~ nu,                                                       # all sectors' capital to output ratio are equal
  nu2 ~ nu, 
  nu3 ~ nu,    
  Y ~ u * K / ( nu * ( 1 + gk ) ),                                # total production
  Inv ~ Inv1 + Inv2 + Inv3,                                       # total investment constraint
  A ~ weighted.mean( c( A1, A2, A3 ), c( Y1 + RD1, Y2 + RD2, Y3 + RD3 )  ),         # average productivity
  RD ~ RD2 + RD3,                                                 # total R&D expenditures
  
  #------------ CONSUMPTION GOOD SECTOR -------------------------- 
  S1 ~ Y - Inv - ( INV1 * gk ) / ( 1 + gk ),                      # sales of consumption good sector 1
  INV1 ~ iota * S1,                                               # inventories, sector 1
  Y1 ~ S1 + ( INV1 * gk ) / ( 1 + gk ),                           # output of sector 1
  K1 ~ nu1 * Y1 * ( 1 + gk ) / ( u1 ) ,                           # capital stock
  Inv1 ~ K1 - (1 - delta ) * K1 / ( 1 + gk ),                     # law of motion of capital, finds investment
  A1 ~  ( u1 * K1 ) / ( nu1 * NF1 * ( 1 + gk ) ),                 # labour productivity, sector 1
  DepK1 ~ ( delta * K1 ) / ( 1 + gk ),                            # real capital depreciation, sector 1

  #--------------- CAPITAL GOOD SECTORS --------------------------
  S3 ~ Inv2,                                                      # investment is the sum of sales of both capital goods producing sectors
  S2 ~ Inv1 + Inv3,                                               # sales of sector 2
  Y2 ~ S2,                                                        # output of sector 2
  Y3 ~ S3,                                                        # output of sector 3
  Inv2 ~ K2 - ( 1 - delta ) * K2 / ( 1 + gk ),                    # investment sector 2, from Ks law of motion
  Inv3 ~ K3 - ( 1 - delta ) * K3 / ( 1 + gk ),                    # investment sector 2, from Ks law of motion
  K2 ~ nu2 * Y2 * ( 1 + gk ) / ( u2 ),                            # capital stock
  K3 ~ nu3 * Y3 * ( 1 + gk ) / ( u3 ),                            # capital stock
  RD1 ~ 0,
  RD2 ~ gamma * S2 / ( 1 + gk ),
  RD3 ~ gamma * S3 / ( 1 + gk ),
  A3 ~ A1,                                                        # sectors 1 and 2 buy equipment from sector 2
  NF1 ~ N * K1 / ( K1 + K2 + K3 ),                                # number of workers sector 1
  NF3 ~ u3 * K3 / ( nu3 * A3 * ( 1 + gk ) ) + RD3 / A3,           # number of workers sector 3
  NF2 ~ N - NF1 - NF3,                                            # number of workers hired by sector 2
  A2 ~  ( u2 * K2 ) / ( nu2 * NF2 * ( 1 + gk ) ),                 # labour productivity, sector 1
 
  #--- RELATIVE PRICES AND DISTRIBUTION --------------------------
  theta1 ~ theta,
  theta2 ~ theta,
  theta3 ~ theta,
  p1 ~ ( 1 + theta1 ) * w / A1,
  p2 ~ ( 1 + theta2 ) * w / A2,
  p3 ~ ( 1 + theta3 ) * w / A3,
  
  #-----------FINANCIAL VARIABLES FIRMS -------------------------- 
  L1 ~ lev * Assets1,                                             # outstanding debt sectors
  L2 ~ lev * Assets2,
  L3 ~ lev * Assets3,
  sumAm ~ sum( ( 1 / ( 1 + gss ) ) ^ ( 1 : lambda ) ),
  ra ~ ( gss * sumAm ) / ( lambda - sumAm ),
  AM1 ~ ( gss + ra ) / ( lambda * (1 + gss) ) * L1 * sumAm,
  AM2 ~ ( gss + ra ) / ( lambda * (1 + gss) ) * L2 * sumAm,
  AM3 ~ ( gss + ra ) / ( lambda * (1 + gss) ) * L3 * sumAm,
  Assets1 ~ Df1 + p2 * K1 + p1 * INV1 / ( 1 + theta1 ),
  Assets2 ~ Df2 + p3 * K2, 
  Assets3 ~ Df3 + p2 * K3, 
  Df1 ~ ( p2 * Inv1 + w * NF1 - ( L1 * gss ) / ( 1 + gss ) - AM1 ) * ( 1 + gss ),               # Dynamics of debt, defines the amount of deposits held
  Df2 ~ ( p3 * Inv2 + w * NF2 - ( L2 * gss ) / ( 1 + gss ) - AM2 ) * ( 1 + gss ),               # Dynamics of debt, defines the amount of deposits held
  Df3 ~ ( p2 * Inv3 + w * NF3 - ( L3 * gss ) / ( 1 + gss ) - AM3 ) * ( 1 + gss ),
  PiG1 ~ p1 * S1 - w * NF1 - ibk * L1 / ( 1 + gss ),              # nominal profits of sectors
  PiG2 ~ p2 * S2 - w * NF2 - ibk * L2 / ( 1 + gss ),
  PiG3 ~ p3 * S3 - w * NF3 - ibk * L3 / ( 1 + gss ),
  DepKN1 ~ ( nk * p2 * K1 * red  ) / ( 1 + gss ),                 # nominal depreciation of sectors
  DepKN2 ~ ( nk * p3 * K2 * red  ) / ( 1 + gss ),
  DepKN3 ~ ( nk * p2 * K3 * red  ) / ( 1 + gss ),
  PiN1 ~ PiG1 + ( gss * ( w / A1 ) * INV1 ) / (1 + gss) - DepKN1, # net profit of sectors
  PiN2 ~ PiG2 - DepKN2, 
  PiN3 ~ PiG3 - DepKN3,
  PiDf1 ~ gss * ( L1 - Df1 ) / ( 1 + gss ) + PiG1 - tau2 * PiN1 - p2 * Inv1, # dividends distributed by sectors  
  PiDf2 ~ gss * ( L2 - Df2 ) / ( 1 + gss ) + PiG2 - tau2 * PiN2 - p3 * Inv2,
  PiDf3 ~ gss * ( L3 - Df3 ) / ( 1 + gss ) + PiG3 - tau2 * PiN3 - p2 * Inv3,
  etaf1 ~ PiDf1 / ( ( 1 - tau2 ) * PiN1  ),                       # dividend payout ratio sectors
  etaf2 ~ PiDf2 / ( ( 1 - tau2 ) * PiN2  ),
  etaf3 ~ PiDf3 / ( ( 1 - tau2 ) * PiN3  ),
  P ~ weighted.mean( c( p1, p2, p3 ), c( S1, S2, S3 ) ),
  
  #---------BANKING SECTOR VARIABLES -------------------------- 
  ibk ~ i + spread,
  L ~ L1 + L2 + L3,
  PiB ~ ( ibk * L + i * Bb ) / ( 1 + gss ),
  PiDB ~ etab * ( 1 - tau2 ) * PiB,
  NwB ~ ( ( 1 - tau2 ) * PiB - PiDB ) * ( 1 + gss ) / gss,
  B ~ gDebtGDP * P * Y,
  Bh ~ V - Dh,
  Bb ~ B - Bh,
  Dh ~ beta * V,
  tau1 ~ ( p1 * G - tau2 * ( PiB + PiN1 + PiN2 + PiN3 ) - B * ( gss - i ) / ( 1 + gss ) ) / ( w * N + PiDf1 + PiDf2 + PiDf3 + PiDB + i * Bh / ( 1 + gss ) ),
  V ~ ( ( 1 - alpha1 ) * ( 1 - tau1 ) * w * N + ( 1 - tau1 ) * ( PiDf1 + PiDf2 + PiDf3 + PiDB + i * Bh / ( 1 + gss ) ) ) * ( 1 + gss ) / ( gss + alpha2 ),
  alpha2 ~ ( p1 * C - alpha1 * ( 1 - tau1 ) * w * N ) * ( 1 + gss ) / V,
  C ~ S1 - G,
  G ~ ( Gamma0 * ( K1 + K2 + K3 ) ) / ( 1 + gk )
)


# Input data from LSD initial pre-SS parameters
external <- sfcr_set(
  gk ~ 0.02,
  inf ~ 0.01,
  u ~ 0.8,
  K ~ 100000,
  nu ~ 1.5,
  iota ~ 0.2,
  delta ~ 0.07,
  lev ~ 0.3,
  N ~ 10000,
  gamma ~ 0.03,
  w ~ 1,
  theta ~ 0.4,
  lambda ~ 10,
  i ~ 0.025,
  spread ~ 0.03,
  nk ~ 0.068,
  red ~ 0.958,
  tau2 ~ 0.35,
  etab ~ 0.9,
  gDebtGDP ~ 0.5,
  beta ~ 0.2,
  alpha1 ~ 1,
  Gamma0 ~ 0.17,
  )

# Solve model using Gauss' algorithm
sim <- sfcr_baseline(
  equations = eqs, 
  external = external,
  periods = 2,
  tol = 1e-15,
  method = 'Gauss'
)
