/******************************************************************************

        Equations related to the labour market.

 ******************************************************************************/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

/*============================== KEY EQUATIONS ===============================*/
EQUATION("DesiredDemandLabour")
/*
Demand for labour considering the desired production.
Update if funds are insufficient to pay the desired number of workers.
Level: firm.
*/
// production purposes
v[0] = V("DesiredCapacityUtilisation");
v[1] = min(v[0], 1); // Bound capacity utilisation to 1
v[2] = max(v[1], 0); // Bound capacity utilisation >= 0
v[3] = V("AvailableCapitalStock");
v[4] = VS(PARENT, "nu");
v[5] = V("ExAnteAvgLabourProductivity");
v[6] = v[2] * v[3] / (v[4] * v[5]); // Labour demand with production purpose

// R&D purposes
v[7] = V("WorkersRD"); // Labour demand with R&D purpose

// total
v[8] = v[6] + v[7];
RESULT(v[8])

EQUATION("EffectiveDemandLabour")
/*
Updates the desired effective demand for labour, after the credit market
interaction. Firms evaluate whether the funds are sufficient to produce the
desired quantity. Level: firm.
*/
v[0] = V("CreditSupply");                  // credit supply
v[1] = V("DemandLoans");                   // demand for credit
v[2] = V("DesiredDemandLabour");           // demand for labour
v[3] = V("NominalWage");                   // nominal wage
v[4] = V("FirmDeposits_PostCreditUpdate"); // deposits available at the moment
v[5] = v[2] * v[3]; // wage bill considering desired labour

if (v[4] >=
    v[5]) // are the deposits sufficient for hiring the desired amount of L?
{         // yes

  END_EQUATION(v[2]);
} else // no
{
  v[7] = v[5] - v[4]; // haircut of desired payroll (total desired wage bill -
                      // funds available)
  v[8] = v[7] / v[3]; // quantity of workers to be reduced
  v[9] = v[2] - v[8]; // new effective demand for workers
}
RESULT(v[9])

/*============================== SUPPORT EQUATIONS
 * ===============================*/
EQUATION("WorkersFirm")
/* Total number of workers of a firm.*/
RESULT(V("EffectiveDemandLabour"))

EQUATION("WorkersRD")
/*
Number of workers employed in the R&D sector. Applies to capital goods producing
sectors. The Number of R&D workers is a function of nominal sales (R&D
expenditure), deflated by the nominal wage. Level: firm.
*/
v[0] = V("RDExpenditure");
v[1] = V("NominalWage");
v[2] = v[0] / v[1];
RESULT(v[2])

EQUATION("ProductionWorkers")
/*
Number of workers in production equals total number of workers the firm hired in
the labour market, less the n_workers allocated in R&D. Level: firm.
*/
v[0] = V("WorkersFirm");
v[1] = V("WorkersRD");
if (v[1] > v[0]) // if the number of R&D workers is superior to the number of
                 // firm's workers, there was severe rationing. All workers goes
                 // to production.
{
  WRITE("WorkersRD", 0);
  END_EQUATION(v[0]);
}
v[2] = v[0] - v[1];
RESULT(v[2])

EQUATION("NominalWage")
/*
Nominal wage agreed between workers and firms.
Level: country.
*/
v[0] = VL("NominalWage", 1);
v[1] = V("psi1");
v[2] = VL("Inflation", 1);
v[3] = V("psi2");
v[4] = VL("AggregateGrowthProductivity", 1);
v[5] = V("StochasticComponentGrowthNominalWage");
v[6] = v[0] * (1 + v[1] * v[2] + v[3] * v[4] + v[5]);
RESULT(v[6])

EQUATION("GrowthRateEmployment")
/*
Pass growth rate of employment to the storage container.
Level: country.
*/
v[0] = V("TotalLabour") / VL("TotalLabour", 1) - 1;
growthEmployment.push_back(v[0]);
RESULT(v[0])

EQUATION("UpperBinaryEmploymentGrowth")
/*
Bands of employment growth (top).
Level: country.
*/
if (t < 2) // only works when there is a minimum amount of information in vector
           // growthEmployment
  v[2] = 1;
else {
  v[0] = ntile_vector(growthEmployment, V("bandT")); // get bandT %tile
  v[1] = VL("GrowthRateEmployment", 1);
  v[2] =
      v[1] > v[0] ? 1 : 0; // binary variable indicating whether last period's
                           // employment growth is greater than bandT %tile
}
RESULT(v[2])

EQUATION("BottomBinaryEmploymentGrowth")
/*
Bands of employment growth (bottom).
Level: country.
*/
if (t < 2) // only works when there is a minimum amount of information in vector
           // growthEmployment
  v[2] = 0;
else {
  v[0] = ntile_vector(growthEmployment, V("bandB")); // get bandB %tile
  v[1] = VL("GrowthRateEmployment", 1);
  v[2] =
      v[1] < v[0] ? -1 : 0; // binary variable indicating whether last period's
                            // employment growth is lower than bandB %tile
}
RESULT(v[2])

EQUATION("StochasticComponentGrowthNominalWage")
/*
Folded normal growth of real wage if above or below bands.
Level: country.
*/
v[0] = V("UpperBinaryEmploymentGrowth");
v[1] = V("muFN");
v[2] = V("sigmaFN");
v[3] = V("BottomBinaryEmploymentGrowth");
v[4] = abs(norm(v[1], v[2]));
v[5] = abs(norm(v[1], v[2]));
v[6] = v[0] * v[4];
v[7] = v[3] * v[5];
v[8] = v[6] + v[7];
RESULT(v[8])

EQUATION("GrowthNominalWage")
/*
Growth of nominal wage.
Level: country.
*/
v[0] = V("NominalWage") / VL("NominalWage", 1) - 1;
RESULT(v[0])

EQUATION("UnitsOfLabour")
/*
Units of labour employed in the economy.
Level: country.
*/
RESULT(SUM("EffectiveDemandLabour"))
