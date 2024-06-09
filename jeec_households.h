/******************************************************************************

        Household object equations
        ----------------------

        Equations that are specific to the households in the Minsky-FIH model.

 ******************************************************************************/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

/*============================== KEY EQUATIONS ===============================*/
EQUATION("Consumption")
/* Real consumption of households */
v[0] = VS(PARENT, "AggregateWageBill");
v[1] = VS(PARENT, "tau1");
v[2] = V("alpha1");
v[3] = VL("HouseholdWealth", 1);
v[4] = V("alpha2");
v[5] = V("ExpectedPrice");
v[6] = ((1 - v[1]) * v[2] * v[0] + v[4] * v[3]) / v[5];
RESULT(v[6])

EQUATION("TotalIncomeHousehold")
/* Sum of wage bill, dividends (from banks and firms), interest on public debt
 */
v[0] = VS(PARENT, "AggregateWageBill");
v[1] = VS(PARENT, "AggregateDividendsFirms");
v[2] = VS(PARENT, "AggregateDividendsBanks");
v[3] = VL("HouseholdBills", 1);
v[4] = V("BaseInterestRate");
v[5] = v[0] + v[1] + v[2] + v[3] * v[4];
RESULT(v[5])

EQUATION("TaxHousehold")
/* Income tax paid by households */
v[0] = V("TotalIncomeHousehold");
v[1] = VS(PARENT, "tau1");
v[2] = v[0] * v[1];
RESULT(v[2])

EQUATION("HouseholdDisposableIncome")
/*Disposable income. Feeds into consumption.*/
v[0] = V("TotalIncomeHousehold");
v[1] = V("TaxHousehold");
RESULT(v[0] - v[1])

EQUATION("HouseholdWealth")
/* Households wealth */
// run after Entry routine (last instance that capital contribution changes).
CYCLES(PARENT, cur, "Sectors") { VS(cur, "Entry"); }
v[0] = VL("HouseholdWealth", 1);
v[1] = V("TotalIncomeHousehold");
v[2] = V("TaxHousehold");
v[3] = V("NominalConsumption");
SUMS(PARENT, "DebtServicing");
v[4] = V("CapitalContribution");
v[5] = v[0] + v[1] - v[2] - v[3] - v[4];
RESULT(v[5])

EQUATION("CapitalContribution")
/* Capital contribution of households to the entry of new firms */
RESULT(0)

EQUATION("HouseholdDeposits")
/* Total households' deposits */
v[0] = V("HouseholdWealth");
v[1] = VS(PARENT, "beta");
v[2] = v[0] * v[1];
RESULT(v[2])
/*============================== SUPPORT EQUATIONS
 * ===============================*/
EQUATION("HouseholdBills");
/*Nominal value of bills held by households.*/
v[0] = V("HouseholdWealth");
v[1] = VS(PARENT, "beta");
v[2] = (1 - v[1]) * v[0];
RESULT(v[2])

EQUATION("NominalConsumption")
/*Nominal consumption of a household.*/
v[0] = VS(PARENT, "PriceLevel");
v[1] = V("Consumption");
v[2] = v[0] * v[1];
RESULT(v[2])
