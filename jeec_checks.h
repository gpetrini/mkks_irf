
#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

EQUATION("CheckMarketShare")
V("RevisionSales");
v[0] = MIN("MarketShare");
if (v[0] < -1e-10)
  PLOG("\nThere is a negative market share. Check model.");
v[1] = SUM("MarketShare");
if (abs(v[1] - 1) > VS(root, "tol") && VS(root, "flagChecks"))
  PLOG("\nTotal market share is not 100%! Check.");
RESULT(SUM("MarketShare"))

EQUATION("CheckInventory")
v[0] = SUM("Sales");
v[1] = SUM("Production");
v[2] = SUML("Inventory", 1);
v[3] = V("AggregateInventories");
v[4] = v[3] - (v[2] + v[1] - v[0]);
v[5] = V("AggregateCapitalStock");
if (abs(v[4] / v[5]) > VS(root, "tol"))
  plog("\nCheck of inventory accounting indicate some possible leakage. Please "
       "check.");
RESULT(v[4])

EQUATION("CheckSupplyDemand")
v[0] = V("AggregateDemand");
v[1] = V("AggregateSupply");
v[2] = v[1] - v[0];
RESULT(v[2])

EQUATION("CheckCapitalStock")
/* Check whether capital stock accounting is correct.*/
v[0] = V("AggregateCapitalStock");
v[1] = V("AggregateInvestment");
v[2] = VL("AggregateCapitalStock", 1);
v[3] = V("AggregateDepreciation");
v[4] = v[0] - v[2] - v[1] + v[3];
if (abs(v[4] / v[0]) > VS(root, "tol") && VS(root, "flagChecks")) {
  plog("\nThere is possibly an accounting error in capital stock dynamics. "
       "Check causes. The error is equivalent to ");
  PLOG("%f", round_digits(v[4] / v[0] * 100, 2));
  plog(" percent of the aggregate capital stock.");
}
RESULT(v[4])

EQUATION("CheckFirmsDebt")
/* Check accounting of firms' debt.*/
v[0] = V("AggregateNewLoans");
v[1] = VL("AggregateDebtFirms", 1);
v[2] = V("AggregateDebtAmortisation");
v[3] = V("AggregateDebtFirms");
v[4] = V("AggregateNonPerformingLoans");
v[5] = v[3] - v[1] + v[2] - v[0] + v[4];
if (abs(v[5] / v[3]) > VS(root, "tol") && VS(root, "flagChecks")) {
  if (v[3] != 0) {
    plog("\nThere is possibly an accounting error in firms' debt dynamics. "
         "Check causes. The error is equivalent to ");
    PLOG("%f", round_digits(v[5] / v[3] * 100, 2));
    plog(" percent of the aggregate firms' debt.");
  }
}
RESULT(v[5])

EQUATION("CheckOutputDemandSupply")
/* Check if GDP accounting is correct */
v[0] = V("RealGDP_Demand");
v[1] = V("AggregateProduction");
v[2] = v[0] - v[1];
if (abs(v[2] / v[0]) > VS(root, "tol") && VS(root, "flagChecks")) {
  PLOG("\nProblem with GDP accounting!");
}
RESULT(v[2])

EQUATION("CheckBudgetConstraintFirmsSector")
/*Check if firms' sector budget constraint correctly holds.*/
V("Entry");
v[0] = V("FinancialSavingsFirmsSector");
v[1] = V("ChangeDepositsFirmsSector");
v[2] = V("ChangeDebtFirmsSector");
v[3] = V("NominalGDP_Demand");
v[4] = V("AggregateNonPerformingLoans");
v[5] = SUM("CapitalContribution");
v[6] = v[0] - v[1] + v[2] + v[4] + v[5];
if (abs(v[6] / v[3]) > VS(root, "tol") && VS(root, "flagChecks")) {
  PLOG("\nFirms' sector budget constraint does not close!");
}
RESULT(v[6])

EQUATION("CheckBudgetConstraintHouseholdsSector")
v[0] = V("FinancialSavingsHouseholdsSector");
v[1] = V("ChangeDepositsHouseholdsSector");
v[2] = V("ChangeBillsHouseholdsSector");
v[3] = V("NominalGDP_Demand");
v[4] = SUM("CapitalContribution");
v[5] = v[0] - v[1] - v[2] - v[4];
if (abs(v[5] / v[3]) > VS(root, "tol") && VS(root, "flagChecks"))
  PLOG("\nHouseholds' sector budget constraint does not close!");
RESULT(v[5])

EQUATION("CheckBudgetConstraintGovernmentSector")
v[0] = V("FinancialSavingsGovermentSector");
v[1] = V("ChangeGovernmentDebt");
v[2] = V("NominalGDP_Demand");
v[3] = v[0] + v[1];
if (abs(v[3] / v[2]) > VS(root, "tol") && VS(root, "flagChecks"))
  PLOG("\nGovernments' sector budget constraint does not close!");
RESULT(v[3])

EQUATION("CheckBudgetConstraintBanksSector")
v[0] = V("FinancialSavingsBankingSector");
v[1] = V("ChangeBillsBanksSector");
v[2] = V("ChangeLoanPortfolioBanksSector");
v[3] = V("ChangeDepositsBanksSector");
v[4] = SUM("NonPerformingLoans");
v[5] = V("NominalGDP_Demand");
v[6] = v[0] - v[1] - v[2] + v[3] - v[4];
if (abs(v[6] / v[5]) > VS(root, "tol") && VS(root, "flagChecks"))
  PLOG("\nBanks' sector budget constraint does not close!");
RESULT(v[6])

EQUATION("CheckFinancialSavings")
v[0] = V("FinancialSavingsFirmsSector_GDP");
v[1] = V("FinancialSavingsHouseholds_GDP");
v[2] = V("FinancialSavingsGovermentSector_GDP");
v[3] = V("FinancialSavingsBankingSector_GDP");
v[4] = v[0] + v[1] + v[2] + v[3];
if (abs(v[4]) > VS(root, "tol") && VS(root, "flagChecks"))
  PLOG("\nFinancial balances do not sum zero!");
RESULT(v[4])

EQUATION("CheckNominalSalesDemand")
v[0] = V("Agg_NominalSales");
v[1] = SUM("NominalConsumption");
v[2] = V("AggregateNominalInvestment");
v[3] = V("NominalGovernmentConsumption");
v[4] = v[0] - v[1] - v[2] - v[3];
if (abs(v[4] / v[0]) > VS(root, "tol") && VS(root, "flagChecks")) {
  PLOG("\nThere may be some problem related to nominal sales accounting!");
}
RESULT(v[4])

EQUATION("CheckRealSalesDemand")
V("Agg_NominalSales");
v[0] = V("Agg_RealSales");
v[1] = V("AggregateConsumption");
v[2] = V("AggregateInvestment");
v[3] = V("GovernmentConsumption");
v[4] = v[0] - v[1] - v[2] - v[3];
if (abs(v[4] / v[0]) > VS(root, "tol") && VS(root, "flagChecks")) {
  PLOG("\nThere may be some problem related to real sales accounting!");
}
RESULT(v[4])

EQUATION("CheckFirmLevelDebt")
v[0] = V("CreditSupply");
v[1] = VL("TotalDebt", 1);
v[2] = V("Amortisation");
v[3] = V("TotalDebt");
v[4] = V("FirmDefault");
v[5] = v[3] - v[1] + v[2] - v[0] + v[4];
v[6] = v[3] > 0 ? v[5] / v[3] > VS(root, "tol") ? true : false : false;
if (v[6] && VS(root, "flagChecks")) {
  if (v[3] != 0) {
    plog("\nThere is possibly an accounting error in firms' debt dynamics. "
         "Check causes. The error is equivalent to ");
    PLOG("%f", round_digits(v[5] / v[3] * 100, 2));
    plog(" percent of the aggregate firms' debt.");
  }
}
RESULT(v[5])

EQUATION("CheckGovernmentDebt")
v[0] = V("GovernmentDebt");
v[1] = SUM("BillsBank");
v[2] = SUM("HouseholdBills");
v[3] = V("NominalGDP_Demand");
v[4] = v[0] - v[1] - v[2];
v[5] = v[4] / v[3];
if (abs(v[5]) > VS(root, "tol") && VS(root, "flagChecks")) {
  PLOG("\nRedundant equation not holding perfectly! The deviation is of ");
  PLOG("%f", round_digits(v[5] * 100, 2));
  plog(" percent of GDP.");
}
RESULT(v[4])
