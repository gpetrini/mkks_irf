/******************************************************************************

        Aggregate equations, in countries object
        ----------------------

        Equations that are specific to aggregates.

 ******************************************************************************/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

EQUATION("Agg_NominalSales")
RESULT(SUM("NominalSales"))

EQUATION("Agg_RealSales")
/* Aggregate sales (quantity) */
CYCLE(cur, "Sectors") { VS(cur, "RevisionSales"); }
RESULT(SUM("Sales"))

EQUATION("PriceLevel")
/* Aggregate level of prices.*/
v[0] = V("Agg_NominalSales");
v[1] = V("Agg_RealSales") RESULT(v[0] / v[1])

    EQUATION("Inflation") v[0] = VL("PriceLevel", 1);
v[1] = V("PriceLevel");
v[2] = v[1] / v[0] - 1;
RESULT(v[2])

EQUATION("AggregateDemand")
/* Sum investment and consumption */
v[0] = V("AggregateConsumption");
v[1] = V("AggregateInvestment");
v[2] = V("GovernmentConsumption");
v[3] = v[0] + v[1] + v[2];

// Check if existing inventories satisfies aggregate demand
v[4] = 0; // counter of available quantity
CYCLE(cur, "Firms") { v[4] += VS(cur, "Inventory"); }
WRITE("AggregateSupply", v[4]);

if (v[4] > v[3])
  END_EQUATION(v[3]);

// In the theoretical case demand is higher than supply, reduce government
// expenditures
v[5] = v[4] - v[0] - v[1];
v[5] = max(0, v[5]);
cur = SEARCH("Government");
WRITES(cur, "GovernmentConsumption", v[5]);

// Check whether the reduction of government consumption suffice to match
// aggregate demand and aggregate supply
v[6] = v[0] + v[1] + v[5];
if (v[4] > v[6])
  END_EQUATION(v[6]);

// To assure all possible cases, if demand is still higher than supply, reduce
// households' consumption
v[7] = v[4] - v[1] - v[5];
v[8] = max(v[7], 0);
cur = SEARCH("Countries");
WRITES(cur, "AggregateConsumption", v[8]);
cur = SEARCH("Households");
WRITES(cur, "Consumption", v[8]);
v[9] = v[1] + v[5] + v[8]; // return the new aggregate demand
RESULT(v[9])

EQUATION_DUMMY("AggregateSupply", "AggregateDemand")

EQUATION("AggregateInvestment")
/* Aggregate investment */
RESULT(SUM("ActualInvestment"))

EQUATION("AggregateConsumption")
RESULT(SUM("Consumption"))

EQUATION("AggregateInventories")
/* End-of-period inventories */
CYCLE(cur, "Sectors") { VS(cur, "RevisionSales"); }
RESULT(SUM("Inventory"))

EQUATION("TotalLabour")
/* Total worked hours in the firms' sector. */
v[0] = SUM("WorkersFirm");
RESULT(v[0])

EQUATION("AggregateCapitalStock")
v[0] = SUM("RealCapitalStock");
RESULT(v[0])

EQUATION("AggregateProduction")
RESULT(SUM("Production"))

EQUATION("AggregateProductivity")
RESULT(WHTAVE("AvgLabourProductivity", "MarketShare"))

EQUATION("AggregateGrowthProductivity")
RESULT(V("AggregateProductivity") / VL("AggregateProductivity", 1) - 1)

EQUATION("AverageMarkup")
RESULT(WHTAVE("Markup", "MarketShare"))

EQUATION("AggregateCapacityUtilisation")
v[0] = 0;
// SUM( "AvailableCapitalStock" );
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[1] = VS(cur1, "AvailableCapitalStock");
    v[0] += v[1];
    if (isnan(v[1]))
      INTERACT("Nan", 0);
  }
}
v[1] = V("AggregateProduction");
v[2] = V("nu");
v[3] = (v[1] * v[2]) / v[0];
RESULT(v[3])

EQUATION("AggregateDepreciation")
v[0] = 0;
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    VS(cur1, "CapitalDepreciation");
    v[0] += VS(cur1, "RealDepreciation");
  }
}
RESULT(v[0])

EQUATION("AggregateNominalDepreciation")
v[0] = 0;
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    VS(cur1, "CapitalDepreciation");
    v[0] += VS(cur1, "NominalDepreciation");
  }
}
RESULT(v[0])

EQUATION("AggregateWageBill")
RESULT(SUM("WageBill"))

EQUATION("AggregateDividendsFirms")
RESULT(SUM("FirmDividends"))

EQUATION("AggregateDebtAmortisation")
RESULT(SUM("Amortisation"))

EQUATION("AggregateDividendsBanks")
RESULT(SUM("BankDividends"))

EQUATION("AggregateNewLoans")
RESULT(SUM("CreditSupply"))

EQUATION("AggregateNominalCapital")
RESULT(SUM("NominalCapitalStock"))

EQUATION("AggregateAssetsFirms")
RESULT(SUM("FirmAssets"))

EQUATION("AggregateLeverage")
v[0] = V("AggregateDebtFirms");
v[1] = V("AggregateAssetsFirms");
RESULT(v[0] / v[1])

EQUATION("AggregateNonPerformingLoans")
RESULT(SUM("NonPerformingLoans"))

EQUATION("RealGDP_Demand")
v[0] = SUM("Sales");
v[1] = V("AggregateInventories");
v[2] = VL("AggregateInventories", 1);
v[3] = v[0] + v[1] - v[2];
RESULT(v[3])

EQUATION("NominalGDP_Demand")
v[0] = SUM("NominalSales");
v[1] = SUM("InventoryRevaluation");
v[2] = v[0] + v[1];
RESULT(v[2])
/*============================== HOUSEHOLDS ===============================*/
EQUATION("AggregateNominalConsumption")
/*Nominal consumption of households*/
v[0] = 0;
CYCLE(cur, "Households") { v[0] += VS(cur, "NominalConsumption"); }
RESULT(v[0])

EQUATION("FinancialSavingsHouseholdsSector")
v[0] = V("AggregateNominalConsumption");
v[1] = SUM("TotalIncomeHousehold");
v[2] = SUM("TaxHousehold");
v[3] = v[1] - v[0] - v[2];
RESULT(v[3])

EQUATION("FinancialSavingsHouseholds_GDP")
v[0] = V("FinancialSavingsHouseholdsSector");
v[1] = V("NominalGDP_Demand");
RESULT(v[0] / v[1])

EQUATION("HouseholdsWealthGDP")
v[0] = SUM("HouseholdWealth");
v[1] = V("NominalGDP_Demand");
RESULT(v[0] / v[1])

EQUATION("ChangeDepositsHouseholdsSector")
v[0] = SUM("HouseholdDeposits");
v[1] = SUML("HouseholdDeposits", 1);
RESULT(v[0] - v[1])

EQUATION("ChangeBillsHouseholdsSector")
v[0] = SUM("HouseholdBills");
v[1] = SUML("HouseholdBills", 1);
RESULT(v[0] - v[1])
/*============================== BANKS ===============================*/
EQUATION("FinancialSavingsBankingSector")
v[0] = SUM("UndistributedProfitsBank");
RESULT(v[0])

EQUATION("FinancialSavingsBankingSector_GDP")
v[0] = V("FinancialSavingsBankingSector");
v[1] = V("NominalGDP_Demand");
RESULT(v[0] / v[1])

EQUATION("ChangeBillsBanksSector")
RESULT(SUM("ChangeBillsBank"))

EQUATION("ChangeLoanPortfolioBanksSector")
RESULT(SUM("ChangeLoanPortfolio"))

EQUATION("ChangeDepositsBanksSector")
RESULT(SUM("ChangeDepositsBank"))
/*============================== GOVT. ===============================*/
EQUATION("FinancialSavingsGovermentSector")
v[0] = V("NominalDeficitGovernment");
RESULT(-v[0])

EQUATION("FinancialSavingsGovermentSector_GDP")
v[0] = V("FinancialSavingsGovermentSector");
v[1] = V("NominalGDP_Demand");
RESULT(v[0] / v[1])

EQUATION("GovernmentDebtToGDPRatio")
RESULT(V("GovernmentDebt") / V("NominalGDP_Demand"))
/*============================== FIRMS ===============================*/
EQUATION("AggregateNominalInvestment")
RESULT(SUM("NominalInvestment"))

EQUATION("AggregateSavingFirms")
/*Aggregate undistributed profits.*/
RESULT(SUM("UndistributedProfits"))

EQUATION("FinancialSavingsFirmsSector")
RESULT(V("AggregateSavingFirms") - V("AggregateNominalInvestment"))

EQUATION("FinancialSavingsFirmsSector_GDP")
RESULT(V("FinancialSavingsFirmsSector") / V("NominalGDP_Demand"))

EQUATION("AggregateDebtFirms")
v[0] = 0;
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") { v[0] += VS(cur1, "TotalDebt"); }
}
RESULT(v[0])

EQUATION("AggregateFirmDeposits")
/*CYCLE( cur, "Sectors" )
// run after entry takes place
{
        VS( cur, "Entry" );
}*/
RESULT(SUM("FirmDeposits"))

EQUATION("ChangeDepositsFirmsSector")
v[0] = V("AggregateFirmDeposits");
v[1] = VL("AggregateFirmDeposits", 1);
RESULT(v[0] - v[1])

EQUATION("ChangeDebtFirmsSector")
v[0] = V("AggregateDebtFirms");
v[1] = VL("AggregateDebtFirms", 1);
RESULT(v[0] - v[1])

EQUATION("FinancialFragilityIndex")
/*
The financial fragility index is calculated according to the financial status of
firms. If a firm is hedge, it weights nothing in the index. A speculative firm
weights 0.5. A Ponzi firm weights 1. The weight of a single firm in the index is
given by the overall share of assets. Level: country.
*/
v[0] = 0; // aggregator of the index
v[1] = 0; // aggregator of the total assets
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[2] = VS(cur1, "SpeculativeFirmFlag");
    v[3] = VS(cur1, "PonziFirmFlag");
    v[4] = VS(cur1, "FirmAssets");
    v[0] += (v[2] * 0.5 + v[3] * 1) * v[4];
    v[1] += v[4];
  }
}
v[0] /= v[1];
RESULT(v[0])

EQUATION("IncidenceHedgeFinancing")
/*Proportion of hedge firms. Not-weighted.*/
v[0] = 0;
v[1] = COUNT("Firms");
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") { v[0] += VS(cur1, "HedgeFirmFlag"); }
}
v[2] = v[0] / v[1];
RESULT(v[2])

EQUATION("WeightedIncidenceHedgeFinancing")
/*Proportion of hedge firms. Weighted by the total assets.*/
v[0] = 0; // aggregator of the index
v[1] = 0; // aggregator of the total assets
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[2] = VS(cur1, "HedgeFirmFlag");
    v[3] = VS(cur1, "FirmAssets");
    v[0] += v[2] * v[3];
    v[1] += v[3];
  }
}
v[0] /= v[1];
RESULT(v[0])

EQUATION("IncidenceSpeculativeFinancing")
/*Proportion of speculative firms. Not-weighted.*/
v[0] = 0;
v[1] = COUNT("Firms");
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") { v[0] += VS(cur1, "SpeculativeFirmFlag"); }
}
v[2] = v[0] / v[1];
RESULT(v[2])

EQUATION("WeightedIncidenceSpeculativeFinancing")
/*Proportion of speculative firms. Weighted by the total assets.*/
v[0] = 0; // aggregator of the index
v[1] = 0; // aggregator of the total assets
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[2] = VS(cur1, "SpeculativeFirmFlag");
    v[3] = VS(cur1, "FirmAssets");
    v[0] += v[2] * v[3];
    v[1] += v[3];
  }
}
v[0] /= v[1];
RESULT(v[0])

EQUATION("IncidencePonziFinancing")
/*Proportion of Ponzi firms. Not-weighted.*/
v[0] = 0;
v[1] = COUNT("Firms");
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") { v[0] += VS(cur1, "PonziFirmFlag"); }
}
v[2] = v[0] / v[1];
RESULT(v[2])

EQUATION("WeightedIncidencePonziFinancing")
/*Proportion of Ponzi firms. Weighted by the total assets.*/
v[0] = 0; // aggregator of the index
v[1] = 0; // aggregator of the total assets
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[2] = VS(cur1, "PonziFirmFlag");
    v[3] = VS(cur1, "FirmAssets");
    v[0] += v[2] * v[3];
    v[1] += v[3];
  }
}
v[0] /= v[1];
RESULT(v[0])

EQUATION("InvToGDPRatio")
/*Investment to GDP ratio*/
RESULT(V("AggregateInvestment") / V("RealGDP_Demand"))

EQUATION("InvToCapitalRatio")
/*Investment to GDP ratio*/
RESULT(V("AggregateInvestment") / V("AggregateCapitalStock"))

EQUATION("GrowthRateMaxProductivity")
/*Growth rate of best-known technology of firms, weighted by the market
 * shares.*/
RESULT(WHTAVE("ACurrent", "MarketShare") /
           WHTAVEL("ACurrent", "MarketShare", 1) -
       1)

EQUATION("GrowthRateCapitalStock")
RESULT(V("AggregateCapitalStock") / VL("AggregateCapitalStock", 1) - 1)

EQUATION("ReplacementInvToCapitalRatio")
RESULT(SUM("ReplacementInvestment") / VL("AggregateCapitalStock", 1))

EQUATION("AggregateProfits")
RESULT(SUM("OperatingCashFlow"))

EQUATION("AggregateProfitRate")
/*Aggregate operating cash flow to capital ratio*/
v[0] = SUM("OperatingCashFlow");
v[1] = VL("AggregateNominalCapital", 1);
RESULT(v[0] / v[1])

EQUATION("_MedA")
RESULT(MED("AvgLabourProductivity") / MAX("AvgLabourProductivity"))

EQUATION("_MinA")
v[2] = MAX("AvgLabourProductivity");
v[0] = v[2];
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[1] = VS(cur1, "AvgLabourProductivity");
    if (v[1] != 0)
      v[0] = min(v[0], v[1]);
  }
}
RESULT(v[0] / v[2])

EQUATION("_LumH")
v[3] = 0;
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[0] = VS(cur1, "AvailableCapitalStock");
    v[1] = VS(cur1, "ActualInvestment");
    v[2] = v[1] / v[0];
    if (v[2] >= 0.2) {
      v[3] += VS(cur1, "MarketShare");
    }
  }
}
RESULT(v[3])

EQUATION("_LumL")
v[3] = 0;
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[0] = VS(cur1, "AvailableCapitalStock");
    v[1] = VS(cur1, "ActualInvestment");
    v[2] = v[1] / v[0];
    if (v[2] < 0.02) {
      v[3] += VS(cur1, "MarketShare");
    }
  }
}
RESULT(v[3])

EQUATION("ProfitDebtDistributionIndex")
/*Calculate the profit-debt distribution index.
First: order firms according to the leverage.
Second: get cumulative profits and debt.
Third: get proportion of profits and debt.
Fourth: get cumulative proportion of each.
Fifth: calculate index.*/
SORT("Firms", "Leverage", "Up");
v[0] = V("AggregateProfits");
v[1] = V("AggregateDebtFirms");
if (v[0] == 0 || v[1] == 0)
  END_EQUATION(0);
v[2] = 0; // store cumsum of profits.
v[3] = 0; // store cumsum of debt.
v[4] = 0; // store index
CYCLE(cur, "Sectors") {
  CYCLES(cur, cur1, "Firms") {
    v[5] = v[2]; // store the lagged cumsum of profits
    v[6] = v[3]; // store the lagged cumsum of debt
    v[2] += VS(cur1, "OperatingCashFlow") / v[0]; // update cumsum of profits
    v[3] += VS(cur1, "TotalDebt") / v[1];         // update cumsum of debt
    v[4] -= (v[3] * v[5] - v[6] * v[2]);          // update area of plot (index)
  }
}
SORT("Firms", "idFirm", "Up");
RESULT(v[4])

EQUATION("_PDIndex")
RESULT(V("ProfitDebtDistributionIndex"))

EQUATION("AggregateDividendPayoutRatioFirms")
RESULT(SUM("FirmDividends") / (SUM("NetProfits") - SUM("TaxFirm")))

EQUATION("AverageInterestPayment")
RESULT(VL("AggregateDebtFirms", 1) > 0
           ? SUM("InterestPayment") / VL("AggregateDebtFirms", 1)
           : 0)

EQUATION("WorkingCapitalToTotalLoanDemand")
v[0] = SUM("WorkingCapitalLoanDemand");
v[1] = SUM("DemandLoans");
v[2] = v[1] != 0 ? v[0] / v[1] : 0;
RESULT(v[2])
