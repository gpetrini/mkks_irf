/******************************************************************************

        Firm object equations
        ----------------------

        Subset of equations related to financial matters.

 ******************************************************************************/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

/*============================== KEY EQUATIONS ===============================*/
EQUATION("DemandLoans")
/*
Demand for new loans.
If there is credit rationing, firms choose which banks to send the prospects.
Level: firm.
*/
v[0] = VS(PARENT, "ExpectedPrice"); // price of the single good
v[1] = V(
    "PlannedInvestment"); // total planned investment (expansion + replacement)
v[2] = V("ExpectedWageBill");
v[3] = VL("FirmDeposits", 1);             // begin-of-period deposits
v[4] = max(0, v[0] * v[1] + v[2] - v[3]); // total demand for loans
WRITE("DemandLoansForBankRiskEvaluation",
      v[4]); // write the parameter demand for loans that is used in bank's risk
             // evaluation. This value is updated to disconsider investment in
             // case the loan is not granted.
if (VS(ROOT, "flagRationing")) // is the credit rationing flag activated?
{                              // yes, find banks
  v[5] = VS(ECO, "epsilon");   // number of banks to search
  if (v[5] > 1) {
    v[6] = v[5] - 1;   // consider only from the second bank onwards
    while (v[6] > 0) { // since firms try to keep relation with the same bank
      v[7] = V("DrawABank");
      cur = SEARCH_CND("idBank", v[7]);
      WRITE_HOOK(v[6], cur); // write hook pointer. Zero is the bank to which
                             // the firm is already client.
      v[6] -= 1;
    }
  }
}
RESULT(v[4])

EQUATION("ProbabilityOfDefault")
/*Calculate the probability of default of a loan project.*/
if (!VS(ROOT, "flagRationing") || V("DemandLoans") == 0)
  END_EQUATION(0);

v[2] = V("DemandLoansForBankRiskEvaluation"); // total loan demand

if (v[2] == 0)
  END_EQUATION(0);

v[0] = V("ExpectedOperatingCashFlow");
v[1] = VS(ECO, "varPhi1");
v[7] = VS(ECO, "varPhi2");
v[8] = VL("Leverage", 1);
v[3] = VS(SEC, "lambda");         // duration of loan
v[4] = V("InterestAgreedBank");   // get interest rate
v[5] = v[2] / v[3] + v[4] * v[2]; // debt servicing of this loan project.
v[6] =
    1 / (1 + exp(v[1] * v[0] / v[5] - v[7] * v[8])); // probability of default
RESULT(v[6])

EQUATION("ExpectedReturnOfLoanProject")
v[0] = V("InterestAgreedBank");
v[1] = VS(SEC, "lambda");
v[2] = V("ProbabilityOfDefault");
if (v[2] > 1e-04)
  v[3] = 1 + v[0] * (v[1] + 1) / 2 -
         (1 + v[0]) *
             (1 + 1 / v[1] + (pow(1 - v[2], v[1] + 1) - 1) / (v[1] * v[2]));
else
  v[3] = 1 + v[0] * (v[1] + 1) / 2;
RESULT(v[3])

EQUATION("OperatingCashFlow")
/*
Actual operating cash flow of firms.
Level: firm.
*/
v[0] = V("NominalSales");
v[1] = V("WageBill");
v[2] = v[0] - v[1];
RESULT(v[2])

EQUATION("GrossProfits")
/*
Actual gross profits of firms, which here is defined as the operating cash flow
less interest payments. Level: firm.
*/
v[0] = V("OperatingCashFlow");
v[1] = V("InterestPayment");
v[2] = v[0] - v[1];
RESULT(v[2])

EQUATION("NetProfits")
/*
Actual net profits of firms. Duduction of the capital depreciation and inventory
revaluation of the gross profits. Level: firm.
*/
v[0] = V("NominalSales");
v[1] = V("WageBill");
v[2] = V("InventoryRevaluation");
v[3] = V("InterestPayment");
v[4] = V("NominalDepreciation");
v[5] = v[0] - v[1] + v[2] - v[3] - v[4];
RESULT(v[5])

EQUATION("FirmDividends")
/*
Dividends paid by the firm.
Level: firm.
*/
v[0] = V("DividendPayoutRatio");
v[1] = V("NetProfits");
v[2] = V("TaxFirm");
v[3] = v[0] * (v[1] - v[2]);
v[4] = max(0, v[3]);
v[6] = VL("Leverage", 1);
v[7] = V("FirmDeposits_PostTaxesUpdate");
v[9] = v[4] > v[7] ? 0 : v[4];
v[8] = V("flagExit");
v[10] = v[8] ? 0 : v[9];
RESULT(v[10])

EQUATION("FirmDeposits")
/*
Calculate the final firms' deposits, by deducing the amortisation and interest
payments. Level: firm.
*/
V("DebtServicing"); // run after debt servicing (because if firms are leaving
                    // the market amortisation and interest payments is updated
                    // there)	v[0] = V( "FirmDeposits_PostDividendsUpdate" );
v[0] = V("FirmDeposits_PostDividendsUpdate");
v[1] = V("Amortisation");
v[2] = V("InterestPayment");
v[3] = v[0] - v[1] - v[2];
RESULT(v[3])

EQUATION("TotalDebt")
/*
Total debt of each firm.
Level: firm.
*/
V("CreateOutstandingLoan");
RESULT(SUM("LoanOutstanding"))

EQUATION("FirmAssets")
/*
Sum of financial and non-financial assets of firms.
Level: firm.
*/
v[0] = V("FirmDeposits");
v[1] = V("NominalCapitalStock");
v[2] = V("Inventory");
v[3] = V("UnitCost");
v[4] = v[2] * v[3];
RESULT(v[0] + v[1] + v[4])

EQUATION("Leverage")
/*
Firm's total debt to total assets ratio.
Level: firm.
*/
v[0] = V("FirmAssets");
v[1] = V("TotalDebt");
RESULT(v[1] / v[0])

/*============================== SUPPORT EQUATIONS
 * ===============================*/
EQUATION("WageBill")
/*
Firm's wage bill.
Level: firm.
*/
v[0] = VS(ECO, "NominalWage");
v[1] = V("WorkersFirm");
RESULT(v[0] * v[1])

EQUATION("ExpectedWageBill")
/*
The expected wage bill is used by firms to demand loans to banks.
It may differ from the actual wage bill if a firm is credit rationed and/or if
it's labour rationed. Level: firm.
*/
v[0] = VS(ECO, "NominalWage");
v[1] = V("DesiredDemandLabour");
v[2] = v[0] * v[1];
RESULT(v[2])

EQUATION("DrawABank")
/*
Function to draw a bank randomly. Used in the matching of firms and banks.
Level: firm.
*/
V("updateProbDrawBank");
cur = RNDDRAWS(ECO, "Banks", "probDrawBank");
RESULT(VS(cur, "idBank"))

EQUATION("InterestAgreedBank")
/*
Store interest rate agreed with bank in the firm object.
Level: firm.
*/
WRITES(BNK, "DebtServicingRatio",
       VL("DebtServicingToOperatingCashFlowRatio",
          1)); // pass information of lagged debt servicing to operating cash
               // flow ratio (interest determination)
v[0] = VS(BNK, "InterestRateBank"); // get interest rate
RESULT(v[0])

EQUATION("DefineCreditLimit")
/*
Maximum amount of credit a bank is willing to provide to firms.
Level: firm (for ease of data access).
*/
cur1 = SHOOK == NULL ? SEARCH_CND("idBank", V("BankSupplierID"))
                     : SHOOK; // gather bank data
v[0] = V("InterestAgreedBank");
v[1] = VLS(BNK, "AverageBankInterestRate", 1);
v[2] = VS(SEC, "lambda");
v[3] = 50;    // max iterations
v[4] = 0;     // initialise iteration count
v[5] = 0.05;  // initial guess probability of default
v[6] = 0.05;  // error measurement initialised
v[7] = 1e-04; // tolerance of probability of default equalisation
while (v[4] < v[3] && v[6] > v[7]) {
  v[8] = v[0] * (v[2] + 1) / 2 -
         (1 + v[0]) *
             (1 + 1 / v[2] + (pow(1 - v[5], v[2] + 1) - 1) / (v[2] * v[5]));
  v[9] = v[8] - v[1]; // error
  v[5] += v[9] * 0.05;
  v[6] = abs(v[9]); // error variable update (absolute deviation from the target
                    // rate of return)
  v[4]++;
}
if (v[5] > V("ProbabilityOfDefault"))
  PLOG("\nOpa! Deu ruim!");
v[10] = V("ExpectedOperatingCashFlow");
v[11] = VS(ECO, "varPhi1");
v[12] = VS(ECO, "varPhi2");
v[13] = VL("Leverage", 1);
v[14] = v[11] * v[10] / (log((1 - v[5]) / v[5]) + v[12] * v[13]);
v[15] = v[14] / (1 / v[2] + v[0]);
v[15] = max(0, v[15]);
RESULT(v[15])

EQUATION("CreditSupply")
/*
Credit supply of banks.
Equal to demand if there is no credit rationing.
Level: firm.
*/
if (VS(ROOT, "flagRationing")) {
  v[0] = V("ExpectedReturnOfLoanProject");
  if (v[0] >= 1)
    v[1] = V("DemandLoans");
  else {
    v[1] = V("DefineCreditLimit");
  }
} else
  v[1] = V("DemandLoans");
RESULT(v[1])

EQUATION("FirmDeposits_PostCreditUpdate")
/*
Total deposits of a firm, after credit market interaction.
The result sums the deposits inherited from the past period plus the credit
granted by the partner bank. The creation of a loan vintage object storing the
new loan information occurs only in equation CreateOutstandingLoan. Level: firm.
*/
v[0] = VL("FirmDeposits", 1);
v[1] = V("CreditSupply");
v[2] = v[0] + v[1];
RESULT(v[2])

EQUATION("FundsExpansionInvestment")
/*
Post-credit market interaction available funds for expansion investment.
Level: firm.
*/
v[0] = V("FirmDeposits_PostCreditUpdate");
v[1] = V("WorkersFirm");
v[2] = V("NominalWage");
v[3] = V("ExpectedPrice");
v[4] = V("PlannedExpansionInvestment");
v[5] = v[1] * v[2]; // Wage bill
v[6] = v[0] - v[5]; // Actual funds available for expansion investment
v[7] = v[3] * v[4]; // Funds required for planned expansionary investment
v[8] = min(v[6], v[7]);
RESULT(v[8])

EQUATION("FundsReplacementInvestment")
/*
Compute the funds actually available for realising replacement investment.
Level: firm.
*/
v[0] = V("FirmDeposits_PostCreditUpdate");
v[1] = V("WorkersFirm");
v[2] = V("NominalWage");
v[4] = V("FundsExpansionInvestment");
v[5] = v[1] * v[2];        // Wage bill
v[6] = v[0] - v[5] - v[4]; // Actual funds available for replacement investment
v[7] = V("ExpectedPrice");
v[8] = V("PlannedReplacementInvestment");
v[9] = v[7] * v[8];
v[10] = min(v[6], v[9]); // Actual replacement investment (expected, nominal)
v[11] = max(v[10], 0);
v[12] = v[11] / v[7]; // Actual replacement investment (effective, real)
WRITE("maximumReplacementInvestmentNominal", v[11]);
WRITE("maximumReplacementInvestmentReal", v[12]);
RESULT(v[11])

EQUATION("FirmDeposits_PostProductionUpdate")
/*
This equation deducts wage payments from firms' deposits after the production
takes place. Level: firm.
*/
v[0] = V("FirmDeposits_PostCreditUpdate");
v[1] = V("WageBill");
RESULT(v[0] - v[1])

EQUATION("FirmDeposits_PostSalesUpdate")
/*
Equation to credit sales in firms' deposits. Also, deduce investment expenses.
Level: firm.
*/
v[0] = V("NominalSales");
v[1] = V("NominalInvestment");
v[2] = V("FirmDeposits_PostProductionUpdate");
v[3] = v[0] + v[2] - v[1];
RESULT(v[3])

EQUATION("TaxFirm")
/*
Total income taxes due by firm.
Level: firm.
*/
v[0] = V("NetProfits");
v[1] = V("tau2");
v[2] = v[0] * v[1];
v[3] = max(0, v[2]);
v[4] = V("FirmDeposits_PostSalesUpdate");
if (v[3] >
    v[4]) // in case the firm cannot pay the due taxes, it will exit the market.
{
  v[3] = v[4];
  WRITE("flagExit", 1);
}
RESULT(v[3])

EQUATION("ExpectedOperatingCashFlow")
/*
Expected operating cash flow of firms. Assumed to be simply the last periods'
cash flow. Level: firm.
*/
RESULT(VL("OperatingCashFlow", 1))

EQUATION("FirmDeposits_PostTaxesUpdate")
/*
Level: firm
*/
v[0] = V("TaxFirm");
v[1] = V("FirmDeposits_PostSalesUpdate");
RESULT(v[1] - v[0])

EQUATION("FirmDeposits_PostDividendsUpdate")
/*
Level: firm
*/
v[0] = V("FirmDividends");
v[1] = V("FirmDeposits_PostTaxesUpdate");
RESULT(v[1] - v[0])

EQUATION("UndistributedProfits")
/*
Nominal undistributed profits.
Level: firm.
*/
v[0] = V("GrossProfits");
v[1] = V("FirmDividends");
v[2] = V("TaxFirm");
v[3] = v[0] - v[1] - v[2];
RESULT(v[3])

EQUATION("HedgeFirmFlag")
/*Identifies if a firm is in hedge financial status.
That happens when the operating cash flow is greater than the investment plus
the financial commitments.*/
v[0] = V("OperatingCashFlow");
v[1] = V("Amortisation");
v[2] = V("InterestPayment");
v[3] = V("NominalInvestment");
v[4] = v[1] + v[2] + v[3];
v[5] = v[4] < v[0] ? 1 : 0;
RESULT(v[5])

EQUATION("SpeculativeFirmFlag")
/*Identifies if a firm is in speculative financial status.
That happens when the operating cash flow is less than the investment plus the
financial commitments, but the cash flow is still greater than the financial
commitments.*/
v[0] = V("OperatingCashFlow");
v[1] = V("Amortisation");
v[2] = V("InterestPayment");
v[3] = V("NominalInvestment");
v[4] = v[1] + v[2] + v[3];
v[5] = v[4] > v[0] ? 1 : 0;
v[6] = v[5] * (v[1] + v[2] <= v[0] ? 1 : 0);
RESULT(v[6])

EQUATION("PonziFirmFlag")
/*Identifies if a firm is in Ponzi financial status.
That happens when the operating cash flow is less than the investment plus the
financial commitments and the cash flow is still lower than the financial
commitments.*/
v[0] = V("HedgeFirmFlag");
v[1] = V("SpeculativeFirmFlag");
v[2] = 1 - v[0] - v[1];
RESULT(v[2])

EQUATION("DebtToSalesRatio")
/*
Total debt to operating cash flow ratio.
Level: firm.
*/
v[1] = V("TotalDebt");
v[2] = V("OperatingCashFlow");
RESULT(v[2] != 0 ? v[1] / v[2] : 1000)

EQUATION("DebtServicingToOperatingCashFlowRatio")
/*
Level: firm.
*/
v[0] = V("OperatingCashFlow");
v[1] = V("Amortisation");
v[2] = V("InterestPayment");
v[3] = v[1] + v[2];
v[4] = v[0] > 0 ? v[3] / v[0] : 20;
RESULT(v[4])

EQUATION("DividendPayoutRatio")
/*
Level: firm.
*/
v[0] = V("etaf1");
v[1] = V("DebtServicingToOperatingCashFlowRatio");
v[2] = VS(BNK, "varepsilon");
v[3] = 2 * v[0] / (1 + exp(v[1] * v[2]));
RESULT(v[3])

EQUATION("AverageInterestRate")
/*
Level: firm.
*/
v[0] = V("TotalDebt");
RESULT(v[0] > 0 ? WHTAVE("LoanInterestRate", "LoanOutstanding") / v[0] : 0)

EQUATION("MinskianFirm")
/*
Level: firm.
*/
v[0] = V("CreditSupply");
v[1] = V("UndistributedProfits");
RESULT(v[0] > v[1])

EQUATION("WorkingCapitalLoanDemand")
/*
Level: firm.
*/
v[0] = V("ExpectedWageBill");
v[1] = VL("FirmDeposits", 1);
v[2] = max(0, v[0] - v[1]);
RESULT(v[2])

/*============================== HOUSEKEEPING: LOAN OUTSTANDING OBJECT
 * ===============================*/
EQUATION("Amortisation")
/*
Sum amortisation commitments.
Level: firm
*/
// force execution at the bank level amortisation computation together with this
// equation.
SUMS(BNK, "AmortisationDue");
RESULT(SUM("AmortisationSchedule"))

EQUATION("InterestPayment")
/*
Sum interest payments.
Level: firm.
*/
v[0] = 0; // accumulator
CYCLE(cur, "OutstandingLoan") {
  if (VS(ROOT, "flagFloatingInterestRate"))
    WRITE("LoanInterestRate",
          VS(BNK, "InterestRateBank")); // if the interest rate is floating,
                                        // get bank's current rate

  v[1] = VS(cur, "LoanInterestRate");
  v[2] = VLS(cur, "LoanOutstanding", 1);
  v[3] = v[1] * v[2];
  WRITES(cur, "InterestSchedule", v[3]);
  v[0] += v[3];
}
RESULT(v[0])

EQUATION("DefaultFlag")
/*
Checks whether the firm has sufficient funds to cover all debt servicing.
Level: firm.
*/
V("CapitalVintageHousekeeping");
v[0] = V("Amortisation") + V("InterestPayment"); // total debt servicing
v[1] = V("FirmDeposits_PostDividendsUpdate");    // deposits available at the
                                              // liquidation stage. Needs to be
                                              // updated.
v[2] = v[0] > v[1] ? true : false;
if (v[2])
  WRITE("flagExit", 1);
v[2] = V("flagExit") == 1 ? true : false;
RESULT(v[2])

EQUATION("DebtServicing")
/*
Miscellaneous housekeeping regard debt servicing:
a) access information regarding the debt servicing of firms;
b) in case the firm is able to repay amortisation and interest, write the
information of debt-servicing in pertinent bank. Otherwise, firm defaults and
dies (dealt with in another equation). c) update time-management of loans
information. Level: firm.
*/

v[0] = V("DefaultFlag");
if (!v[0]) // is the firm leaving the market? no. In case of default, the debt
           // servicing is updated elsewhere.
  CYCLE(cur1, "OutstandingLoan") {

    v[1] = VS(BNK, "InterestIncome");    // get current interest income (bank)
    v[2] = VS(cur1, "InterestSchedule"); // get interest schedule (firm)
    WRITES(BNK, "InterestIncome",
           v[1] + v[2]); // update interest income of bank with firm payment

    v[3] = VS(
        BNK,
        "AmortisationFlow"); // get amount of amortisation received by the bank
    v[4] = VS(cur1, "AmortisationSchedule"); // get amortisation schedule
    WRITES(BNK, "AmortisationFlow",
           v[3] + v[4]); // update amortisation schedule

    v[5] = VLS(cur1, "LoanOutstanding", 1);       // get loan outstanding
    WRITES(cur1, "LoanOutstanding", v[5] - v[4]); // update loan outstanding

    v[6] = VS(cur1, "PeriodsRemaining"); // get number of periods remaining to
                                         // fulfil the debt
    WRITES(cur1, "PeriodsRemaining", v[6] - 1);
  }
// in case the firm is leaving the market (for any reason):
// use existing deposits to pay banks (which is done proportionally, considering
// the amount owned per credit contract) calculate total default if there is
// some resources remaining in the bank account, transfer to households this
// computation considers the credit supply of the current period, which still
// has not been added
else {
  v[9] = V("FirmDeposits_PostDividendsUpdate");
  v[10] = SUM("LoanOutstanding") + V("CreditSupply");
  v[11] = SUM("InterestSchedule");
  v[12] = v[10] + v[11];
  v[14] =
      min(1, v[12] != 0 ? v[9] / v[12] : 1); // proportion of interest that can
                                             // be paid with existing deposits
  v[15] = min(1, v[12] != 0 ? v[9] / v[12]
                            : 1); // proportion of amortisation that can be paid
                                  // with the existing deposits
  WRITE("Amortisation",
        v[15] *
            v[10]); // rewrite actual amortisation actually paid by the firm.
  v[13] = V("CreditSupply");
  v[16] = v[13] * v[15];             // accumulator of amount paid
  v[17] = V("CreditSupply") - v[16]; // accumulator of firms' default
  v[9] -= v[16]; // exclude payment of current period's debt out of available
                 // resources
  WRITES(BNK, "AmortisationFlow",
         VS(BNK, "AmortisationFlow") +
             v[16]); // update amortisation flow of bank that granted credit
                     // this period
  WRITES(BNK, "NonPerformingLoans",
         VS(BNK, "NonPerformingLoans") + v[17]); // update default of the bank
  WRITE("FirmDefault", (1 - v[15]) * v[10]);

  CYCLE(cur1, "OutstandingLoan") {
    if (v[9] > 0) {
      v[1] = VS(BNK, "InterestIncome");    // get current interest income (bank)
      v[2] = VS(cur1, "InterestSchedule"); // get interest schedule (firm)
      v[3] = v[2] * v[14];                 // interest due that can be paid
      v[9] -= v[3];                        // reduce deposits accordingly
      v[16] += v[3];                       // accumulate total amount paid
      v[17] += v[2] - v[3]; // accumulate default (includes interest)
      WRITES(BNK, "InterestIncome",
             v[1] + v[3]); // update interest income of bank with firm payment

      v[4] = VS(BNK, "AmortisationFlow"); // get amount of amortisation received
                                          // by the bank
      v[5] = VS(cur1, "LoanOutstanding"); // get loan outstanding
      v[6] =
          v[5] * v[15]; // amortisation due (apply % of total loans outstanding)
      v[9] -= v[6];     // reduce available deposits after amortisation
      v[8] =
          v[5] -
          v[6]; // non-performing loans of banks (includes write-off of loans)
      v[16] += v[6]; // accumulate total amount paid
      v[17] += v[8]; // accumulate total default (include interest)
      WRITES(BNK, "AmortisationFlow",
             v[4] + v[6]); // update amortisation flow of bank

      v[7] = VS(BNK, "NonPerformingLoans");
      v[18] =
          v[8] + v[2] - v[3]; // default (include interest not paid to banks)

      if (v[8] > 0) {
        WRITES(BNK, "NonPerformingLoans",
               v[7] + v[8]); // update total non-performing loans of bank (does
                             // not include interest)
        WRITES(BNK, "DefaultLoans",
               VS(BNK, "DefaultLoans") +
                   v[18]); // update total default on loans
      }

      WRITES(cur1, "LoanOutstanding", 0); // update loan outstanding
    }
  }

  WRITE("InterestPayment", v[16] - V("Amortisation"));

  if (v[9] > 0) // if there is some remaining cash balance, distribute between
                // households
  {
    v[18] = COUNTS(ECO, "Households");
    WRITES(HHS, "CapitalContribution",
           VS(HHS, "CapitalContribution") - v[9] / v[18]);
    WRITE("FirmDeposits_PostDividendsUpdate",
          V("FirmDeposits_PostDividendsUpdate") -
              v[9] / v[18]); // deduce deposits of this transfer to households
  }
  RECALC("GrossProfits"); // consider the changed interest payments into the
                          // gross and net profits
  RECALC("NetProfits");
}
RESULT(0)

EQUATION_DUMMY("LoanOutstanding", "DebtServicing")

EQUATION("CreateOutstandingLoan")
/*
This equation eliminates expired outstanding loans instances. It further creates
new loans object after the credit market interaction. Created after debt
servicing. Level: firm.
*/

V("DebtServicing");

v[1] = 0;
if (V("CreditSupply") > 0 && !V("DefaultFlag")) {
  cur = ADDOBJ("OutstandingLoan");
  v[0] = VS(ECO, "lambda");
  WRITES(cur, "LoanOutstanding", V("CreditSupply"));
  WRITES(cur, "BankSupplierID", V("BankID"));
  WRITES(cur, "AmortisationSchedule", V("CreditSupply") / v[0]);
  WRITES(cur, "InterestSchedule", 0);
  WRITES(cur, "PeriodsRemaining", v[0]);
  // add hook to bank
  cur1 = SEARCH_CND("idBank", V("BankID"));
  WRITE_SHOOKS(cur, cur1);
  WRITES(cur, "LoanInterestRate", V("InterestAgreedBank"));

  // v[1] = V( "CreditSupply" );
}
// Removes instances of outstanding loans that were fully paid.

CYCLE_SAFE(cur, "OutstandingLoan") { // for each OutstandingLoan
  v[2] = VS(cur, "PeriodsRemaining");
  v[3] = COUNT("OutstandingLoan");
  if (v[2] <= 0 && v[3] > 1)
    DELETE(cur);
  else if (v[3] == 1 && v[2] == 0 && VS(cur, "AmortisationSchedule") != 0) {
    WRITES(cur, "LoanOutstanding", 0);
    WRITES(cur, "AmortisationSchedule", 0);
    WRITES(cur, "InterestSchedule", 0);
    WRITES(cur, "PeriodsRemaining", 0);
    WRITES(cur, "LoanInterestRate", 0);
  }
}
RESULT(v[1])

EQUATION_DUMMY("FirmDefault", "DebtServicing")
