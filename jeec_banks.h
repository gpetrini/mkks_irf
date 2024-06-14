/******************************************************************************

        Bank object equation

        ----------------------

        Few equations are added assuming that multiple bank object instances
 will be added in the future. However, the possibility of multiple banks still
        needs to be implemented.


 ******************************************************************************/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

/*============================== KEY EQUATIONS ===============================*/
EQUATION("InterestRateBank")
/*
Banks set interest rate to a particular firm.
Case 0: interest rate is the same for all firms (base interest rate + a markup)
Case 1: interest rate depends on the debt servicing to operating cash flow ratio
*/
if (VS(ROOT, "flagVariableBankInterestRate")) {
  v[0] = V("mu");
  v[1] = V("DebtServicingRatio");
  v[2] = V("BaseInterestRate");
  v[3] = v[2] + v[0] * v[1];
} else {
  v[0] = V("BaseInterestRate");
  v[1] = V("interestSpread");
  v[3] = v[0] + v[1];
}
RESULT(v[3])

EQUATION("NetWorthBank")
/*
Net worth of a particular bank.
*/
v[0] = VL("NetWorthBank", 1);
v[1] = V("UndistributedProfitsBank");
v[2] = V("NonPerformingLoans");
v[3] = v[0] + v[1] - v[2];
RESULT(v[3])

/*============================== SUPPORT EQUATIONS
 * ===============================*/
EQUATION("LoanPortfolio")
/*
Loan portfolio of a bank. Aggregates the loans supplied to all firms.
*/
v[0] = 0;           // initialise the counter of outstanding loans
v[1] = V("idBank"); // bank ID
for (i = 0; i < COUNT_HOOK; i++) {
  if (HOOK(i) != NULL) {
    VS(HOOK(i), "CreateOutstandingLoan");
    CYCLES(HOOK(i), cur, "OutstandingLoan") {
      // check supplier of that vintage of loan
      v[2] = VS(cur, "BankSupplierID");
      v[0] += v[1] == v[2] ? VS(cur, "LoanOutstanding") : 0;
    }
  }
}
RESULT(v[0])

EQUATION("DepositsFromFirms")
/*
Get total deposits from firms.
It is assumed that firms holds all deposits in the last bank's that granted
credit.
*/
v[0] = 0;
v[1] = V("idBank"); // bank ID
for (i = 0; i < COUNT_HOOK; i++) {
  if (HOOK(i) != NULL) {
    VS(HOOK(i)->up, "Entry"); // run after entry/exit took place.
    if (VS(HOOK(i), "BankID") == v[1])
      v[0] += VS(HOOK(i), "FirmDeposits");
  }
}
RESULT(v[0])

EQUATION("LoanShare")
/*
Compute the market share of banks, as % of total loans.
*/
v[0] = V("LoanPortfolio");
v[1] = VS(ECO, "AggregateDebtFirms");
v[2] = v[1] != 0 ? v[0] / v[1] : 0;
RESULT(v[2])

EQUATION("updateProbDrawBank")
/*Update the probability that a bank is drawn by the firms.
Only relevant when there is credit rationing.*/
RESULT(1)

EQUATION("AmortisationFlow")
/*
Updated in debt servicing (firms_financial).
*/
RESULT(0)

EQUATION("InterestIncome")
/*
Updated in debt servicing (firms_financial).
*/
RESULT(0)

EQUATION("DefaultLoans")
/*
Updated in debt servicing (firms_financial).
*/
RESULT(0)

EQUATION("NonPerformingLoans")
/*
Updated in debt servicing (firms_financial).
*/
RESULT(0)

EQUATION("AmortisationDue")
/*
Amount of amortisation that the bank should receive from firms under the
assumption of no default there is no default.
*/
v[0] = 0;
v[1] = V("idBank");
CYCLES(SEC, cur1, "Firms") {
  CYCLES(cur1, cur2, "OutstandingLoan") {
    v[2] = VS(cur2, "BankSupplierID");
    if (v[2] == v[1])
      v[0] += VS(cur2, "AmortisationSchedule");
  }
}
RESULT(v[0])

EQUATION("BankDividends")
/*
Calculate bank's distributed dividends.
*/
v[0] = V("etab");
v[1] = V("BankTaxableIncome");
v[2] = V("BankTax");
v[3] = v[0] * (v[1] - v[2]);
v[4] = max(0, v[3]);
RESULT(v[4])

EQUATION("BankGrossProfits")
/*
Calculate bank's gross profits: interest on loans plus interest on bills.
*/
v[0] = V("InterestIncome");
v[1] = V("BaseInterestRate");
v[2] = VL("BillsBank", 1);
v[3] = v[0] + v[1] * v[2];
RESULT(v[3])

EQUATION("AverageBankInterestRate")
/*
Calculate the average interest rate of the bank.
*/
V("BankGrossProfits"); // call after gross profits equation
v[0] = V("InterestIncome");
v[1] = VL("LoanPortfolio", 1);
v[2] = v[1] > 0 ? v[0] / v[1] : 0;
RESULT(v[2])

EQUATION("BillsBank")
/*
Bank allocation of bills closes its budget constraint.
*/
v[0] = V("NetWorthBank");
v[1] = V("LoanPortfolio");
v[2] = V("DepositsBank");
v[3] = v[0] - v[1] + v[2];
RESULT(v[3])

EQUATION("BankTaxableIncome")
/*
Calculate taxable income corresponds to the gross profits net of default.
*/
v[0] = V("BankGrossProfits");
v[1] = V("DefaultLoans");
RESULT(v[0] - v[1])

EQUATION("BankTax")
/*
Calculate bank's tax due.
*/
v[0] = VS(ECO, "tau2");
v[1] = V("BankTaxableIncome");
v[2] = v[0] * v[1];
v[3] = max(v[2], 0);
RESULT(v[3])

EQUATION("UndistributedProfitsBank")
/*
Retained earnings of banks.
*/
v[0] = V("BankGrossProfits");
v[1] = V("BankTax");
v[2] = V("BankDividends");
v[3] = v[0] - v[1] - v[2];
RESULT(v[3])

EQUATION("ChangeBillsBank")
v[0] = V("BillsBank");
v[1] = VL("BillsBank", 1);
RESULT(v[0] - v[1])

EQUATION("ChangeLoanPortfolio")
/*
Defaults on loans already deduced.
*/
v[0] = V("LoanPortfolio");
v[1] = VL("LoanPortfolio", 1);
RESULT(v[0] - v[1])

EQUATION("DepositsBank");
/*
Total deposits liabilities.
*/
v[0] = V("HouseholdDeposits");
v[1] = V("DepositsFromFirms");
RESULT(v[0] + v[1])

EQUATION("ChangeDepositsBank")
v[0] = V("DepositsBank");
v[1] = VL("DepositsBank", 1);
RESULT(v[0] - v[1])

EQUATION("BankCreditSupply")
v[0] = SUMS(SEC, "CreditSupply");
RESULT(v[0])

EQUATION("BankAssets")
/*Total assets of each bank.*/
v[0] = V("BillsBank");
v[1] = V("LoanPortfolio");
RESULT(v[0] + v[1])
