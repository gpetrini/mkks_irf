/******************************************************************************

        Model initialisation

 ******************************************************************************/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

EQUATION("Init")
/*
Technical variable to initialise the firms and some initial conditions.
It is computed only once in the beginning of the simulation.
Must be the first variable in the list in model's setup.
*/
std::string callFile = std::string(CONFIG);
std::string pPath = std::string(PATH);
std::string rPath = "/init_cond/";

// for some reason, when running in parallel, CONFIG macro returns the complete
// address of the configuration file in this case, this chunk of the code
// removes the address from the string, include a dot in rPath address and also
// rewrite pPath.
std::size_t found = callFile.find_last_of("/\\");

if (found != string::npos) {
  pPath = callFile.substr(0, found);
  callFile = callFile.substr(found + 1);
  std::string rPath = "./init_cond/";
}
std::string scriptLocation = pPath + rPath;
std::string addressScript = scriptLocation + "get_init.R";
std::string rSave = callFile;
std::string fullCommand =
    "Rscript " + addressScript + " " + rSave + " " + scriptLocation;
PLOG("\nCommand sent to system was: ");
PLOG(fullCommand.c_str());

// access values of parameters to feed into r and get the steady state values.

// NOTE: Keeping the same curs
// NOTE: Trying to make it prepared ofr NO_SEARCH flag
ECO = cur6 = SEARCHS(ROOT, "Countries");
SEC = cur1 = SEARCHS(ECO, "Sectors");
CG = cur = SEARCHS(SEC, "Firms");
GOV = cur2 = SEARCHS(ECO, "Government");
HHS = cur3 = SEARCHS(ECO, "Households");
CBK = cur4 = SEARCHS(ECO, "CentralBank");
BNK = cur5 = SEARCHS(ECO, "Banks");

// NOTE: Inputs for the SS solveing internally
double gK0 = VS(root, "gk0");
double infla = VS(ROOT, "inf0");
double u0 = VS(ROOT, "u0");
double K0 = VS(ROOT, "K0");
double nu = VS(SEC, "nu");
double Lab = VS(ROOT, "Nf0");
double gamma = VS(SEC, "gamma");
double iota = VS(SEC, "iota");
double h0 = VS(ROOT, "h0");
double Gamma = VS(GOV, "Gamma0");
double Lev = VS(ROOT, "lev0");
double Assets0 = VS(ROOT, "Assets0");
double r0 = VS(ROOT, "r0");
double thetal = VLS(CG, "Markup", 1);
double nk = VS(ROOT, "nk0");
double ibk = VS(ROOT, "ibk0");
double lambda = v[1] = VS(SEC, "lambda");
double tau2 = VS(ECO, "tau2");
double baserate = VS(CBK, "BaseInterestRate");
double etab = VS(BNK, "etab");
double gDebGDP = VS(ROOT, "gDebtGDP0");
double betaH = VS(ECO, "beta");
double alpha1 = VS(HHS, "alpha1");
double kappa = v[0] = VS(SEC, "kappa");
double ncg = v[2] = VS(CG, "n");
double Kfill = VS(root, "kFilledInit");
double etaf1 = V("etaf1");
double epsilon = VS(ECO, "epsilon");

exp_data(V("gk0"), scriptLocation, rSave, "gk");
exp_data(V("inf0"), scriptLocation, rSave, "inf");
exp_data(V("u0"), scriptLocation, rSave, "u");
exp_data(V("K0"), scriptLocation, rSave, "K");
exp_data(V("nu"), scriptLocation, rSave, "nu");
exp_data(V("Nf0"), scriptLocation, rSave, "Nf");
exp_data(V("gamma"), scriptLocation, rSave, "gamma");
exp_data(V("iota"), scriptLocation, rSave, "iota");
exp_data(V("h0"), scriptLocation, rSave, "h");
exp_data(V("Gamma0"), scriptLocation, rSave, "Gamma0");
exp_data(V("lev0"), scriptLocation, rSave, "lev");
exp_data(V("Assets0"), scriptLocation, rSave, "Assets");
exp_data(V("r0"), scriptLocation, rSave, "r");
exp_data(VL("Markup", 1), scriptLocation, rSave, "theta");
exp_data(V("nk0"), scriptLocation, rSave, "nk");
exp_data(V("ibk0"), scriptLocation, rSave, "ibk");
exp_data(V("lambda"), scriptLocation, rSave, "lambda");
exp_data(V("tau2"), scriptLocation, rSave, "tau2");
exp_data(V("BaseInterestRate"), scriptLocation, rSave, "i");
exp_data(V("etab"), scriptLocation, rSave, "etab");
exp_data(V("gDebtGDP0"), scriptLocation, rSave, "gDebtGDP");
exp_data(V("beta"), scriptLocation, rSave, "beta");
exp_data(V("alpha1"), scriptLocation, rSave, "alpha1");

// call R script in system
std::system(fullCommand.c_str());

double gss = imp_data("gss", scriptLocation, rSave);

double INV = imp_data("INV", scriptLocation, rSave);
double inv = INV / ncg;
double uD = imp_data("uT", scriptLocation, rSave);
double SCG = imp_data("S", scriptLocation, rSave);
double scg = SCG / ncg;
double LCG = imp_data("L", scriptLocation, rSave);
double lcg = LCG / ncg;
double pC = imp_data("P", scriptLocation, rSave);
double nSCG = pC * SCG;
double nscg = nSCG / ncg;
double DCG = imp_data("Df", scriptLocation, rSave);
double dcg = DCG / ncg;
double ms = 1 / ncg;
double w = imp_data("w", scriptLocation, rSave);
double ACG = imp_data("A", scriptLocation, rSave);
double ulc = w / ACG;
double etaf = imp_data("etaf", scriptLocation, rSave);
double k0 = K0 / ncg;
double WB = w * Lab;
double wb = WB / ncg;
double CF = nSCG - WB;
double cf = nscg - wb;

double BG = imp_data("B", scriptLocation, rSave);

double BH = imp_data("Bh", scriptLocation, rSave);
double DH = imp_data("Dh", scriptLocation, rSave);
double VH = imp_data("V", scriptLocation, rSave);
double alpha2 = imp_data("alpha2", scriptLocation, rSave);

double spread = imp_data("spread", scriptLocation, rSave);
double nB = V("nBanks");
double nwB = imp_data("nwB", scriptLocation, rSave);
double nwb = nwB / nB;
double LB = LCG;
double lb = LB / nB;
double BB = imp_data("Bb", scriptLocation, rSave);
double bb = BB / nB;
double DB = DCG + DH;
double db = DB / nB;
double icb = VS(CBK, "BaseInterestRate");
double rb = icb + spread;

double tau1 = imp_data("tau1", scriptLocation, rSave);

// Create capital vintage and loans outstanding objects.
ADDNOBJS(CG, "CapitalVintage", kappa - 1);   // add capital vintage objects
ADDNOBJS(CG, "OutstandingLoan", lambda - 1); // add loan outstanding objects

// Write lagged variables and parameters
WRITELLS(CG, "Inventory", inv, 0, 1);
WRITELLS(CG, "DesiredCapacityUtilisation", uD, 0, 1);
WRITES(SEC, "normalCapacity", uD);
WRITELLS(CG, "Sales", scg, 0, 1);
WRITELLS(CG, "TotalDebt", lcg, 0, 1);
WRITELLS(CG, "NominalSales", nscg, 0, 1);
WRITELLS(CG, "ExpectedSales", scg, 0, 1);
WRITELLS(CG, "FirmDemand", scg, 0, 1);
WRITELLS(CG, "FirmDeposits", dcg, 0, 1);
WRITELLS(CG, "MarketShare", ms, 0, 1);
WRITELLS(CG, "MarketShare", ms, 0, 2);
WRITELLS(CG, "UnitCost", ulc, 0, 1);
WRITELLS(CG, "Leverage", Lev, 0, 1);
WRITELLS(CG, "AvgLabourProductivity", ACG, 0, 1);

WRITES(SEC, "etaf", etaf);
WRITES(SEC, "initialMarkup", thetal);

WRITELLS(GOV, "GovernmentDebt", BG, 0, 1);

WRITELLS(HHS, "HouseholdDeposits", DH, 0, 1);
WRITELLS(HHS, "HouseholdWealth", VH, 0, 1);
WRITELLS(HHS, "HouseholdBills", BH, 0, 1);
WRITES(HHS, "alpha2", alpha2);

WRITES(BNK, "interestSpread", spread);
WRITELLS(BNK, "NetWorthBank", nwb, 0, 1);
WRITELLS(BNK, "LoanPortfolio", lb, 0, 1);
WRITELLS(BNK, "BillsBank", bb, 0, 1);
WRITELLS(BNK, "DepositsBank", db, 0, 1);
WRITELLS(BNK, "AverageBankInterestRate", rb, 0, 1);

WRITELS(ECO, "AggregateDebtFirms", LCG, 0);
WRITELLS(ECO, "Inflation", infla, 0, 1);
WRITELLS(ECO, "NominalWage", w, 0, 1);
WRITELLS(ECO, "PriceLevel", pC, 0, 1);
WRITELLS(ECO, "AggregateCapitalStock", K0, 0, 1);
WRITELLS(ECO, "AggregateCapacityUtilisation", uD, 0, 1);
WRITELLS(ECO, "AggregateInventories", INV, 0, 1);
WRITELLS(ECO, "AggregateFirmDeposits", DCG, 0, 1);
WRITELLS(ECO, "TotalLabour", Lab, 0, 1);
WRITELLS(ECO, "GrowthRateEmployment", gss, 0, 1);
WRITELLS(ECO, "AggregateGrowthProductivity", gK0, 0, 1);
WRITELLS(ECO, "AggregateProductivity", ACG, 0, 1);
WRITES(ECO, "tau1", tau1);
WRITES(ECO, "beta", betaH);

// Cycle in capital vintages and pass values
double Kfilled = v[7] = 1;
double KtoFill = v[8] = Kfill;
double pK = 0;
CYCLES(CG, cur2, "CapitalVintage") {
  if (KtoFill > 0) {
    double vintk = v[9] = k0 / pow(1 + gK0, Kfilled);
    WRITELLS(cur2, "CapitalQuantity", vintk, 0, 1);
    pK = v[10] = pC / pow((1 + infla), Kfilled - 1);
    WRITES(cur2, "VintagePrice", pK);
    WRITES(cur2, "VintageTime", T - Kfilled);
    WRITES(cur2, "scrapTime", kappa - Kfilled + 1);
  }
  Kfilled++;
  KtoFill--;
}
double knom = WHTAVES(CG, "CapitalQuantity", "VintagePrice");
WRITELLS(CG, "NominalCapitalStock", knom, 0, 1);

// normalise values (capital quantity) to total initial capital stock and fill
// technology variables (frontier and vintages' productivity)
double Kcg = v[10] = SUMLS(CG, "CapitalQuantity", 1);
double Krev = v[11] = 0;
double auxL = v[12] = 1 / pow(1 + gK0, 2);
double Knst = v[14] = MAXS(CG, "CapitalQuantity") / Kcg * k0;
double Aadj = v[15] =
    k0 * ACG * (1 + gK0) / Knst * (auxL - 1) / (pow(auxL, Kfill) - 1);
double Kcont = v[16] = 1;
WRITELLS(CG, "ACurrent", Aadj, 0, 1);
CYCLES(CG, cur2, "CapitalVintage") {
  double Kadj = v[9] = VLS(cur2, "CapitalQuantity", 1) / Kcg * k0;
  WRITELLS(cur2, "CapitalQuantity", Kadj, 0, 1);
  double Ak = v[17] = Kcont <= Kfill ? Aadj / pow(1 + gK0, Kcont) : 0;
  WRITES(cur2, "VintageProductivity", Ak);
  Krev += Kadj;
  Kcont++;
}

// Pass loan values to outstanding loan object
double Lpaid = v[21] = 0; // aggregator
for (int j = 0; j < lambda; j++) {
  double discount = v[22] = (lambda - j) / lambda;
  double auxLnom = v[23] = 1 / pow(1 + gss, j);
  Lpaid += discount * auxLnom;
}
double Lout0 = v[24] = lcg / Lpaid; // value of first loan oustanding
double countL = v[28] = 0;
double iLcg = v[38] = 0;
CYCLES(CG, cur2, "OutstandingLoan") {
  double Linit = v[29] =
      Lout0 / pow(1 + gss, countL);   // loan initially acquired
  double am = v[30] = Linit / lambda; // amortisation schedule
  double discountL = v[31] =
      (lambda - countL) / lambda; // discount factor of already repaid loans
  double Lout = v[32] =
      discountL * Linit; // outstanding loan by object's instance
  iLcg += Lout * rb;
  WRITELLS(cur2, "LoanOutstanding", Lout, 0, 1);
  WRITES(cur2, "LoanInterestRate", rb);
  WRITES(cur2, "AmortisationSchedule", am);
  WRITES(cur2, "PeriodsRemaining", lambda - countL);
  countL++;
}

// Dividend equality
double AM = v[35] = SUMS(CG, "AmortisationSchedule");
double DS = v[36] = AM + iLcg;  // total debt servicing
double ds_cf = v[40] = DS / cf; // debt servicing to cash flow ratio
double varepsilon = v[41] = log((2 * etaf1 - etaf) / etaf) / ds_cf;
WRITES(SEC, "varepsilon", varepsilon);
WRITELLS(CG, "DebtServicingToOperatingCashFlowRatio", ds_cf, 0, 1);

// bank interest rate parameter
double mu = v[43] = spread / ds_cf;

double idBank = v[15] = 1;
WRITES(BNK, "mu", mu);
WRITES(BNK, "idBank", i);
ADDHOOKS(BNK, ncg);
WRITES(BNK, "idBank", idBank);

ADDNOBJ_EXLS(SEC, "Firms", ncg - 1, CG,
             0); // add identical firms in each sector

// work some unique values in firm-level
i = 1; // initialise index for firms' ID
CYCLES(SEC, cur2, "Firms") {
  WRITES(cur2, "idFirm", i);

  ADDHOOKS(cur2, epsilon);
  WRITE_HOOKS(BNK, i - 1, cur2);
  WRITE_HOOKS(cur2, 0, BNK);
  WRITES(cur2, "BankID", idBank);
  i++;
  CYCLES(cur2, cur4, "OutstandingLoan") {
    WRITES(cur4, "BankSupplierID", idBank);
    WRITE_SHOOKS(cur4, BNK);
  }
}
PARAMETER;
RESULT(0)
