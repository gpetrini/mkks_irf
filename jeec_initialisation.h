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
double baserate = VS(CBK, "InitialInterestRate");
double etab = VS(BNK, "etab");
double gDebtGDP = VS(ROOT, "gDebtGDP0");
double betaH = VS(ECO, "beta");
double alpha1 = VS(HHS, "alpha1");
double kappa = v[0] = VS(SEC, "kappa");
double ncg = v[2] = VS(CG, "n");
double Kfill = VS(root, "kFilledInit");
double etaf1 = V("etaf1");
double epsilon = VS(ECO, "epsilon");

// NOTE: Solving SS system

double spread = ibk - baserate;
double LCG = Lev * Assets0;
double G = (Gamma * K0) / (1 + gK0);
double uT = u0 - 0.02;
double Yp = K0 / (nu * (1 + gK0));
double Y = Yp * u0;
double ICG = h0 * Y;
double DeltaReal = (gK0) / (1 + gK0);
double SCG = (Y / (1 + DeltaReal * iota));
double INV = iota * SCG;
double C = SCG - ICG - G;
double RD = gamma * SCG / (1 + gK0);
double ACG = (u0 * K0 + nu * RD * (1 + gK0)) / (nu * Lab);
double gss = (1 + gK0) * (1 + infla) - 1;

double sumAm_cg = 0;
for (int j = 1; j <= lambda; j++) {
  double lag = 1 / pow(1 + gss, j);
  sumAm_cg += lag;
}
double sumAm = sumAm_cg;
double ra = (gss * sumAm) / (lambda - sumAm);
double AM0 = (gss + ra) / (lambda * (1 + gss)) * LCG * sumAm;
// Begin BLOCK 16
// NOTE: Using values not far from the solution as a initial guess
double P = 0.05;
double w = .5;
double ucf = .35;
double DCG = 25000;
double tol = 1e-06;
for (i = 0; i < 1e05; i++) {
  // NOTE: Store old updates
  double P0 = P;
  double w0 = w;
  double ucf0 = ucf;
  double DCG0 = DCG;
  // NOTE: restore errors
  double err = 0;

  // NOTE: Writing the system
  P = (Assets0 - DCG) / (r0 * K0 + INV / (1 + thetal));
  ucf = P / (1 + thetal);
  w = ucf * ACG;
  DCG = (P * ICG + w * Lab - AM0 - LCG * (gss / (1 + gss))) * (1 + gss);

  err += fabs(P - P0);
  err += fabs(w - w0);
  err += fabs(ucf - ucf0);
  err += fabs(DCG - DCG0);

  if (err < tol) {
    break;
  }
}
// End BLOCK 16
double BG = gDebtGDP * P * Y;
double DepKN = (nk * P * K0 * r0) / (1 + gss);
double piG = P * SCG - w * Lab - ibk * LCG / (1 + gss);
double piN = piG + (gss * ucf * INV) / (1 + gss) - DepKN;
double piDF = gss * (LCG - DCG) / (1 + gss) + piG - tau2 * piN - P * ICG;

// Begin BLOCK for 22
double piB = 1e04;
double BH = 1e04;
double BB = 5e03;
double tau1 = .3;
double alpha2 = .05;
double VH = 1e04;
double DH = 1e03;
double piDB = 1e03;

for (i = 0; i < 1e05; i++) {
  // NOTE: Store old updates
  double piB0 = piB;
  double piDB0 = piDB;
  double DH0 = DH;
  double BH0 = BH;
  double BB0 = BB;
  double tau10 = tau1;
  double VH0 = VH;
  double alpha20 = alpha2;

  // NOTE: restore errors
  double err = 0;

  // NOTE: Writing the system
  piB = (ibk * LCG + baserate * BB) / (1 + gss);
  piDB = etab * (1 - tau2) * piB;
  BH = VH - DH;
  BB = BG - BH;
  DH = betaH * VH;
  tau1 = (P * G - tau2 * (piB + piN) - BG * (gss - baserate) / (1 + gss)) /
         (w * Lab + piDF + piDB + baserate * BH / (1 + gss));
  VH = ((1 - alpha1) * (1 - tau1) * w * Lab +
        (1 - tau1) * (piDF + piDB + baserate * BH / (1 + gss))) *
       (1 + gss) / (gss + alpha2);
  alpha2 = (P * C - alpha1 * (1 - tau1) * w * Lab) * (1 + gss) / VH;

  err += fabs(piB0 - piB);
  err += fabs(piDB0 - piDB);
  err += fabs(DH0 - DH);
  err += fabs(BH0 - BH);
  err += fabs(BB0 - BB);
  err += fabs(tau10 - tau1);
  err += fabs(VH0 - VH);
  err += fabs(alpha20 - alpha2);

  if (err < tol) {
    break;
  }
}
// END BLOCK for 22

double nwB = ((1 - tau2) * piB - piDB) * (1 + gss) / (gss);
double etaf = piDF / ((1 - tau2) * piN);

double inv = INV / ncg;
double uD = uT;
double scg = SCG / ncg;
double lcg = LCG / ncg;
double pC = P;
double nSCG = pC * SCG;
double nscg = nSCG / ncg;
double dcg = DCG / ncg;
double ms = 1 / ncg;
double ulc = w / ACG;
double k0 = K0 / ncg;
double WB = w * Lab;
double wb = WB / ncg;
double CF = nSCG - WB;
double cf = nscg - wb;

double nB = V("nBanks");
double nwb = nwB / nB;
double LB = LCG;
double lb = LB / nB;
double bb = BB / nB;
double DB = DCG + DH;
double db = DB / nB;
double icb = VS(CBK, "BaseInterestRate");
double rb = icb + spread;

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

WRITELLS(CBK, "BaseInterestRate", baserate, 0, 1);
WRITES(CBK, "BaseInterestRate", baserate);
WRITES(CBK, "nominal_target_rate", baserate);

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
WRITELLS(ECO, "AggregateLeverage", Lev, 0, 1);
WRITELLS(ECO, "Inflation", infla, 0, 1);
// NOTE: For MAVE
WRITELLS(ECO, "Inflation", infla, 0, 2);
WRITELLS(ECO, "Inflation", infla, 0, 3);
WRITELLS(ECO, "Inflation", infla, 0, 4);
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
