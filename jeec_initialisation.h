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

// Create capital vintage and loans outstanding objects.
//
//
//
// NOTE: Keeping the same curs
// NOTE: Trying to make it prepared ofr NO_SEARCH flag
ECO = cur6 = SEARCHS(ROOT, "Countries");
SEC = cur1 = SEARCHS(ECO, "Sectors");
CG = cur = SEARCHS(SEC, "Firms");
GOV = cur2 = SEARCHS(ECO, "Government");
HHS = cur3 = SEARCHS(ECO, "Households");
CBK = cur4 = SEARCHS(ECO, "CentralBank");
BNK = cur5 = SEARCHS(ECO, "Banks");

v[0] = VS(cur1, "kappa");
v[1] = VS(cur1, "lambda");
ADDNOBJS(cur, "CapitalVintage", v[0] - 1);  // add capital vintage objects
ADDNOBJS(cur, "OutstandingLoan", v[1] - 1); // add loan outstanding objects

// Write lagged variables and parameters
v[2] = VS(cur, "n");

WRITELLS(cur, "Inventory", imp_data("INV", scriptLocation, rSave) / v[2], 0, 1);
WRITELLS(cur, "DesiredCapacityUtilisation",
         imp_data("uT", scriptLocation, rSave), 0, 1);
WRITES(cur1, "normalCapacity", imp_data("uT", scriptLocation, rSave));

WRITELLS(cur, "Sales", imp_data("S", scriptLocation, rSave) / v[2], 0, 1);
// PLOG( "\n" );
// PLOG( "%f", VLS( cur, "Sales", 1 ) );
WRITELLS(cur, "TotalDebt", imp_data("L", scriptLocation, rSave) / v[2], 0, 1);
WRITELLS(cur, "NominalSales",
         imp_data("S", scriptLocation, rSave) / v[2] *
             imp_data("P", scriptLocation, rSave),
         0, 1);
WRITELLS(cur, "ExpectedSales", imp_data("S", scriptLocation, rSave) / v[2], 0,
         1);
WRITELLS(cur, "FirmDemand", imp_data("S", scriptLocation, rSave) / v[2], 0, 1);
WRITELLS(cur, "FirmDeposits", imp_data("Df", scriptLocation, rSave) / v[2], 0,
         1);
WRITELLS(cur, "MarketShare", 1 / v[2], 0, 1);
WRITELLS(cur, "MarketShare", 1 / v[2], 0, 2);
WRITELLS(cur, "UnitCost",
         imp_data("w", scriptLocation, rSave) /
             imp_data("A", scriptLocation, rSave),
         0, 1);
WRITELLS(cur, "Leverage", VS(root, "lev0"), 0, 1);
WRITELLS(cur, "AvgLabourProductivity", imp_data("A", scriptLocation, rSave), 0,
         1);
WRITES(cur1, "etaf", imp_data("etaf", scriptLocation, rSave));
WRITES(cur1, "initialMarkup", VLS(cur, "Markup", 1));
WRITELLS(cur2, "GovernmentDebt", imp_data("B", scriptLocation, rSave), 0, 1);
WRITELLS(cur3, "HouseholdDeposits", imp_data("Dh", scriptLocation, rSave), 0,
         1);
WRITELLS(cur3, "HouseholdWealth", imp_data("V", scriptLocation, rSave), 0, 1);
WRITELLS(cur3, "HouseholdBills", imp_data("Bh", scriptLocation, rSave), 0, 1);
WRITES(cur3, "alpha2", imp_data("alpha2", scriptLocation, rSave));
WRITES(cur5, "interestSpread", imp_data("spread", scriptLocation, rSave));
WRITELLS(cur5, "NetWorthBank",
         imp_data("nwB", scriptLocation, rSave) / V("nBanks"), 0, 1);
WRITELLS(cur5, "LoanPortfolio",
         imp_data("L", scriptLocation, rSave) / V("nBanks"), 0, 1);
WRITELLS(cur5, "BillsBank", imp_data("Bb", scriptLocation, rSave) / V("nBanks"),
         0, 1);
WRITELLS(cur5, "DepositsBank",
         (imp_data("Df", scriptLocation, rSave) +
          imp_data("Dh", scriptLocation, rSave)) /
             V("nBanks"),
         0, 1);
WRITELLS(cur5, "AverageBankInterestRate",
         VS(cur4, "BaseInterestRate") +
             imp_data("spread", scriptLocation, rSave),
         0, 1);

CYCLE(cur, "Countries") {
  WRITELS(cur, "AggregateDebtFirms", imp_data("L", scriptLocation, rSave), 0);
  WRITELLS(cur, "Inflation", VS(root, "inf0"), 0, 1);
  WRITELLS(cur, "NominalWage", imp_data("w", scriptLocation, rSave), 0, 1);
  WRITELLS(cur, "PriceLevel", imp_data("P", scriptLocation, rSave), 0, 1);
  WRITELLS(cur, "AggregateCapitalStock", VS(root, "K0"), 0, 1);
  WRITELLS(cur, "AggregateCapacityUtilisation",
           imp_data("uT", scriptLocation, rSave), 0, 1);
  WRITELLS(cur, "AggregateInventories", imp_data("INV", scriptLocation, rSave),
           0, 1);
  WRITELLS(cur, "AggregateFirmDeposits", imp_data("Df", scriptLocation, rSave),
           0, 1);
  WRITELLS(cur, "TotalLabour", VS(root, "Nf0"), 0, 1);
  WRITELLS(cur, "GrowthRateEmployment", imp_data("gss", scriptLocation, rSave),
           0, 1);
  WRITELLS(cur, "AggregateGrowthProductivity", VS(root, "gk0"), 0, 1);
  WRITELLS(cur, "AggregateProductivity", imp_data("A", scriptLocation, rSave),
           0, 1);
  WRITES(cur, "tau1", imp_data("tau1", scriptLocation, rSave));
  WRITES(cur, "beta", imp_data("beta", scriptLocation, rSave));

  // import external initial values and replace them into model's variables
  cur1 = SEARCH("Firms");
  v[0] = VS(cur1, "n");

  // initialise capital vintages' values
  v[1] = VS(root, "K0") / v[0];
  v[2] = VS(root, "kFilledInit"); // number of capital vintages to create to be
                                  // != 0 capital quantity
  v[3] = VS(root, "gk0");
  v[4] = 0; // aggregator
  v[5] = imp_data("P", scriptLocation, rSave);
  v[6] = VS(root, "inf0");

  // Cycle in capital vintages and pass values
  v[7] = 1;
  v[8] = v[2];
  CYCLES(cur1, cur2, "CapitalVintage") {
    if (v[8] > 0) {
      v[9] = v[1] / pow(1 + v[3], v[7]);
      WRITELLS(cur2, "CapitalQuantity", v[9], 0, 1);
      v[10] = v[5] / pow((1 + v[6]), v[7] - 1);
      WRITES(cur2, "VintagePrice", v[10]);
      WRITES(cur2, "VintageTime", T - v[7]);
      WRITES(cur2, "scrapTime", V("kappa") - v[7] + 1);
    }
    v[7]++;
    v[8]--;
  }
  WRITELLS(cur1, "NominalCapitalStock",
           WHTAVES(cur1, "CapitalQuantity", "VintagePrice"), 0, 1);

  // normalise values (capital quantity) to total initial capital stock and fill
  // technology variables (frontier and vintages' productivity)
  v[10] = SUMLS(cur1, "CapitalQuantity", 1);
  v[11] = 0;
  v[12] = 1 / pow(1 + v[3], 2);
  v[13] = imp_data("A", scriptLocation, rSave);
  v[14] = MAXS(cur1, "CapitalQuantity") / v[10] * v[1];
  v[15] =
      v[1] * v[13] * (1 + v[3]) / v[14] * (v[12] - 1) / (pow(v[12], v[2]) - 1);
  v[16] = 1;
  WRITELLS(cur1, "ACurrent", v[15], 0, 1);
  CYCLES(cur1, cur2, "CapitalVintage") {
    v[9] = VLS(cur2, "CapitalQuantity", 1) / v[10] * v[1];
    WRITELLS(cur2, "CapitalQuantity", v[9], 0, 1);
    v[17] = v[16] <= v[2] ? v[15] / pow(1 + v[3], v[16]) : 0;
    WRITES(cur2, "VintageProductivity", v[17]);
    v[11] += v[9];
    v[16]++;
  }

  // Pass loan values to outstanding loan object
  v[18] =
      imp_data("L", scriptLocation, rSave) / v[0]; // total debt of each firm
  v[19] = (1 + v[3]) * (1 + v[6]) - 1; // initial condition nominal gdp growth
  v[20] = V("lambda");
  v[21] = 0; // aggregator
  for (int j = 0; j < v[20]; j++) {
    v[22] = (v[20] - j) / v[20];
    v[23] = 1 / pow(1 + v[19], j);
    v[21] += v[22] * v[23];
  }
  v[24] = v[18] / v[21]; // value of first loan oustanding
  v[25] = V("BaseInterestRate");
  v[26] = imp_data("spread", scriptLocation, rSave);
  v[27] = v[25] + v[26];
  v[28] = 0;
  v[38] = 0;
  CYCLES(cur1, cur2, "OutstandingLoan") {
    v[29] = v[24] / pow(1 + v[19], v[28]); // loan initially acquired
    v[30] = v[29] / v[20];                 // amortisation schedule
    v[31] = (v[20] - v[28]) / v[20]; // discount factor of already repaid loans
    v[32] = v[31] * v[29];           // outstanding loan by object's instance
    v[38] += v[32] * v[27];
    WRITELLS(cur2, "LoanOutstanding", v[32], 0, 1);
    WRITES(cur2, "LoanInterestRate", v[27]);
    WRITES(cur2, "AmortisationSchedule", v[30]);
    WRITES(cur2, "PeriodsRemaining", v[20] - v[28]);
    v[28]++;
  }

  // Dividend equality
  v[33] = VS(cur1, "etaf");
  v[34] = V("etaf1");
  v[35] = SUMS(cur1, "AmortisationSchedule");
  v[36] = v[35] + v[38];                                 // total debt servicing
  v[37] = VL("NominalWage", 1) * VS(root, "Nf0") / v[0]; // wage bill
  v[39] = VLS(cur1, "NominalSales", 1) - v[37];          // operating cash flow
  v[40] = v[36] / v[39]; // debt servicing to cash flow ratio
  v[41] = log((2 * v[34] - v[33]) / v[33]) / v[40];
  WRITES(cur1->up, "varepsilon", v[41]);
  WRITELLS(cur1, "DebtServicingToOperatingCashFlowRatio", v[40], 0, 1);

  // bank interest rate parameter
  v[42] = V("interestSpread");
  v[43] = v[42] / v[40];

  CYCLES(cur, cur1, "Sectors") {
    v[0] = VS(cur1, "n") - 1; // number of firms to be added in sector
    v[1] = MINS(cur1, "idFirm");
    cur2 = SEARCH_CNDS(cur1, "idFirm", v[1]);
    ADDNOBJ_EXLS(cur1, "Firms", v[0], cur2,
                 0); // add identical firms in each sector

    // work some unique values in firm-level
    int i = 1; // initialise index for firms' ID
    CYCLES(cur1, cur2, "Firms") {
      WRITES(cur2, "idFirm", i);
      i++;
    }
  }

  // create banks
  v[5] = VS(cur, "nBanks");
  ADDNOBJS(cur, "Banks", v[5] - 1);
  i = 1;
  CYCLES(cur, cur1, "Banks") {
    WRITES(cur1, "mu", v[43]);
    WRITES(cur1, "idBank", i);
    i++;
  }
}

CYCLE(cur, "Countries") {

  // initialise network of firms and banks (per sector)
  v[11] = V("nBanks");
  CYCLES(cur, cur1, "Sectors") {
    v[12] = VS(cur1, "n");
    v[13] = v[12] / v[11];     // number of firms per bank
    if (v[13] != floor(v[13])) // is the number of firms per bank an integer?
    {                          // no
      plog("\nERROR: program was terminated.\nNumber of firms per bank is not "
           "an integer.\nMake sure N Firms divided by N Banks is an integer in "
           "all sectors.");
      std::terminate();
    }
    v[14] = 0;
    v[15] = 1;
    CYCLES(cur1, cur2, "Firms") {
      v[14] = floor(v[14]) == 0 ? v[13] : v[14];
      cur3 = SEARCH_CND("idBank", v[15]);
      ADDHOOKS(cur2, VS(cur, "epsilon"));
      WRITE_HOOKS(cur2, 0, cur3);
      WRITES(cur2, "BankID", v[15]);
      // id the object "OutstandingLoan" with bankID
      CYCLES(cur2, cur4, "OutstandingLoan") {
        WRITES(cur4, "BankSupplierID", v[15]);
        WRITE_SHOOKS(cur4, cur3);
      }
      v[14] -= 1;
      if (v[14] == 0)
        v[15]++;
    }
  }

  // initialise hooks for banks (number of firms)
  v[16] = 0;
  CYCLES(cur, cur1, "Sectors") { v[16] += VS(cur1, "n"); }
  CYCLES(cur, cur1, "Banks") {
    ADDHOOKS(cur1, v[16]);
    for (i = 0; i < COUNT_HOOKS(cur1); i++) {
      cur2 = SEARCH_CND("idFirm", i + 1);
      WRITE_HOOKS(cur1, i, cur2);
      // WRITELLS( cur2, "BankID", i, T, 1 );
    }
  }
}
PARAMETER;
RESULT(0)
