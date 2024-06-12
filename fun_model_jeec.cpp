// SCRIPT FOR THE MODEL DEVELOPED IN THE PAPER
// To what extent does aggregate leverage determine financial fragility? New
// insights from an agent-based stock-flow consistent model WRITTEN BY ITALO
// PEDROSA

// #define EIGENLIB				// uncomment to use Eigen linear
// algebra library #define NO_POINTER_INIT	// uncomment to disable pointer
// checking
#define INTERNALS 1 // NOTE: For autocompetion with dabrev
#define USE_SEARCH

#include "fun_head_fast.h"
#include <fstream>
#include <list>
#include <memory>
#include <numeric>
#include <stdlib.h>
#include <vector>

object *ECO;
object *HHS;
object *SEC;
object *BNK;
object *GOV;
object *CBK;
object *SFC; // NOTE: Not used yet
object *SHK; // NOTE: Not used yet

object *CG; // NOTE: Points only at the first object

// function to import text file with one value
// imp_data function was initially with auto mytxt
double imp_data(std::string mytxt, std::string dir, std::string sim) {
  double value;
  ifstream inFile;
  std::string file = dir + sim + "_" + mytxt + ".txt";
  inFile.open(file);
  inFile >> value;
  return value;
}

void exp_data(double value, std::string folder, std::string calF,
              std::string fileName) {
  ofstream outFile;
  outFile.open(folder + calF + "_" + fileName + ".txt");
  outFile << value;
  outFile << "\n";
  outFile.close();
}

// create vector to store historical data of employment growth
vector<double> growthEmployment;

// function obtain the nth percentile of a double vector
double ntile_vector(vector<double> a, float ntile) {
  int n = a.size();
  sort(a.begin(), a.end());
  double index = ntile * n;
  double result;

  // check if the index is a whole number.
  if (floor(index) == index) {
    result = (a[index - 1] + a[index]) / 2;
  } else {
    index = n == 1 ? ceil(index) : round(index);
    result = a[index - 1];
  }

  return (result);
}

MODELBEGIN

// The equation files below contains all LSD equations, organized by the
// container object.

#include "jeec_initialisation.h" // Model inicialisation

#include "central_bank.h" // Monetary policy shock

#include "jeec_aggregate.h" // Equations for aggregate data

#include "jeec_banks.h" // Equations for banks

#include "jeec_firms_capital.h" // Manage capital stock and investment of firms

#include "jeec_firms_financial.h" // Manage financial implications of firms' decisions

#include "jeec_firms_production.h" // Manage production decisions of firms

#include "jeec_firms_rd.h" // Manage decisions related to R&D

#include "jeec_government.h" // Manage equations related to the government

#include "jeec_households.h" // Manage equations related households/workers

#include "jeec_labour_market.h" // Manage equations related labour market

#include "jeec_demand_distribution.h" // Manage equations related to the demand distribution

#include "jeec_entry_exit.h" // Manage equations related to the entry/exit of firms

#include "jeec_checks.h" // Manage equations with model's checks

#include "jeec_save.h" // Manage equations with model's saved output

MODELEND

// do not add Equations in this area

void close_sim(void) {
  // close simulation special commands go here
}
