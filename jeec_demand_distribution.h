/******************************************************************************

        Demand distribution across firms

 ******************************************************************************/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

EQUATION("AveragePriceForCompetitiveness")
/*
Average price for competitiveness. Not to be confounded with the effective price
level, which is obtained ex post and depends on current market share. Level:
sector.
*/
v[0] = 0; // aggregator
CYCLE(cur, "Firms") {
  v[1] = VS(cur, "Price");
  v[2] = VLS(cur, "MarketShare", 1);
  v[0] += v[1] * v[2];
}
RESULT(v[0])

EQUATION("AverageUnfilledDemandForCompetitiveness")
/*
Average unfilled demand for competitiveness.
Level: sector.
*/
v[0] = 0; // aggregator
CYCLE(cur, "Firms") {
  v[1] = VLS(cur, "UnfilledDemand", 1);
  v[2] = VLS(cur, "MarketShare", 1);
  v[0] += v[1] * v[2];
}
RESULT(v[0])

EQUATION("Competitiveness")
/*
Firm index of competitiveness
Level: firm.
*/
v[0] = V("Price");
v[1] = VL("UnfilledDemand", 1);
v[2] = V("AveragePriceForCompetitiveness");
v[3] = VL("Sales", 1);
v[4] = VS(PARENT, "omega1");
v[5] = VS(PARENT, "omega2");
v[6] = v[0] / v[2];     // price component
v[7] = v[1] / v[3] + 1; // unfilled demand component
v[8] = v[3] == 0
           ? -v[4] * v[6]
           : -v[4] * v[6] -
                 v[5] * v[7]; // if the average unfilled demand is null, then
                              // disregard it in the competitiveness index.
RESULT(v[8])

EQUATION("AverageCompetitiveness")
/*
Average competitiveness of the firms' sector
Level: sector.
*/
v[0] = 0; // aggregator
CYCLE(cur, "Firms") {
  v[1] = VS(cur, "Competitiveness");
  v[2] = VLS(cur, "MarketShare", 1);
  v[0] += v[1] * v[2];
}
RESULT(v[0])

EQUATION("MarketShare")
/*
Market share calculation, from the replicator dynamics.
Level: firm.
*/
v[0] = V("Competitiveness");
v[1] = V("AverageCompetitiveness");
v[2] = V("chi");
v[3] = VL("MarketShare", 1);
v[4] = v[1] != 0 ? v[3] * (1 - v[2] * (v[0] - v[1]) / v[1]) : v[3];
if (t > 2 && V("EntryTime") == t - 2)
  v[4] = V("marketShareEntry");
RESULT(v[4])

EQUATION("NearZeroMarketShare")
/*
Check firms with near zero market share.
Write those to zero market share.
Then rebalance overall market shares to sum 1.
*/
v[0] = V("minimumMarketShareTolerance"); // parameter
v[1] = 0;                                // aggregator
v[4] = 0;                                // aggregator total market share
CYCLE(cur, "Firms") {
  v[2] = VS(cur, "MarketShare");
  v[3] = v[2] < v[0] ? true : false;
  v[4] += v[2];
  if (v[3] == 1 && V("EntryTime") != t - 1) {
    WRITES(cur, "MarketShare", 0);
    WRITES(cur, "flagExit", 1);
  }
  v[1] += v[3];
}
if (v[1] > 0 || v[4] != 1) {
  V("RebaseMarketShare");
}
RESULT(v[1])

EQUATION("RebaseMarketShare")
/*
Function to recompute market share in case of entry/exit, change in some market
share (near zero), or sum of market share != 1.
*/
v[0] = SUM("MarketShare");
CYCLE(cur, "Firms") {
  v[1] = VS(cur, "MarketShare");
  v[2] = v[1] / v[0];
  WRITES(cur, "MarketShare", v[2]);
}
RESULT(0)

EQUATION("Sales")
/*
Compute firm's sales. Updated afterwards if inventories are insufficient to fill
existing demand for each firm's goods. Level: firm.
*/
V("NearZeroMarketShare"); // force to run after rebasing of market share was run
                          // once.
v[0] = VS(GRANDPARENT, "AggregateDemand");
v[1] = V("MarketShare");
v[2] = v[0] * v[1];
WRITE("FirmDemand", v[2]); // demand faced by the firm. Used to compute
                           // production and investment plans

// check whether existing inventories fills the existing demand
v[3] = V("Inventory");
if (v[3] < v[2]) {
  v[4] = v[2] - v[3]; // unfilled demand
  WRITE("UnfilledDemand", v[4]);
  v[2] = v[3]; // update total sales
}
v[5] = v[3] - v[2];       // after-sales inventory
WRITE("Inventory", v[5]); // update inventory after-sales
RESULT(v[2])

/*============================== SUPPORT EQUATIONS
 * ===============================*/
EQUATION("UnfilledDemand")
/*
Unfilled demand (quantity). Updated when relevant (sales and remaining sales
distribution). Level: firm.
*/
RESULT(0)

EQUATION_DUMMY("FirmDemand", "Sales")

EQUATION("TotalUnfilledDemand")
/*
If there is a new entrant, total unfilled demand is calculated directly from the
aggregates.
*/
v[0] = SUM("UnfilledDemand");
v[1] = 0;
CYCLE(cur, "Firms") { v[1] += VS(cur, "EntryTime") == t - 1 ? 1 : 0; }
if (v[1] > 0) {
  v[2] = V("AggregateDemand");
  v[3] = SUM("Sales");
  v[0] = v[2] - v[3];
}
RESULT(v[0])

EQUATION("RevisionSales")
/*
Handle exceptions regarding sales' process.
Whole function executed only if triggering events happended.
Level: sector.
*/
v[0] = V("TotalUnfilledDemand");
v[9] = SUM("MarketShare");
if (v[0] ==
    0) // if some firm could not fill all demand, rebase sales and recalculate
       // market share. Otherwise, go ahead with the simulationion.
  END_EQUATION(0);
v[1] = SUM("Inventory");
if (v[1] == 0)
  END_EQUATION(0); // no inventories to distribute!
v[2] = V("AggregateDemand");
CYCLE(cur, "Firms") {
  v[3] = VS(cur, "Inventory"); // get post-sales updated inventory
  v[4] = v[3] / v[1];          // share of firm's inventory to total
  v[5] = v[4] * v[0];          // additional sales
  v[6] = VS(cur, "Sales");
  v[7] = v[5] + v[6];
  v[8] = v[7] / v[2];
  WRITES(cur, "Sales", v[7]);            // update total sales
  WRITES(cur, "MarketShare", v[8]);      // update market share
  WRITES(cur, "Inventory", v[3] - v[5]); // update inventory
}
RESULT(1)

EQUATION("NominalSales")
/*
Nominal sales of firms.
Run after revision sales.
Level: firm.
*/
VS(PARENT, "RevisionSales");
v[0] = V("Price");
v[1] = V("Sales");
RESULT(v[0] * v[1])
