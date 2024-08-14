#ifndef INTERNALS
#include "./src/fun_head.h" // NOTE: So emacs can recognize internal funcions
#endif

EQUATION("BaseInterestRate")

double rT = v[0] = VS(CBK, "nominal_target_rate");

if (T < 4) {
  END_EQUATION(rT);
}

double r0 = v[1] = CURRENT;
double rshock0 = v[2] = VLS(CBK, "rShock", 1);
double rNoShock = v[3] = r0 - rshock0;
double rAdj = v[4] = VS(CBK, "interest_adjustment_step");

double infT = v[5] = VS(CBK, "target_inflation");
double capT = v[6] = VS(CBK, "target_capacity");
double levT = v[7] = VS(CBK, "target_leverage");

double sens_inf = v[8] = VS(CBK, "cb_sensib_inflation");
double sens_cap = v[9] = VS(CBK, "cb_sensib_capacity");
double sens_lev = v[10] = VS(CBK, "cb_sensib_leverage");

double infl = v[11] = MAVELS(ECO, "Inflation", 4, 1);
double dinf = v[12] = (infl - infT);
double cap = v[13] = VLS(ECO, "AggregateCapacityUtilisation", 1);
double dcap = v[14] = (cap - capT);
double lev = v[15] = VLS(ECO, "AggregateLeverage", 1);
double dlev = v[16] = (lev - levT);

double taylor = v[17] =
    rT + sens_inf * (dinf) + sens_cap * (dcap) + sens_lev * (dlev);

double rnew = rNoShock;
// Discretize adjustment rate;
if (fabs(rNoShock - taylor) > 2 * rAdj) {
  rnew += (taylor > rNoShock) ? 2 * rAdj : -2 * rAdj;
} else {
  if (fabs(rNoShock - taylor) > rAdj) {
    rnew += (taylor > rNoShock) ? 1 * rAdj : -1 * rAdj;
  }
}

double rshock = v[5] = VS(CBK, "rShock");
rnew = max(rnew, 0);
rnew += rshock;
RESULT(rnew)

EQUATION("rShock")
/*
Shock to prime interest rate for impulse response function computation
*/

double shock = v[0] = VS(ROOT, "flagShock");
double Tshock = v[1] = VS(ROOT, "Tshock");
double rhoShock = v[2] = VS(CBK, "rhoShock");
double fullshock = v[3] = VS(CBK, "fullShock");
double rchg = v[4] = shock && Tshock == T;
double rincr = v[5] =
    rchg ? fullshock : pow((1 - rhoShock), T - Tshock) * fullshock;
double active = v[6] = (shock * (T >= Tshock));
double rshock = v[7] = (active * rincr);

RESULT(rshock)
