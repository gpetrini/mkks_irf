#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

EQUATION("BaseInterestRate")
/*
Shock to prime interest rate for impulse response function computation
*/

double shock = v[0] = VS(ROOT, "flagShock");
double Tshock = v[1] = VS(ROOT, "Tshock");
double r0 = v[2] = VS(CBK, "InitialInterestRate");
double rhoShock = v[3] = VS(CBK, "rhoShock");
double fullshock = v[4] = VS(CBK, "fullShock");
double rchg = v[5] = shock && Tshock == T;
double rincr = v[6] =
    rchg ? fullshock : pow((1 - rhoShock), T - Tshock) * fullshock;
double active = v[7] = (shock * (T >= Tshock));
double rnew = v[8] = r0 + (active * rincr);
rnew = max(rnew, 0);
RESULT(rnew)
