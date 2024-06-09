/******************************************************************************

        Firm object equations
        ----------------------

        Subset of equations related to technological dynamics.

 ******************************************************************************/

/*============================== KEY EQUATIONS ===============================*/

#ifndef INTERNALS
#include "./src/fun_head_fast.h" // NOTE: So emacs can recognize internal funcions
#endif

EQUATION("TechnologicalFrontier")
/*
Idenfication of the firm in the technological frontier
Level: sector.
*/
v[0] = MAX("ACurrent");
RESULT(v[0])

EQUATION("Imitation")
/*
Firm imitation routine.
Level: firms.
*/
v[1] = VS(PARENT, "zeta2");   // firm search capabilities parameter
v[2] = V("ImitationWorkers"); // retrieve number of imitation workers
v[3] = 1 - exp(-v[1] * v[2]); // probability of imitation
v[4] = binomial(v[3], 1);     // random draw of a binomial distribution, with
                              // probability of innovation given above
if (v[4]) {
  v[5] = VL("ACurrent", 1);
  v[7] = 0; // Initialise sum of probabilities (for speed)
  v[6] = V("idFirm");
  CYCLES(PARENT, cur, "Firms") // compute the Euclidian distance between a firm
                               // best-known technologies to the others' techs
  {
    v[8] = VLS(cur, "ACurrent", 1); // retrieve other firms' technology (A)
    v[9] = pow((v[5] - v[8]), 2);   // distance A (squared)
    v[10] = pow(v[9], 0.5);         // square root of squared distance
    v[11] = v[10] != 0 ? 1 / v[10]
                       : 1e-40; // invert scale of probability (the farther, the
                                // less likely to be randomly chosen).
    WRITES(cur, "probImitation", v[11]); // rewrite the probability of imitation
    v[7] += v[11];
  }
  cur = RNDDRAW_TOTS(PARENT, "Firms", "probImitation",
                     v[7]);        // draw a firm to copy the technology
  v[12] = VLS(cur, "ACurrent", 1); // copy A
} else
  v[12] = 0;
RESULT(v[12])

EQUATION("Innovation")
/*
Firm innovation routine.
*/
v[0] = VS(PARENT, "zeta1");
v[1] = V("InnovationWorkers");
v[2] = VS(PARENT, "betaDist1");
v[3] = VS(PARENT, "betaDist2");
v[4] = VS(PARENT, "betaMin");
v[5] = VS(PARENT, "betaMax");
v[6] = 1 - exp(-v[0] * v[1]); // Probability of innovation
v[7] = binomial(v[6], 1);     // Draw of a binomial distribution, with the
                              // probability of innovation given above
v[8] = beta(v[2], v[3]); // Random draw of a beta distribution. Value may be
                         // either positive or negative
v[9] = v[8] - v[2] / (v[2] + v[3]);     // Normalise the draw to zero
v[10] = v[9] * (abs(v[4]) + abs(v[5])); // Rescale on support (betaMin, betaMax)
v[11] = VL("ACurrent", 1);
v[12] = v[11] * (1 + v[7] * v[10]); // New technology = old (best-known)
                                    // technology times 1 + random draw.
v[13] = v[12] == v[11] ? 0 : v[12];
RESULT(v[13])

EQUATION("ACurrent")
/*
Technology adopted at the end of the period. Available in t + 1.
*/
v[1] = V("Innovation");   // innovation A
v[2] = V("Imitation");    // imitation A
v[3] = VL("ACurrent", 1); // old A
v[4] = max(v[1], v[2]);   // better tech
v[5] = max(v[3], v[4]);
RESULT(v[5])

EQUATION("RDExpenditure")
/*
R&D nominal expenditure.
*/
v[0] = VL("NominalSales", 1); // Lagged nominal sales of the firm
v[1] = VS(PARENT, "gamma");   // R&D investment propensity
v[2] = v[0] * v[1];
RESULT(v[2])

EQUATION("InnovationWorkers")
/*
Number of workers working in innovation.
*/
v[0] = VL("TechnologicalFrontier", 1); // All firms
v[1] = VL("ACurrent", 1);              // Firm-specific
v[2] = VS(PARENT, "xi");               // R&D allocation to innovative search
v[3] = V("WorkersRD");                 // Number of R&D Workers
v[4] = v[2] * v[3];                    // Number of workers in Innovation
if (v[0] == v[1]) // If the firm is in the technological frontier, all the
                  // spending goes to innovation
{
  v[4] = v[3];
}
RESULT(v[4])

EQUATION("ImitationWorkers")
/*
Number of workers working in imitation.
*/
v[0] = V("WorkersRD");
v[1] = V("InnovationWorkers");
RESULT(v[0] - v[1])

/*
EQUATION( "AvgImitationSuccess" )
        v[0] = 0;
// accumulator success weighted by id v[3] = COUNT( "Firms" ); CYCLE( cur,
"Firms" )
        {
                v[1] = VS( cur, "Imitation" );
                if ( v[1] != 0 )
                        v[0] += VS( cur, "idFirm" );
        }
RESULT( v[0] / v[3] )

EQUATION( "AvgInnovationSuccess" )
        v[0] = 0;
// accumulator success weighted by id v[3] = COUNT( "Firms" ); CYCLE( cur,
"Firms" )
        {
                v[1] = VS( cur, "Innovation" );
                if ( v[1] != 0 )
                        v[0] += VS( cur, "idFirm" );
        }
RESULT( v[0] / v[3] )

EQUATION( "AvgImitationSuccessIncrease" )
        v[0] = 0;
// accumulator success weighted by id v[3] = COUNT( "Firms" ); CYCLE( cur,
"Firms" )
        {
                v[1] = VS( cur, "Imitation" );
                v[2] = VLS( cur, "ACurrent", 1 );
                if ( v[1] > v[2] )
                        v[0] += VS( cur, "idFirm" );
        }
RESULT( v[0] / v[3] )


EQUATION( "AvgInnovationSuccessIncrease" )
        v[0] = 0;
// accumulator success weighted by id v[3] = COUNT( "Firms" ); CYCLE( cur,
"Firms" )
        {
                v[1] = VS( cur, "Innovation" );
                v[2] = VLS( cur, "ACurrent", 1 );
                if ( v[1] > v[2] )
                        v[0] += VS( cur, "idFirm" );
        }
RESULT( v[0] / v[3] )
*/
