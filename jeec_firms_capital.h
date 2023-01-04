/******************************************************************************

	Firm object equations
	----------------------

	Equations that are specific to the firms in the model.
	Subset of equations related to capital goods.
	Technology, accumulation, destruction and handling.
 
 ******************************************************************************/

/*============================== KEY EQUATIONS ===============================*/
EQUATION( "PlannedExpansionInvestment" )
/*
Expansion investment - equation 11 of the paper. 
Level: firm.
*/
		v[0] = VS( PARENT, "gamma0" );			
		v[1] = VS( PARENT, "gammau" );
		v[2] = V( "DesiredCapacityUtilisation" );
		v[3] = VS( PARENT, "normalCapacity" );
		v[4] = V( "AvailableCapitalStock" );
		v[5] = VS( PARENT, "maxGrowthCapacity" ); 	
		v[6] = v[0] + v[1] * ( v[2] - v[3] );								// Desired growth rate of capital stock
		if ( v[6] < 0 )														// Expansionary investment cannot be negative
		{
				WRITE( "ShrinkCapacity", v[6] * v[4] );						// Write the amount that firms want to shrink capacity
				v[6] = 0;
		}
		else
				WRITE( "ShrinkCapacity", 0 );				        
		if ( v[6] > v[5] )													// Check if the desired growth of capital stock exceed the maximum growth of physical capital
		{
				v[6] = v[5];                  
		}	
		v[7] = v[6] * v[4];													// Desired level of capital accumulation
RESULT( v[7] ) 

EQUATION( "ExpansionInvestment" )
/*
Actual expansion investment considers the cash available for firms, post-credit market interaction. 
Level: firm.
*/
	v[0] = V( "FundsExpansionInvestment" );
	v[1] = V( "ExpectedPrice" );
	v[2] = V( "PlannedExpansionInvestment" );
	v[3] = v[0] / v[1];
	v[4] = min( v[3], v[2] );
	v[5] = max( v[4], 0 );
RESULT( v[5] )

EQUATION( "FunctionReplacementInvestment" )
/*
Function to manage firms' replacement investment.
It's called once when there is no credit rationing.
It may be called multiple times otherwise.
Level: firm.
*/
	if ( t == 1 ) 												// workaround to update first instance's of capital vintage object. 								
	{															// as it's required in the initialisation, object's variables are calculated in advance. With this device, a recomputation is forced only in the first period.
		CYCLE( cur, "CapitalVintage" )
		{
			RECALCS( cur, "UnitCostVintage" );
			RECALCS( cur, "PaybackReplacementVintage" );
		}
	}

	v[0] = VS( PARENT, "payback" ); 							// Number of periods of payback of replacement investments
	v[1] = V( "ExpectedPrice" );								// Expected price	
	v[2] = 0;													// Initialise counter of total replacement investment
	v[3] = V( "maximumReplacementInvestmentReal" ); 			// Maximum real replacement investment
	v[4] = V( "ShrinkCapacity" );								// If firm wants to shrink productive capacity
	v[5] = 1;													// Flag to signal whether cycles should keep going 
	
	// if the firm wants to shrink productive capacity, it will do so if the vintage is on the verge of physical depletion
	v[13] = COUNT( "CapitalVintage" );
	if ( v[4] < 0 ) 												
	{
		v[6]= MIN( "scrapTime" );
		cur = SEARCH_CND( "scrapTime", v[6] );
		if ( v[6] == t && v[13] != 1 )
		{
			v[7] = VLS( cur, "CapitalQuantity", 1 );
			v[8] = v[7] + v[4];
			v[9]= max( v[8], 0 );
			WRITES( cur, "Replace", true );
			WRITES( cur, "QuantityReplace", v[9] );
			WRITES( cur, "RevisedByShrinkCapacity", true );			// place a marker that this vintage has been already revised.
		}
	}

	// general case: while there are funds, starting from the less productive vintages, 
	// firms replace capital either because of physical depletion or following the payback routine
	
	SORT( "CapitalVintage", "VintageProductivity", "UP" );		// Sort capital vintages descending according to the vintage's productivity
	CYCLE( cur, "CapitalVintage" )
	{
		if ( v[3] > 0 )												// proceed the calculation only if the firms still have budget to replace capital
		{
			v[10] = VS( cur, "PaybackReplacementVintage" );
			v[13] = VS( cur, "scrapTime" );
			if ( v[10] <= v[0] )										// if payback period is lower than the cut payback
			{
				v[11] = VLS( cur, "CapitalQuantity", 1 );				// get quantity of capital
				v[12] = v[11] > v[3] ? v[3] : v[11];					// compare with the remaining budget to replace capital; if the capital quantity of that vintage is greater than the budget, bound to budget. Otherwise, replace all quantity
				WRITES( cur, "Replace", true );
				WRITES( cur, "QuantityReplace", v[12] );
				v[3] -= v[12];											// deduct replacement investment from budget
				v[2] += v[12];											// add to total replacement investment
			}
			else if (  v[13] == t )
			{
				v[11] = VLS( cur, "CapitalQuantity", 1 );				// get quantity of capital
				v[12] = v[11] > v[3] ? v[3] : v[11];					// compare with the remaining budget to replace capital; if the capital quantity of that vintage is greater than the budget, bound to budget. Otherwise, replace all quantity
				if ( VS( cur, "RevisedByShrinkCapacity" ) )
				{
					WRITES( cur, "Replace", true );
					WRITES( cur, "QuantityReplace", v[12] );
					v[3] -= v[12];											// deduct replacement investment from budget
					v[2] += v[12];											// add to total replacement investment
				}				
			}
		}
		else
		{
			WRITES( cur, "Replace", false );
			WRITES( cur, "QuantityReplace", 0 );
		}
	}
RESULT( v[2] )

EQUATION( "UnconstrainedReplacementInvestment" )
/*
Equation to retrieve what would be the unconstrained replacement investment.
It's obtained by setting a maximum investment deduced of expansionary investment (prioritary).
In presence of credit rationing, may be updated.
Level: firm.
*/
		v[0] = V( "ExpectedPrice" );								// Expected price	
		v[1] = V( "PlannedExpansionInvestment" ); 					// Planned expansion investment
		v[2] = V( "maxInvestmentRatio" ); 							// Max. investment to capital ratio
		v[3] = V( "AvailableCapitalStock" );                        // Begin-of-period capital stock		
		v[4] = V( "maxReplacementInvestment" );						// Max. replacement investment to capital ratio
		v[5] = v[2] * v[3]  - v[1];									// Max. real replacement investment after discounting the expansionary investment
		v[6] = v[4] * v[3];											// Max. real replacement investment																				
		v[7] = min( v[5], v[6] );									// Define the actual maximum replacement investment	(in real terms)
		v[8] = v[7] * v[0];
		WRITE( "maximumReplacementInvestmentReal", v[7] );
		WRITE( "maximumReplacementInvestmentNominal", v[8] );
RESULT( v[8] )
	
EQUATION( "PlannedReplacementInvestment" )
/*
Considers the unconstrained (in terms of credit) planned replacement investment.
Level: firm.
*/
		V( "UnconstrainedReplacementInvestment" );
		v[0] = V( "FunctionReplacementInvestment" );
RESULT( v[0] )
	
EQUATION( "ReplacementInvestment" )
/*
Considers the constrained (in terms of credit) replacement investment.
Level: firm*/
		v[0] = V( "FundsReplacementInvestment" );
		v[1] = V( "UnconstrainedReplacementInvestment" );
		if ( v[0] == v[1] ) 
			END_EQUATION( V( "PlannedReplacementInvestment" ) )
		v[2] = V( "FunctionReplacementInvestment" );
RESULT( v[2] )	

	
	
/*========================== SUPPORT EQUATIONS ===========================*/	
EQUATION( "UnitCostVintage" )
/*
Unit cost of existing vintage of capital. Used in the evaluation of replacement investment.
Level: capital vitange.
*/
	v[0] = VS( GRANDPARENT -> up, "NominalWage" );
	v[1] = V( "VintageProductivity" );
	v[2] = v[1] > 0 ? v[0] / v[1] : VS( PARENT, "NewMachineUnitCost" );
RESULT( v[2] )
	
EQUATION( "NewMachineUnitCost" )
/*
Unit cost of prospective new vintage of capital. Used in the evaluation of replacement investment.
Level: firm.
*/
	v[0] = VS( GRANDPARENT, "NominalWage" );
	v[1] = VL( "ACurrent", 1);
	v[2] = v[0] / v[1];
RESULT( v[2] )
	
EQUATION( "PaybackReplacementVintage" )
/*
Number of periods that the replacement of a particular vintage of capital would yield.
Level: capital vintage.
*/
	v[0] = VS( GRANDPARENT, "ExpectedPrice" );
	v[1] = VS( PARENT, "NewMachineUnitCost" );
	v[2] = V( "UnitCostVintage" );
	v[3] = v[2] - v[1];												// difference of unit cost between old and new vintage
	v[4] = v[3] == 0 ? 1e6 : v[0] / v[3];							// payback period for the particular vintage. If there is no difference of unit cost (technology did not change), the payback period becomes a very large and inviable figure
RESULT( v[4] )
	
EQUATION( "AvailableCapitalStock" )
/* 
Quantity of capital stock available for production (begin-of-period). 
Level: firm. 
*/
		v[0] = SUML( "CapitalQuantity", 1 ); 		
RESULT( v[0] )

EQUATION( "ExAnteAvgLabourProductivity" )
/* 
Average labour productivity of the firm. Used to compute the number of workers needed to produce. 
In case the firm is unable to hire the desired amount of workers, 
the average labour productivity is recalculated in equation AvgLabourProductivityUpdate.
Case 1 (flagProductivity = 1): firms use first newest vintages
Case 2 (default, flagProductivity = 0): firms use all vintages in the same proportion
Level: firm 
*/
		v[0] = VS( root, "flagProductivity" );
		v[1] = V( "DesiredCapacityUtilisation" );
		v[2] = V( "AvailableCapitalStock" );
		v[3] = v[1] * v[2];																// total capital to be used (subtractions below)
		v[4] = v[3];																	// total capital to be used
		v[5] = 0;																		// initialise counter of average productivity
		if ( v[1] == 1 || v[1] == 0 )													// does capacity utilisation equals 1 or 0?
		{																					// yes
				CYCLE( cur, "CapitalVintage" )
				{
						v[6] = VLS( cur, "CapitalQuantity", 1 );
						v[7] = VS( cur, "VintageProductivity" );
						v[5] += v[6] / v[2] * v[7]; 					
				}
				END_EQUATION( v[5] )
		} 
		else 																				// no
		{																										
    		if ( v[0] )																			// are we in case 1?
    		{																						// yes
    				SORT( "CapitalVintage", "VintageProductivity", "DOWN" );							// sort capital vintages descending according to productivity
    				CYCLE( cur, "CapitalVintage" )
    				{
    						v[6] = VLS( cur, "CapitalQuantity", 1 );
    						if ( v[3] >= v[6] )															// is capital quantity needed greater than the quantity of capital of the vintage?
    						{																				// yes
    								v[7] = VS( cur, "VintageProductivity" );									// get productivity
									v[5] += v[6] * v[7] / v[4]; 											// accumulate using the weight of capital vintage to total usage
									v[3] -= v[6];
    						}																				// no, but is there any further need for capital?
    						else if ( v[3] > 0 )																// yes
    						{
    								v[7] = VS( cur, "VintageProductivity" );										// get productivity
    								v[5] += v[3] * v[7] / v[4];														// accumulate using the remaining capital quantity to total usage
    								v[3] -= v[3];
    						}
    				}
    		}
    		else																					// no, we are not in case 1
    		{
					CYCLE( cur, "CapitalVintage" )														// just calculate the weighted average
    				{
    						v[6] = VLS( cur, "CapitalQuantity", 1 );
    						v[7] = VS( cur, "VintageProductivity" );
    						v[5] += v[6] / v[2] * v[7];	
    				}
    		}	
    	}	
RESULT( v[5] )

EQUATION( "AvgLabourProductivity" )
/*
Actual labour productivity of a firm.
Level: firm.
*/
	
	if ( VS( root, "flagProductivity" ) == 0 )									// are all capital vintages used proportionally?
		END_EQUATION( V( "ExAnteAvgLabourProductivity" ) )						// yes, ex-post average labour productivity is unchanged
	if ( V( "DesiredDemandLabour" ) == V( "WorkersFirm" ) )						// demand for labour equal to workers available for production?
		END_EQUATION( V( "ExAnteAvgLabourProductivity" ) )						// yes, ex-post average labour productivity is unchanged
		
	v[0] = V( "ProductionWorkers" );											// number of production workers	
	v[1] = V( "nu" );															// capital output ratio (parameter)
	v[2] = V( "AvailableCapitalStock" );										// available capital stock
	v[3] = 0;																	// initialise average labour productivity
	v[8] = v[0];																// permanent number of production workers (used for proportion)
	SORT( "CapitalVintage", "VintageProductivity", "DOWN" );
	CYCLE( cur, "CapitalVintage" )
	{
		// firstly, get potential number of workers to the particular vintage
		v[4] = VLS( cur, "CapitalQuantity", 1 );
		v[5] = VS( cur, "VintageProductivity" );
		v[6] = v[4] / ( v[1] * v[5] );											// max. number of workers to work in a particular vintage
		
		// is the number of workers available to work in a particular vintage less than the number of the number of total available workers?
		if ( v[6] < v[0] )															// yes
		{
			v[3] += ( v[6] / v[8] ) * v[5] ;											// weight the average labour productivity
			v[0] -= v[6] ;																// reduce number of available workers
		}
		else if ( v[6] > v[0] )														// no, but there is still labour 
	 	{
			v[3] += ( v[0] / v[8] ) * v[5]; 											// update the average labour productivity
			v[0] -= v[0];																// zero the number of workers available
	 	}
		else 																		// else, continue
			continue;	
	}
RESULT( v[3] )
	
EQUATION( "AgeVintage" )
/* 
Update age of capital goods 
Level: capital vintage.
*/
		v[0] = V( "VintageTime" );		
		v[1] = T - v[0];
RESULT( v[1] )

EQUATION_DUMMY( "ShrinkCapacity", "PlannedExpansionInvestment" )	

EQUATION( "PlannedInvestment" )
/* 
Desired level of investment by a firm (expansionary + replacement).
Level: firm.
*/
RESULT ( V( "PlannedExpansionInvestment") + V( "PlannedReplacementInvestment" ) )

EQUATION( "ActualInvestment" )
/*
Compute actual total investment of each firm.
Level: firm.
*/
	v[0] = V( "ReplacementInvestment" );
	v[1] = V( "ExpansionInvestment" );
	v[2] = v[0] + v[1];
RESULT( v[2] )
	
EQUATION( "CapitalShare" )
/*
Return firms' share of capital within the sector
Level: firm
*/	
	v[0] = VS( PARENT, "SectorCapital" );
	v[1] = V( "AvailableCapitalStock" );
	v[2] = v[1] / v[0]
RESULT( v[2] )
	
EQUATION( "SectorCapital" )
/*
Return total stock of capital
Level: sector
*/	
	v[0] = SUM( "AvailableCapitalStock" ); 
RESULT( v[0] )
	
EQUATION( "EmbedCapacity" )
/*
Equation for including the newly acquired capital vintage into capital stock object.
Level: firm.
*/
	v[0] = V( "ActualInvestment" );
	if ( v[0] == 0 )
		END_EQUATION( 0 );
	cur = ADDOBJ( "CapitalVintage" );
	v[1] = V( "kappa" );
	WRITES( cur, "VintageTime", t );
	WRITES( cur, "CapitalQuantity", v[0] );
	WRITES( cur, "QuantityDepreciation", 0 );
	WRITES( cur, "QuantityReplace", 0 );
	WRITES( cur, "Replace", 0 );
	v[2] = V( "ACurrent" );
	WRITES( cur, "VintageProductivity", v[2] );
	WRITES( cur, "scrapTime", t + v[1] );
	v[3] = VS( GRANDPARENT, "PriceLevel" );
	WRITES( cur, "VintagePrice", v[3] );
RESULT( 0 )

EQUATION( "CapitalQuantity" )
/*
Just replicate the lagged capital quantity. Run at the beginning of the period and eventually changed afterwards (capital depreciation).
Level: capital vintage.
*/
RESULT( VL( "CapitalQuantity", 1 ) )	

EQUATION( "CapitalDepreciation" )
/*
Calculate real and nominal capital depreciations.
Level: firm.
*/
	V( "EmbedCapacity" );
	v[0] = 0;												// accumulator real depreciation
	v[1] = 0;												// accumulator nominal depreciation
	CYCLE( cur, "CapitalVintage" )
	{
		v[2] = VS( cur, "QuantityReplace" );
		v[3] = VS( cur, "scrapTime" );
		v[4] = VS( cur, "VintagePrice" );
		v[5] = VLS( cur, "CapitalQuantity", 1 );
		if ( v[3] == t )
		{
			v[0] += v[5];
			v[1] += v[5] * v[4];
			WRITES( cur, "CapitalQuantity", 0 );
			WRITES( cur, "QuantityReplace", 0 );
			WRITES( cur, "Replace", false );
		}
		else if ( v[2] > 0 )
		{
			v[0] += v[2];
			v[1] += v[2] * v[4];
			if ( v[5] - v[2] < 0 ) PLOG( "\nDepreciation routine is producing negative capital quantity. Please check!" );
			WRITES( cur, "CapitalQuantity", v[5] - v[2] );
			WRITES( cur, "QuantityReplace", 0 );
			WRITES( cur, "Replace", false );
		}
	}
	WRITE( "RealDepreciation", v[0] );
	WRITE( "NominalDepreciation", v[1] );
RESULT( 0 )	

EQUATION( "CapitalVintageHousekeeping" )
/*
Housekeeping of capital vintages. This equation excludes depleted or replaced capital vintages.
Level: firm.
*/
	V( "CapitalDepreciation" );									// run after capital depreciation took place
	v[2] = COUNT( "CapitalVintage" );
	
	if ( v[2] == 1 )
	{
		CYCLE( cur, "CapitalVintage" )
		{
			v[0] = VS( cur, "CapitalQuantity" );
			v[1] = VS( cur, "scrapTime" );
		}
		if( v[0] == 0 || v[1] == t )
		{
			WRITE( "flagExit", 1 );
		}
	}
	else
	{
		CYCLE_SAFE( cur, "CapitalVintage" )
		{
			v[0] = VS( cur, "CapitalQuantity" );
			v[1] = VS( cur, "scrapTime" );
			if( v[0] == 0 || v[1] == t && v[2] != 1 )
				DELETE( cur );
		}
	}
RESULT( 0 )
	
EQUATION( "NominalCapitalStock" )
/*
Compute nominal capital stock of a firm.
Level: firm.
*/
	V( "CapitalVintageHousekeeping" );					
	v[0] = 0;
	CYCLE( cur, "CapitalVintage" )
	{
		v[0] += VS( cur, "CapitalQuantity" ) * VS( cur, "VintagePrice" );
	}
RESULT( v[0] )
	
EQUATION( "RealCapitalStock" )
/*
Compute nominal capital stock of a firm.
Level: firm.
*/
	V( "NominalCapitalStock" );	
	v[0] = SUM( "CapitalQuantity" );
RESULT( v[0] )	

EQUATION( "NominalInvestment" )
/*
Nominal investment in machines & equipment.
Level: firm.
*/
	v[0] = V( "ActualInvestment" );
	v[1] = V( "PriceLevel" );
RESULT( v[0] * v[1] )	
	
EQUATION_DUMMY( "RealDepreciation", "CapitalDepreciation" )

EQUATION_DUMMY( "NominalDepreciation", "CapitalDepreciation" )

EQUATION( "FirmProductivityGrowth" )
	if( V( "EntryTime" ) != t - 1 && V( "EntryTime" ) != t - 2 )
		v[0] = V( "AvgLabourProductivity" ) / VL( "AvgLabourProductivity", 1 ) - 1;
	else 
		v[0] = 0; 
RESULT( v[0] )

	
	