/******************************************************************************

	Firm object equations
	----------------------

	Subset of equations related to production and pricing.
 
 ******************************************************************************/

/*============================== KEY EQUATIONS ===============================*/
EQUATION( "Price" )
/* 
Markup applied on unit labour costs.
Since while firms fix prices firms do not know the actual production, consider the ex ante one to compute the average labour productivity.
Level: firm.*/
	v[0] = V( "Markup" );
	v[1] = V( "NominalWage" );
	v[2] = V( "ExAnteAvgLabourProductivity" );
	v[3] = ( 1  + v[0] ) * v[1] / v[2];
RESULT( v[3] )

EQUATION( "UnitCost" )
/*
Actual unit cost of each firm (considers actual labour productivity).
Level: firm.
*/
	v[0] = V( "NominalWage" );
	v[1] = V( "AvgLabourProductivity" );
	v[2] = v[0] / v[1];
RESULT( v[2] )
	
EQUATION( "NominalSales" )
/*
Compute firm's nominal sales. Price times real sales.
Level: firm.
*/
		v[0] = V( "Sales" );
		v[1] = V( "Price" );
		v[2] = v[0] * v[1];
RESULT( v[2] )

EQUATION( "Markup" )
/*
Markup evolves in time according to the dynamics of market share. 
Level: firm.
*/
	v[0] = VL( "MarketShare", 1 );
	v[1] = VL( "MarketShare", 2 );
	v[2] = V( "upsilon" );
	v[3] = VL( "Markup", 1 );
	if ( v[1] > 0 )
	{
		v[4] = v[3] * ( 1 + v[2] * ( v[0] - v[1] ) / v[1] );	
	}
	else
	{
		v[4] = v[3];
	}
RESULT( v[4] )

EQUATION( "DesiredCapacityUtilisation" )
/*
Desired level of capacity utilisation. May be higher than 1 (important for expansionary investment behaviour).
Bounded later in actual capacity utilisation equation.
Level: firm.
*/	
		v[0] = VS( PARENT, "nu" );													// Capital output ratio (parameter)
		v[1] = V( "DesiredProduction" );											// Desired production or orders
		v[2] = V( "AvailableCapitalStock" );										// Total capital stock available for production
		v[3] = v[0] * v[1] / v[2];														// Desired capacity utilisation 
RESULT( v[3] )	

EQUATION( "DesiredProduction" )
/*
Desired Production. Depends on the expected sales and on the opening inventories.
Level: firm.
*/
		v[1] = VS( PARENT, "iota" );													// target inventory to sales ratio
		v[2] = V( "ExpectedSales" );										
		v[3] = VL( "Inventory", 1 );          											// opening inventory
		v[4] = ( 1 + v[1] ) * v[2] - v[3];		
		v[4] = max( 0, v[4] );
RESULT( v[4] )

EQUATION( "Production" )
/* 
Actual production of firms.
Level: firm.
*/
	v[0] = V( "ProductionWorkers" );
	v[1] = V( "AvailableCapitalStock" );
	v[2] = VS( PARENT, "nu" );
	v[3] = V( "AvgLabourProductivity" );
	v[4] = v[1] / v[2];
	v[5] = v[0] * v[3];
	v[6] = min ( v[4], v[5] );
RESULT(	v[6] )
	
/*============================== SUPPORT EQUATIONS ===============================*/
EQUATION( "ExpectedSales" )
/*
Firms' expected sales is assumed to previous period's total demand 
(which may differ from the actual sales, if the firm could not fill the entire demand).
Level: firm.
*/
RESULT( VL( "FirmDemand", 1 ) )
	
EQUATION( "CapacityUtilisation" )
/* 
Effective capacity utilisation takes into account the effective production.
Ex post to labour market interactions. Already bounded by capital constraint.
Level: firm.
*/
	v[0] = V( "Production" );
	v[1] = V( "AvailableCapitalStock" );
	v[2] = V( "nu" );
	v[3] = v[0] * v[2] / v[1];
RESULT( v[3] )
		
EQUATION( "Inventory" )
/*
Post-production inventory. Updated later after sales take place.
Level: firm.
*/
		v[0] = VL( "Inventory", 1 ) + V( "Production" );
RESULT( v[0] )	
			
EQUATION( "ExpectedPrice" )
/*
Expected price level. Adaptive expectations.
Level: firm.
*/
	v[0] = VL( "PriceLevel", 1 );
	v[1] = VL( "Inflation", 1 );
	v[2] = v[0] * ( 1 + v[1] );
RESULT( v[2] )
	
EQUATION( "InventoryRevaluation" )
/*
End-of-period inventory revaluation.
Level: firm.
*/
	V( "AggregateInventories" );										// ensure order of execution
	v[0] = VL( "UnitCost", 1 );
	v[1] = V( "UnitCost" );
	v[2] = VL( "Inventory", 1 );
	v[3] = V( "Inventory" );
	v[4] = v[1] * v[3] - v[0] * v[2];
RESULT( v[4] )
		
		
		
		
	
	
	
	