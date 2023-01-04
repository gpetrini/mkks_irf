/******************************************************************************

	Government object equations
	----------------------

	Equations that are specific to the government in the Minsky-FIH model.
 
 ******************************************************************************/



/*============================== KEY EQUATIONS ===============================*/
EQUATION( "DepositsG" )
/*
End-of-period deposits
*/
	v[0] = V( "DepositsG1" );
RESULT( v[0] )

EQUATION( "GovernmentConsumption" )
/* Government consumption */
	v[0] = VL( "AggregateCapitalStock", 1 );
	v[1] = V( "Gamma0" );
	v[2] = V( "Gamma1" );
	v[3] = V( "normalCapacity" );
	v[4] = VL( "AggregateCapacityUtilisation", 1 );
	v[5] = v[1] - v[2] * ( v[4] - v[3] );
	v[6] = v[5] * v[0];
RESULT( v[6] )
	
EQUATION( "GovernmentDebt" )
	v[0] = V( "InterestGovernmentDebt" );
	v[1] = V( "NominalGovernmentConsumption" );
	v[2] = V( "TotalTaxRevenues" );
	v[3] = VL( "GovernmentDebt", 1 ); 
	v[4] = v[3] + v[0] + v[1] - v[2];
RESULT( v[4] )

	
/*============================== SUPPORT EQUATIONS ===============================*/
EQUATION( "TotalTaxRevenues" )
/*Taxes levied on households' income, banks' and firm's profits.*/
	v[0] = 0;
	CYCLES( PARENT, cur, "Sectors" )
	{
		v[0] += SUMS( cur, "TaxFirm" );
	}
	v[1] = 0;
	CYCLES( PARENT, cur, "Banks" )
	{
		v[1] += SUMS( cur, "BankTax" );
	}
	v[2] = 0;
	CYCLES( PARENT, cur, "Households" )
	{
		v[2] += SUMS( cur, "TaxHousehold" );
	}
	v[3] = v[0] + v[1] + v[2];
RESULT( v[3] )

	
EQUATION( "TaxToGDP" )
/*Total tax revenue to GDP ratio*/
	v[0] = V( "TotalTaxRevenues" );
	v[1] = V( "NominalGDP_Demand" );
RESULT( v[0] / v[1] )
	  
EQUATION( "InterestGovernmentDebt" ) 
/*Financial expenditure of government.*/
	v[0] = V( "BaseInterestRate" );
	v[1] = VL( "GovernmentDebt", 1 );
	v[2] = v[0] * v[1];
RESULT( v[2] )

EQUATION( "NominalGovernmentConsumption" )
/*Nominal government consumption*/
	V( "AggregateDemand" );
	v[0] = VS( PARENT, "PriceLevel" );
	v[1] = V( "GovernmentConsumption" );
RESULT( v[0] * v[1] )
	  
EQUATION( "GovernmentDebtToGDP" ) 
	v[0] = V( "GovernmentDebt" );
	v[1] = V( "NominalGDP_Demand" );
RESULT( v[0] / v[1] )

EQUATION( "NominalDeficitGovernment" )
	v[0] = V( "NominalGovernmentConsumption" );
	v[1] = V( "TotalTaxRevenues" );
	v[2] = V( "InterestGovernmentDebt" );
	v[3] = v[0] + v[2] - v[1];
RESULT( v[3] )
	
EQUATION( "ChangeGovernmentDebt" );
	v[0] = V( "GovernmentDebt" );
	v[1] = VL( "GovernmentDebt", 1 );
RESULT( v[0] - v[1] )
	  
	  
	  





