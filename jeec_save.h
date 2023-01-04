/******************************************************************************

	Equations with short names to save and analyse
	----------------------

	_ indicate and aggregate variable
	__ indicate a micro or sectoral variable
 
 ******************************************************************************/
EQUATION( "_FFI" )
RESULT( V( "FinancialFragilityIndex" ) )
	
EQUATION( "_UnwHedge" )
RESULT( V( "IncidenceHedgeFinancing" ) )
	
EQUATION( "_UnwSpec" )
RESULT( V( "IncidenceSpeculativeFinancing" ) )	
	
EQUATION( "_UnwPonzi" )
RESULT( V( "IncidencePonziFinancing" ) )		
	
EQUATION( "_WeiHedge" )
RESULT( V( "WeightedIncidenceHedgeFinancing" ) )
	
EQUATION( "_WeiSpec" )
RESULT( V( "WeightedIncidenceSpeculativeFinancing" ) )	
	
EQUATION( "_WeiPonzi" )
RESULT( V( "WeightedIncidencePonziFinancing" ) )		
	
EQUATION( "_I" )
RESULT( V( "AggregateInvestment" ) )
	
EQUATION( "_K" )
RESULT( V( "AggregateCapitalStock" ) )

EQUATION( "_C" )
RESULT( V( "AggregateConsumption" ) )

EQUATION( "_G" )
RESULT( V( "GovernmentConsumption" ) )

EQUATION( "_Y" )
RESULT( V( "RealGDP_Demand" ) )

EQUATION( "_A" )
RESULT( V( "AggregateProductivity" ) )
	
EQUATION( "_N" )
RESULT( V( "TotalLabour" ) )

EQUATION( "_L" )
RESULT( V( "AggregateDebtFirms" ) )
	
EQUATION( "_Lev" )	
RESULT( V( "AggregateLeverage" ) )
	
EQUATION( "_inf" )
RESULT( V( "Inflation" ) )
	
EQUATION( "_Profits" )
RESULT( V( "AggregateProfits") )	
	
EQUATION( "_Tax" )
RESULT( V( "TotalTaxRevenues" ) )

EQUATION( "_Inv" )
RESULT( V( "AggregateInventories" ) )

EQUATION( "_Markup" )
RESULT( V( "AverageMarkup"  ) )

EQUATION( "_YN" ) 
RESULT( V( "NominalGDP_Demand" ) )

EQUATION( "_DF" ) 
RESULT( SUM( "FirmDeposits" ) )

EQUATION( "_AssetsF" )
RESULT( V( "AggregateAssetsFirms" ) )
	
EQUATION( "__Lf" ) 
RESULT( V( "TotalDebt" ) )

EQUATION( "_NI" )
RESULT( V( "AggregateCapitalStock" ) - VL( "AggregateCapitalStock", 1 ) )
	
EQUATION( "_u" )
RESULT( V( "AggregateCapacityUtilisation" ) )

EQUATION( "_ProfitRate" ) 
RESULT( V( "AggregateProfitRate" ) )

EQUATION( "_NI" )
RESULT( V( "AggregateNominalInvestment" ) )

EQUATION( "_KN" )
RESULT( V( "AggregateNominalCapital" ) )

EQUATION( "__A" )
RESULT( V( "AvgLabourProductivity" ) )
	
EQUATION( "__Lev" )
RESULT( V( "Leverage" ) )	
		
EQUATION( "__Assets" )
RESULT( V( "FirmAssets" ) )
	
EQUATION( "__WeiItoK" ) 
RESULT( V( "AvailableCapitalStock" ) > 0 ? V( "ActualInvestment" ) * V( "CapitalShare" ) / V( "AvailableCapitalStock" ) : 0 )
	
EQUATION( "__KN" )
RESULT( V( "NominalCapitalStock" ) )

EQUATION( "__K" )
RESULT( V( "RealCapitalStock" ) ) 

EQUATION( "__ProfitRate" )
RESULT( V( "AvailableCapitalStock" ) > 0 ? V( "OperatingCashFlow" ) / VL( "NominalCapitalStock", 1 ) : 0 )
	
EQUATION( "__CashF" )
RESULT( V( "OperatingCashFlow" ) )
	
EQUATION( "__I" )
RESULT( V( "ActualInvestment" ) )

EQUATION( "__Minskian" )
RESULT( V( "CreditSupply" ) > V( "UndistributedProfits" ) )

EQUATION( "_pie" )
RESULT( VL( "Inflation", 1 ) )
	
EQUATION( "_P" )
RESULT( V( "PriceLevel" ) )

EQUATION( "_SN" )
RESULT( V( "Agg_NominalSales" ) )

EQUATION( "_BtoY" )
RESULT( V( "GovernmentDebtToGDPRatio" ) )

EQUATION( "_VtoY" )
RESULT( V( "HouseholdsWealthGDP" ) )
	
EQUATION( "_SavfToY" )
RESULT( V( "FinancialSavingsFirmsSector_GDP" ) )
	
EQUATION( "_SavhToY" )
RESULT( V( "FinancialSavingsHouseholds_GDP" ) )
	
EQUATION( "_SavgToY" )
RESULT( V( "FinancialSavingsGovermentSector_GDP" ) )

EQUATION( "_SavbToY" )
RESULT( V( "FinancialSavingsBankingSector_GDP" ) )	

EQUATION( "_gA" )
RESULT( V( "AggregateGrowthProductivity" ) )

EQUATION( "__int" )
RESULT( V( "AverageInterestRate" ) )	
	
EQUATION( "_gY" )
RESULT( V( "RealGDP_Demand" ) / VL( "RealGDP_Demand", 1 ) - 1 )
	
EQUATION( "_Minskian" )
RESULT( WHTAVE( "MinskianFirm", "FirmAssets" ) / SUM( "FirmAssets" ) )
	
EQUATION( "_br" )
RESULT( V( "BankruptcyRate" ) )

EQUATION( "_HHI" )
	v[0] = 0;
	CYCLE( cur, "Sectors" )
	{
		CYCLES( cur, cur1, "Firms" )
		{
			v[0] += pow( VS( cur1, "MarketShare" ), 2 );
		}
	}
RESULT( v[0] )

EQUATION( "_TD" )
/* Technological dispersion*/
	v[0] = V( "AggregateProductivity" );									// weighted average
	v[1] = 0;																// accumulator
	CYCLE( cur, "Sectors" )
	{
		CYCLES( cur, cur1, "Firms" )
		{
			v[2] = VS( cur1, "MarketShare" );
			v[3] = VS( cur1, "AvgLabourProductivity" );
			v[1] += v[2] * pow( ( v[3] - v[0] ), 2 );
		}
	}
	v[4] = v[0] != 0 && v[1] > 0 ? pow( v[1], 0.5 ) / v[0] : 0 ;
RESULT( v[4] )
		
EQUATION( "_TechHet" )
/* Technological dispersion*/
	v[0] = V( "AggregateProductivity" );									// weighted average
	v[1] = MAX( "AvgLabourProductivity" );									// frontier
RESULT( v[0] / v[1] )
	
	