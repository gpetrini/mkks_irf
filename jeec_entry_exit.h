/******************************************************************************

	Entry/exit of firms
	----------------------
 
 ******************************************************************************/
EQUATION( "BankruptcyRate" )
RESULT( SUM( "DefaultFlag" ) / COUNT( "Firms" ) * 100 )

EQUATION( "Exit" )
/* Manage exit of firms.*/
	V( "NumberOfEntrants" );									// execute after the number of entrants is calculated.
	V( "BankruptcyRate" );
	if( MAX( "DefaultFlag" ) == 0 )		
		END_EQUATION( 0 );

	// run after several aggregates were calculated
	V( "CheckFinancialSavings" );
	V( "CheckMarketShare" );
	V( "CheckFirmsDebt" );
	V( "RealGDP_Demand" );

	// housekeeping of pointers
	CYCLE( cur, "Banks" )
	{
		v[0] = COUNT_HOOKS( cur );
		for( i = 0; i < v[0]; i++ )
		{
			VS( cur, "NetWorthBank" );
			VS( cur, "LoanPortfolio" );
			if( HOOKS( cur,  i ) != NULL )
			{
				v[1] = VS( HOOKS( cur,  i ), "DefaultFlag" );
				if ( v[1] == 1 )
					WRITE_HOOKS( cur, i, NULL );
			}	
		}
		
	}

	// housekeeping of capital depreciation
	CYCLE( cur, "Firms" )
	{
		VS( cur, "TotalDebt" );
		VS( cur, "FirmDeposits" );
		VS( cur, "Leverage" );
		
		if( VS( cur, "flagExit" ) == 1 ) 
		{
			//PLOG( "\nA firm died!" );
			v[2] = 0;
			v[3] = 0;
			CYCLES( cur, cur1, "CapitalVintage" )
			{
				v[4] = VS( cur1, "CapitalQuantity" );
				v[5] = VS( cur1, "VintagePrice" );
				v[2] += v[4];
				v[3] += v[4] * v[5];
			}
			WRITE( "AggregateDepreciation", V( "AggregateDepreciation" ) + v[2] );
			WRITE( "AggregateNominalDepreciation", V( "AggregateNominalDepreciation" ) + v[3] );
			WRITE( "AggregateInventories", V( "AggregateInventories" ) - VS( cur, "Inventory" ));
			WRITE( "AggregateCapitalStock", V( "AggregateCapitalStock" ) - v[2] );
			WRITE( "AggregateNominalCapital", V( "AggregateNominalCapital" ) - v[3] );
						
		}
	}

	CYCLE_SAFE( cur, "Firms" )
	{
		VS( cur, "DebtServicing" );
		if( VS( cur, "flagExit" ) == 1 ) 
		{
		//	PLOG( "\nFirm entry: " );
		//	PLOG( "%f", VS( cur, "EntryTime" ) );
			DELETE( cur );
		}
	}
RESULT( 0 )	
	
EQUATION( "NumberOfEntrants" )
/* get the total number of firms entering the market */
RESULT( SUM( "DefaultFlag" ) )

	
EQUATION( "Entry" )
/* manage entry of new firms in the market */
	V( "Exit" );														// exectute after exit of firms
	v[0] = V( "NumberOfEntrants" );
	if ( v[0] == 0 )
		END_EQUATION( 0 );													// if there is no entrant, end equation
	if ( LAST_T - t < 2 )	
		END_EQUATION( 0 );													// only run equation until two steps before the simulation is stopped (entry routine requires that)
	
	// and firm, edit relevant values and 
	for( i = 0; i < v[0]; i++ )
	{
		j = 1;
		// identify first empty id
		CYCLE( cur, "Firms" )
		{
			if ( j != VS( cur, "idFirm" ) )
			 	continue;
			else j++;
		}
		
		
		cur = ADDOBJ( "Firms" );											// add firm and create pointer to it
		WRITES( cur, "idFirm", j );
		
		// firms have initially zero debt (number of objects in excess are deleted in the next period in Outstanding Loan object's housekeeping
		CYCLES( cur, cur1, "OutstandingLoan" )
		{
			WRITES( cur1, "LoanOutstanding", 0 );
			WRITELLS( cur1, "LoanOutstanding", 0, t, 1 );
			WRITES( cur1, "BankSupplierID", 0 );
			WRITES( cur1, "AmortisationSchedule", 0 );
			WRITES( cur1, "AmortisationSchedule", 0 );
			WRITES( cur1, "PeriodsRemaining", 0 );
			WRITES( cur1, "InterestSchedule", 0 );
			WRITES( cur1, "LoanInterestRate", 0 );
		}
		WRITELS( cur, "Leverage", 0, t-1 );
		WRITES( cur, "Leverage", 0 );
		
		// firms have initially zero capital stock (investment on new capital goods are stablished later)
		CYCLES( cur, cur1, "CapitalVintage" )
		{
			WRITES( cur1, "AgeVintage", 0 );
			WRITES( cur1, "CapitalQuantity", 0 );
			WRITELLS( cur1, "CapitalQuantity", 0, t, 1 );
			WRITES( cur1, "PaybackReplacementVintage", 0 );
			WRITES( cur1, "QuantityDepreciation", 0 );
			WRITES( cur1, "QuantityReplace", 0 );
			WRITES( cur1, "Replace", 0 );
			WRITES( cur1, "UnitCostVintage", 0 );
			WRITES( cur1, "VintagePrice", 0 );
			WRITES( cur1, "VintageProductivity", 0 );
			WRITES( cur1, "VintageTime", 0 );
			WRITES( cur1, "scrapTime", 0 );
		}
		// get technology of new firm
		v[1] = beta( V( "betaEntry1" ), V( "betaEntry2" ) );
		v[2] = V( "TechnologicalFrontier" );
		v[3] = v[1] * v[2];
		WRITELS( cur, "ACurrent", v[3], t + 1 );
		WRITELS( cur, "AvgLabourProductivity", 0, t + 1 ); 
		WRITELS( cur, "ExAnteAvgLabourProductivity", 0, t + 1 ); 
		
		// get size of the new firm applying a percentage to average incumbents' size
		v[4] = uniform( V( "bottomBoundSizeEntry" ), V( "topBoundSizeEntry" ) );
		v[5] = AVE( "AvailableCapitalStock" );
		v[6] = v[4] * v[5];																	// this is the desired investment in the next period
		WRITELS( cur, "ActualInvestment", v[6], t + 1 );
		WRITELS( cur, "ExpansionInvestment", v[6], t + 1 );
		WRITELS( cur, "PlannedReplacementInvestment", 0, t + 1 );
		WRITELS( cur, "ReplacementInvestment", 0, t + 1 );
		WRITELS( cur, "NewMachineUnitCost", 0, t + 1 );
				
		// initial market share of firm (starting from two periods ahead) starts from the share of capital
		v[7] = v[6] / SUM( "AvailableCapitalStock" );
		//WRITELS( cur, "MarketShare", v[7], t - 1 );
		//WRITES( cur, "MarketShare", 0 );
		WRITELS( cur, "MarketShare", 0, t + 1 );
		WRITELS( cur, "MarketShare", v[7], t + 2 );
		WRITES( cur, "marketShareEntry", v[7] );
		WRITES( cur, "Markup", V( "initialMarkup" ) );
		WRITELS( cur, "Markup", V( "initialMarkup" ), t + 1 );
		WRITELS( cur, "Markup", V( "initialMarkup" ), t + 2 );
		WRITELS( cur, "Price", 0, t + 1 );
		WRITELS( cur, "UnitCost", 0, t + 1 );
		
		// initial expected sales is related to the initial capital stock. Fill values ahead too.
		v[8] = v[6] * V( "normalCapacity" ) / V( "nu" );
		WRITELS( cur, "Sales", 0, t - 1 );
		WRITELS( cur, "Sales", 0, t + 1 );
		WRITELS( cur, "UnfilledDemand", 0, t + 1 );
		WRITELS( cur, "CapacityUtilisation", 0, t + 1 );
		WRITELS( cur, "DesiredCapacityUtilisation", 0, t + 1 );
		WRITELS( cur, "DesiredCapacityUtilisation", V( "normalCapacity" ), t + 2 );
		WRITELS( cur, "DesiredDemandLabour", 0, t + 1 );
		WRITELS( cur, "FirmDemand", 0, t + 1 );
		WRITELS( cur, "ExpectedSales", v[8], t + 2 );
		WRITELS( cur, "Inventory", 0, t + 1 );
		WRITES( cur, "EntryTime", t );
		
		// assume that households provide part of new money (with its own deposits).
		// the other part of the resources to stablish a new firm comes from new loans.
		// to define the proportion of capital contribution by households and new debt, 
		// we consider the average leverage of the firms' sector and the average liquid assets to total assets of the firms' sector.
		// this design is inteded to affect the least model's results through entry.
		v[13]= V( "AggregateFirmDeposits" ) / V( "AggregateAssetsFirms" ); 									// average liquidy ratio
		v[14]= V( "AggregateLeverage" );
		v[15]= v[8] * V( "iota" );																			// expected inventory (quantity)
		v[16]= v[15] * V( "PriceLevel" ) / ( 1 + V( "AverageMarkup" ) );									// expected inventory (nominal)
		v[9] = V( "PriceLevel" ) * v[6] ;																	// expected cost of investment
		v[17]= ( v[16] + v[9] ) / ( 1 - v[13] );															// total assets of the new firm
		v[18]= v[17] * v[14];																				// demand for loans	
		WRITELS( cur, "DemandLoans", v[18], t + 1 );												
		v[19]= v[17] + v[8] * V( "PriceLevel" ) - v[18];													// total deposits needed by the new firm (includes initial production for sales)
		WRITES( cur, "FirmDeposits", v[19] );
		cur2 = SEARCHS( PARENT, "Households" );
		v[21]= VS( cur2, "CapitalContribution" );
		WRITES( cur2, "CapitalContribution", v[21] + v[19] );
		WRITES( PARENT, "ChangeDepositsFirmsSector", VS( PARENT, "ChangeDepositsFirmsSector" ) + v[19] );
		WRITES( PARENT, "AggregateFirmDeposits", VS( PARENT, "AggregateFirmDeposits" ) + v[19] );
		
		// expected cash flow of firms (needed for loan demand when there is credit rationing)
		v[11]= ( V( "initialMarkup" ) - V( "gamma" ) ) * V( "NominalWage" ) * v[8] / v[3];					// expected nominal profits minus the wage bill
		WRITELS( cur, "ExpectedOperatingCashFlow", v[11], t + 1 );
		
		// create hook from firms to banks and vice-versa
		v[12] = V( "DrawABank" );
		SORT( "Firms", "idFirm", "UP" );
		cur1 = SEARCH_CND( "idBank", v[12] );
		WRITES( cur, "BankID", v[12] );
		WRITE_SHOOKS( cur, cur1 );
		CYCLES( PARENT, cur1, "Banks" )
		{
			if( ( VS( cur, "idFirm" ) - 1  ) >= COUNT_HOOKS( cur1 ) )
				ADDHOOKS( cur1, VS( cur, "idFirm" ) );
			
			WRITE_HOOKS( cur1, VS( cur, "idFirm" ) - 1, cur );
		}		
	}
RESULT( v[0] )
	




