# CO2AnalysisModules 3.1.0
- Added upset plot to PhenotypeScoring module
- 
# CO2AnalysisModules 
- Updated to use DatabaseConnector v7
- Label both cases and controls in CodeWAS results visualization
- Have covariate group names instead of group ids

# CO2AnalysisModules 2.4.1
- Fix regex pattern in assertion string

# CO2AnalysisModules 2.4.0
- Adding the PhenotypeScoring module
- Fixed bug on warning on different sex and birth year
- Add filters for the CodeWAS and TimeCodeWAS result tables

# CO2AnalysisModules 2.3.3
- Added ATC DDD to CodeWAS and TimeCodeWAS
- Changes in UI for small screens

# CO2AnalysisModules 2.3.2
- Fixed bug in CodeWAS when there are no covariates

# CO2AnalysisModules 2.3.1
- log filter bug
  
# CO2AnalysisModules 2.3.0
- Added ATC groups as covariate to CodeWAS and TimeCodeWAS
- Option to automatically build a matched control cohort for CodeWAS and TimeCodeWAS
- Filter controls wait one second before launching update, so it is possible to change several controls before (potentially lengthy) redraw
- Table views have drop-down menus for sorting (shift-click are not passed in sandbox)
- CodeWAS visualization indicates the limits of computation as gray areas


# CO2AnalysisModules 2.2.0
- Added endpoint and library cohorts as covariate to CodeWAS
- Added concept code and Vocabulary to tables in CodeWAS and TimeCodeWAS
- Replaced DT by reactable
- Other minor fixes

# CO2AnalysisModules 2.1.0
- Replaced DT by reactable
- other minor fixes

# CO2AnalysisModules 2.0.0
- Adapted to DBI drivers
- Fixed GWAS token
- Added year of birth distribution check to GWAS
- Other minor fixes

# CO2AnalysisModules 1.0.4

- Improve TimeCodeWAS plot readability #80
- removed all source drugs from default in CodeWAS and TimeCodeWAS

# CO2AnalysisModules 1.0.3

- Hot fix: volcano plot and upset plot default intersection value to 0

# CO2AnalysisModules 1.0.2

- Hot fix: wrong selected cohorts in CohortOverlaps and CohortDemographics
- Other minor hot fixes

# CO2AnalysisModules v1.0.0

- Initial release
- Modules: CohortsOverlaps, CohortDemographics, CodeWAS, TimeCodeWAS, GWAS
