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
