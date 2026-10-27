## CodeWAS

CodeWAS compares concepts between a case cohort and a control cohort. It can analyse binary concepts, counts, categorical values, continuous values, age at first event, and days to first event when those analyses are available for the selected concepts.

### Before running

- Select the case and control cohorts.
- Select the concepts or concept groups to analyse.
- Set the minimum cell count. Concepts with too few observations are excluded from the relevant analysis.
- Review the cohort counts and overlap information shown in the settings panel.

### Interpreting results

The result table reports sample sizes, an effect estimate, and a p-value for each concept and analysis type. The meaning of the effect depends on the analysis: for example, binary results use an odds ratio, while continuous or count results use a model-specific comparison.

A significant result is a statistical association, not proof that the concept causes the phenotype. Check the concept definition, sample sizes, effect direction, and analysis type before drawing a conclusion. The same concept may appear in several analysis types because different aspects of its data are being tested.

The viewer's **Study details** page records the cohorts, database, and settings used for the run.
