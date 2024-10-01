## CodeWAS

Cohort Operations provides a PheWAS tool to explore medical codes and measurement value enrichment between cohorts with visualizations and p-values. 

### Analysis Settings

Analysis settings consist on:

- Cases cohort: Group of patient with common characteristics.
- Controls cohort: Group of patients without thes characteristics of cases.
- Analysis: One or more group of covariates to be compare between cases and controls.
- Min cell count: Limit the results to covariates with at least this number of samples in each group.

### Analysis Results

#### Table

Results table show one row for each `Covarate Name`, which are grouped by `Analysis Name` and `Domain`. 

- `N cases` and `N ctrls` show the number of subjects with the covariate in the case and control groups, respectively.
- `Ratio/Mean cases` and `Ratio/Mean ctrls` show the ratio of subjects with the covariate for binary covariates and the mean value for continuous covariates.
- `SD cases` and `SD ctrls` show the standard deviation for the ration or the mean.
- `OR`, `p`, and `Beta` indicate the odds ratio, p-value, and beta coefficient from statistical test assessing the dissimilarity of the covatiate between cases and controls.
- `Model` indicates the model used for the statistical test. Which can be :
  - `Chi-square` for binary covariates with more than 10 samples in each group. `p` value is calculated using chi-square test, and `OR` as $OR = \frac{n_{casesYes} \times n_{ctlsNo}}{n_{casesNo} \times n_{ctrlYes}}$.
  - `Fisher` for binary covariates with less than 10 samples in each group. `p` value is calculated using Fisher's test, and `OR` as $OR = \frac{n_{casesYes} \times n_{ctlsNo}}{n_{casesNo} \times n_{ctrlYes}}$.
  - `Welch Two Sample t-test` for continuous covariates, `p` value is calculated using Welch Two Sample t-test, and `OR` as the exponent of Cohens' d, $OR = \exp\left( t \times \sqrt{\frac{1}{n_{cases}} + \frac{1}{n_{controls}}} \right)$
  
  In all cases `Beta` is calculated as $Beta = log(OR)$.
  
  
#### Plot

The plot tab show a Volcano plot. Where each point is a covariate, the x-axis is the beta coefficient and the y-axis is the -log10(p-value). The points are colored by the domain of the covariate. Hovering over a point shows detailed information about the covariate. The plot includes two reference dashed red lines, one vertical line at 0 and one horizontal the Bonferroni correction level, caculated as $-log(0.05/n_{covariates})$.
  
### CodeWAS in FinnGen Handbook

`https://finngen.gitbook.io/finngen-handbook/working-in-the-sandbox/which-tools-are-available/cohort-operations-tool-co/explore-code-and-endpoint-enrichments-with-co`
