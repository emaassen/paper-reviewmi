# The Dire Disregard of Measurement Invariance Testing in Psychological Science

## Overview
This repository contains all the data, code, and figures from our article titled "The Dire Disregard of Measurement Invariance Testing in Psychological Science”. The study aimed to examine common practices for conducting and documenting measurement invariance testing, evaluating the reproducibility of those tests, and assessing the degree of measurement invariance in scale mean comparisons across 426 psychology research articles.

## Background
The research process was divided into four steps, preceded by a pilot study.

Step 1. Report: We analyzed 426 psychology research articles to identify scale mean comparisons across groups or over time and to what extent they reported necessary measurement information and measurement invariance testing.
Step 2. Reproduce: We attempted to computationally reproduce the reported measurement invariance tests from Step 1, using the fit measures and fit criteria of the authors.
Step 3. Reassess: We assessed measurement invariance of the studies in Step 2, using our own chosen fit measures and fit criteria.
Step 4. Reuse: We conducted MI tests on scale mean comparisons from Step 1 that originally did not report doing them.

These steps are sequential and interdependent, with the results of one phase informing the subsequent phases.

Note that an ethical review of this study has not been carried out, as this study only makes use of secondary data. Data collection and analysis occurred from 2020-2021. For the pilot study we sampled articles from 2011-2014. For the main study we sampled articles from 2018-2019.

The article was accepted for publication in 2023, and published online on December 25th 2023 here: https://doi.org/10.1037/met0000624.

The authors made the following contributions:
-	Conceptualization: E.D.D., E.M., M.B.N., J.M.W.
-	Data curation: E.D.D., E.M.
-	Formal analysis: E.D.D., E.M.
-	Funding Acquisition: K.D.R., J.M.W.
-	Investigation: E.D.D., E.M.
-	Methodology: E.D.D., E.M., J.M.W.
-	Project administration: E.D.D., E.M.
-	Resources: E.D.D., E.M., J.M.W.
-	Software: E.D.D., E.M.
-	Supervision: M.A.L.M., M.B.N., K.D.R., J.M.W.
-	Validation: E.D.D., E.M.
-	Visualization: E.D.D., E.M.
-	Writing; Original draft: E.D.D., E.M.
-	Writing; Review and Editing: E.D.D., E.M. M.A.L.M., M.B.N., K.D.R., J.M.W.

This research was supported by a VICI grant (project number VI.C.221.100) from the Dutch Research Council (NWO) and Consolidator Grant 726361 (IMPROVE) from the European Research Council (ERC), both awarded to J.M. Wicherts. The funders had no role in study design, data collection and analysis, decision to publish, or preparation of the manuscript.

## Repository Structure
Below is a description of the main components of this repository. 

### Code
-	0-example-mi-test.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script demonstrating a sample measurement invariance test.
-	0-example-simulation.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script with simulation code to determine whether a scale mean comparison has enough statistical power to identify a moderate degree of scalar measurement noninvariance.
-	0a-pilot-sample-articles.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script used to sample articles to analyze for the pilot study.
-	0b-pilot-mi-tests.zip  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Zip file with individual R scripts used to test for measurement invariance in the articles from the pilot study.
-	0c-pilot-analyses.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script for the analysis of results in the pilot study.
-	1a-sample-articles.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script used to sample articles to analyze for the main study.
-	1b-analysis.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script for the analysis of results of Step 1.
-	2a-mi-tests.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script to reproduce measurement invariance tests in Step 2.
-	2b-analyses.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script for the analysis of results of Step 2 and 3.
-	4a-sample-comparisons.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script used to sample scale mean comparisons for Step 4.
-	4b-mi-tests.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script to run measurement invariance tests in Step 4.
-	4c-analyses.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script for the analysis of results of Step 4.

### Data
-	article-index/article-index-pilot.txt  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Text file with indices referring to studies in journals to sample for the pilot study. 
-	article-index/sample-edd.txt  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Text file with indices and dois for studies in Step 1. This is the order in which author E.D.D. coded the studies.
-	article-index/sample-em.txt  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Text file with indices and dois for studies in Step 1. This is the order in which author E.M. coded the studies.
-	article-index/sample-ps.txt  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Text file with indices and dois for studies from Psychological Science, used in Step 1.
-	codebook-main-step1.xlsx   
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Codebook of Step 1.
-	codebook-main-step2step3-sample-without-results.xlsx  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Codebook of Step 2 and Step 3 without results.
-	codebook-main-step2step3.xlsx  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Codebook of Step 2 and Step 3.
-	codebook-main-step4-sample-without-results.xlsx  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Codebook of Step 4 without results. This codebook is generated by 4a-sample-comparisons.R.
-	codebook-main-step4.xlsx  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Codebook of Step 4.
-	codebook-pilot.xlsx  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Codebook of pilot study.
-	codebook-variables-description.html  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;HTML file with description of all variables used in the codebooks, for all steps.
-	codebook-variables-description.Rmd  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;RMarkdown file to generate the HTML file codebook-variables-description.html.
-	interraterreliability.xlsx   
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Results from the individual codebooks of coders E.D.D. and E.M. to determine the interrater reliability.

### Figures
-	Figure1.drawio  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Diagram file to generate Figure 1.
-	Figure1.jpg  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;JPG file of Figure 1, saved from Figure1.drawio.
-	Figure2.jpg  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;JPG file of Figure 2, saved from Figure2.R.
-	Figure2.R  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;R script to generate Figure 2.

### Supplemental Materials
-	Addendum A.pdf  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Supplemental Material A, results of the pilot study.
-   Addendum A.Rmd  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Rmarkdown file to generate Addendum A.pdf
-	Addendum B.drawio  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Diagram file to generate Supplemental Material B. 
-	Addendum B.pdf  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Supplemental Material B, generated from Addendum B.drawio.
-	Addendum C.pdf   
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Supplemental Material C, generated from 4b-mi-tests.R.

### Submission
-	240114-manuscript.pdf  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;PDF document of the final manuscript,
-	240114-manuscript.docx  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Word document of the final manuscript, generated by 240114-manuscript.Rmd.
-	240114-manuscript.Rmd  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;RMarkdown file to generate final manuscript.
-	apa7.csl  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;CSL file needed to run RMarkdown file.
-	mi-bibliography.bib  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Bib file needed to run RMarkdown file.
-	Table1.docx  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Word document of Table 1.

## License
Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a [Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
## Contact
For any inquiries or additional information, please contact Esther Maassen at esther@emaassen.com and Damiano D’Urso at dursodamiano@gmail.com

## Citation

Maassen, E., D’Urso, E. D., van Assen, M. A. L. M., Nuijten, M. B., De Roover, K., & Wicherts, J. M. (2023). The dire disregard of measurement invariance testing in psychological science. In Psychological Methods. American Psychological Association (APA). https://doi.org/10.1037/met0000624

