



frequencies variables = post.infected post.objcold post.mucwt_tot post.nasclr_avg 
/statistics mean stddev median minimum maximum.

compute log_post.nasclr_avg = lg10(post.nasclr_avg+1).
compute log_post.mucwt_tot = lg10(post.mucwt_tot+1).
execute.


USE ALL.
COMPUTE filter_$=(post.infected=1).
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

frequencies variables = post.mucwt_tot post.nasclr_avg 
/statistics mean stddev median minimum maximum.


USE ALL.

frequencies variables = di.totcomplete
/statistics mean stddev median minimum maximum.


*COMPARISON OF REPORTS OF HUGS WITH REPORTS OF TENSION.

NPTESTS 
  /RELATED TEST(pcthug pctten) 
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE
  /CRITERIA ALPHA=0.05  CILEVEL=95.

frequencies variables = pcthug pctten
/statistics mean stddev median minimum maximum.

correlations variables = isel12tot pcthug pctten.

USE ALL.
COMPUTE filter_$=(not(missing(bodymass))).
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*****RELATIONS OF COVARIATES WITH INFECTION*****.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER sex
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER study.id
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER race.white
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER pre_ab_ge4
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER bodymass
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER season
  /CATEGORICAL = season
  /CONTRAST (season)simple(1)
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER spring
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER virus
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER educ.4cat
  /CATEGORICAL = educ.4cat
  /CONTRAST (educ.4cat) simple
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER sni.hcr.married
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).


*****RELATIONS OF COVARIATES WITH COLDS*****.

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER age
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER sex
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER study.id
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER race.white
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER pre_ab_ge4
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER bodymass
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER season
  /CATEGORICAL = season
  /CONTRAST (season)simple(1)
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER spring
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER virus
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER educ.4cat
  /CATEGORICAL = educ.4cat
  /CONTRAST (educ.4cat) simple
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER sni.hcr.married
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).



*****ILLNESS SIGNS AMONG INFECTED SUBSET*****.

USE ALL.
COMPUTE filter_$=(not(missing(bodymass)) and post.infected = 1).
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*****UNIVARIATE ASSOCIATIONS OF COVARIATES WITH MUCUS WEIGHTS*****.

correlations variables = post.mucwt_tot post.mucwt_tot
/statistics descriptives.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER sex.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER race.white.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER study.id.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER pre_ab_ge4.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER bodymass.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER spring summer fall.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER winter.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER virus.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER educ.hschl educ.lt2yr educ.assoc.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER sni.hcr.married.



*****UNIVARIATE ASSOCIATIONS OF COVARIATES WITH NASAL CLEARANCE TIME*****.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER age.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER sex.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER caucasian.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER study.id.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER pre_ab_ge4.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER bmi.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER spring summer fall.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_adj
  /METHOD=ENTER spring.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_adj
  /METHOD=ENTER virus.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_adj
  /METHOD=ENTER educ.hschl educ.lt2yr educ.assoc.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_adj
  /METHOD=ENTER sni.hcr.married.



***** MAIN ANALYSES: RISK FOR INFECTION ****.

* (1)  TENSION AND PERCEIVED SOCIAL SUPPORT.

* FULL MODEL.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married
  /METHOD=ENTER cisel12tot
  /METHOD=ENTER cpctten
  /METHOD=ENTER cisel12tot*cpctten
  /PRINT=CI(95) GOODFIT CORR ALL
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5)
  /SAVE=PRED (tenxisel1).

* MODEL WITHOUT COVARIATES.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER cisel12tot
  /METHOD=ENTER cpctten
  /METHOD=ENTER cisel12tot*cpctten
  /PRINT=CI(95) GOODFIT CORR
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).



* FIGURE 3. Adjusted predicted probability of infection with increasing days of interpersonal tension among participants with high or low levels of perceived social support.  

frequencies variables = isel12tot
/statistics median stddev
/format notable.

RECODE isel12tot (lowest thru 41 = 0)(42 thru highest = 1) into isel12tot_mdn.
EXECUTE.

GRAPH
/SCATTERPLOT (BIVARIATE) pctten with iselxten1 by isel12tot_mdn.


* CREATE P-VALUES FOR LINES.

compute hicisel12tot = cisel12tot-5.57.
compute locisel12tot = cisel12tot+5.57.
execute.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married  
  /METHOD=ENTER cpctten hicisel12tot
  /METHOD=ENTER cpctten*hicisel12tot
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married 
  /METHOD=ENTER cpctten locisel12tot
  /METHOD=ENTER cpctten*locisel12tot
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).



* (2) TENSION AND HUGS.

* FULL MODEL.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married
  /METHOD=ENTER cpcthug
  /METHOD=ENTER cpctten
  /METHOD=ENTER cpcthug*cpctten
  /PRINT=CI(95) GOODFIT CORR ALL
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5)
  /SAVE=PRED (tenxhug1).

* MODEL WITHOUT COVARIATES.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER cpcthug
  /METHOD=ENTER cpctten
  /METHOD=ENTER cpcthug*cpctten
  /PRINT=CI(95) GOODFIT CORR ALL
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).


* FIGURE 4.  Adjusted predicted probability of infection with increasing days of interpersonal tension among participants with high or low frequency of being hugged.  

frequencies variables = pcthug
/statistics median stddev
/format notable.

RECODE pcthug (lowest thru .6786 = 0)(.6786 thru highest = 1) into pcthug_mdn.
EXECUTE.

GRAPH
/SCATTERPLOT (BIVARIATE) pctten with tenxhug1 by pcthug_mdn.


* CREATE P-VALUES FOR LINES.

compute hicpcthug = cpcthug-0.34.
compute locpcthug = cpcthug+0.34.
execute.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married  
  /METHOD=ENTER cpctten hicpcthug
  /METHOD=ENTER cpctten*hicpcthug
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married 
  /METHOD=ENTER cpctten locpcthug
  /METHOD=ENTER cpctten*locpcthug
  /PRINT=CI(95)
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).



* (3) CONTROLLING FOR FREQUENCY OF SOCIAL INTERACTION.

correlations variables = pctten pcthug di.socdays di.totsoc_avg.

do if di.socdays gt 0.

compute pcthugsoc = di.hugdays/di.socdays.
compute pcttensoc = di.tendays/di.socdays.

end if.

do if di.socdays = 0.

compute pcthugsoc = 0.
compute pcttensoc = 0.

end if.

execute.

frequencies variables = pcthugsoc pcttensoc
/statistics mean stddev
/format notable.

compute cpcthugsoc = pcthugsoc - 0.67.
compute cpcttensoc = pcttensoc - 0.15.
execute.


LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married di.totsoc_avg
  /METHOD=ENTER pcthugsoc
  /METHOD=ENTER pcttensoc
  /METHOD=ENTER pcthugsoc*pcttensoc
  /PRINT=CI(95) GOODFIT CORR ALL
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).



* (4) IS THE BUFFERING EFFECT OF HUGS RESPONSIBLE FOR SUPPORT BUFFERING TENSION?.

LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married
  /METHOD=ENTER cpcthug cpctten cisel12tot
  /METHOD=ENTER cisel12tot*cpctten
  /METHOD=ENTER cpcthug*cpctten
  /PRINT=CI(95) GOODFIT CORR ALL
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).



* (5) DO BUFFERING HUGS OCCUR ON TENSION DAYS?.

USE ALL.
COMPUTE filter_$=(not(missing(bodymass)) and di.tendays ge 1).
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

NPTESTS 
  /RELATED TEST(pcthugten pcthugnoten) 
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
  /CRITERIA ALPHA=0.05  CILEVEL=95.


LOGISTIC REGRESSION VARIABLES post.infected
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married
  /METHOD=ENTER pcthugnoten
  /PRINT=CI(95) GOODFIT CORR ALL
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).



**** MAIN ANALYSES: SIGNS OF ILLNESS AMONG INFECTED PARTICIPANTS ****.

USE ALL.
COMPUTE filter_$=(not(missing(bodymass)) and post.infected = 1).
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

* (1) TENSION AND PERCIEVED SOCIAL SUPPORT.

*NASAL CLEARANCE - FULL MODEL.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE zpp
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married 
  /METHOD=ENTER cisel12tot cpctten
  /METHOD=ENTER cpctten_cisel12tot.


*NASAL CLEARANCE - MODEL WITHOUT COVARIATES.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE zpp
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER cisel12tot cpctten
  /METHOD=ENTER cpctten_cisel12tot.


*MUCUS WEIGHTS - FULL MODEL.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE zpp
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married 
  /METHOD=ENTER cisel12tot cpctten
  /METHOD=ENTER cpctten_cisel12tot.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE zpp
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER cisel12tot cpctten
  /METHOD=ENTER cpctten_cisel12tot.


* (2) TENSION AND HUGS.

*NASAL CLEARANCE - FULL MODEL.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married 
  /METHOD=ENTER cpcthug cpctten
  /METHOD=ENTER cpctten_cpcthug.


*NASAL CLEARANCE - MODEL WITHOUT COVARIATES.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE ZPP
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER cpcthug cpctten
  /METHOD=ENTER cpctten_cpcthug.


*MUCUS WEIGHTS - FULL MODEL.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE zpp
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.mucwt_tot
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married 
  /METHOD=ENTER cpcthug cpctten
  /METHOD=ENTER cpctten_cpcthug.



* (3) SIMULTANEOUS EXAMINATION OF SUPPORT AND HUGS.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA CHANGE zpp
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT log_post.nasclr_avg
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married 
  /METHOD=ENTER cisel12tot cpcthug.



*** MAIN ANALYSES: CLINICAL ILLNESS ****.

USE ALL.

*TENSION AND PERCEIVED SUPPORT - FULL MODEL.

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married
  /METHOD=ENTER cisel12tot
  /METHOD=ENTER cpctten
  /METHOD=ENTER cisel12tot*cpctten
  /PRINT=CI(95) GOODFIT CORR
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).


*TENSION AND HUGS - FULL MODEL.

LOGISTIC REGRESSION VARIABLES post.objcold
  /METHOD=ENTER age sex study.id race.white pre_ab_ge4 bodymass spring summer fall virus educ.hschl educ.lt2yr educ.assoc sni.hcr.married
  /METHOD=ENTER cpcthug
  /METHOD=ENTER cpctten
  /METHOD=ENTER cpcthug*cpctten
  /PRINT=CI(95) GOODFIT CORR ALL
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).






