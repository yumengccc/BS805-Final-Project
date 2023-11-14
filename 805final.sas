options ls=70 ps=55 nofmterr; 
libname pjt805 '/home/u62266791/BS805_project'; 
 
/*import the three original dataset and rename the id column*/ 
data dem; set pjt805.demog_bs805_f22; rename demogid=id; run; 
data lab; set pjt805.labs_bs805_f22; rename labsid=id; run; 
data neu; set pjt805.neuro_bs805_f22; rename neuroid=id; run; 
 
 
/*Q1: combine the three data sets into a single, temporary SAS data set*/ 
proc sort data=dem; by id; run; 
proc sort data=lab; by id; run; 
proc sort data=neu; by id; run; 
data question1;  
     merge dem lab neu; 
     label ID='subject ID'; 
     label AGE='age in years'; 
     label MALE='=1 if male, =0 if female'; 
     label EDUCG='1 if education < 8 years, =2 if education >=8 years but no HS degree, =3 if HS degree but no college, = 4 if at least some college'; 
     label PKYRS='pack years of cigarette smoking'; 
     label HCY='plasma homocysteine level (μmol/L)'; 
     label FOLATE='plasma folate (nmol/mL)'; 
     label VITB12='plasma vitamin B12 (pmol/L)'; 
     label VITB6='plasma vitamin B6 (nmol/L)'; 
     label MMSE='Mini-Mental State Examination (a measure of cognitive function with range 0-30)'; 
     label ADIN7YRS='=0 if no AD in 7 years of follow-up, =1 if AD in 7 years of follow-up'; 
run; 
 
 
/*Q2: create a permanent SAS data set that includes new variables*/ 
data pjt805.question2; 
     retain ID MALE	EDUCG AGE PKYRS FOLATE VITB6 VITB12 HCY MMSE ADIN7YRS; 
     set question1; 
     LHCY = log(HCY); 
     HCYGE14 = (HCY >= 14); 
 
     if 65<=AGE<=74 then AGEGRP=1; 
     else if 75<=AGE<=79 then AGEGRP=2; 
     else if 80<=AGE<=84 then AGEGRP=3; 
     else if 85<=AGE<=89 then AGEGRP=4; 
      
     if 1<=EDUCG<=2 then HSDEG=0; 
     else if 3<=EDUCG<=4 then HSDEG=1; 
 
     if ADIN7YRS=. then EXCLUDE=1; 
     else EXCLUDE=0; 
      
     if MMSE=. then MMSEF=. ; 
     else if EDUCG=1 then MMSEF = (MMSE <= 22); 
     else if EDUCG=2 then MMSEF = (MMSE <= 24); 
     else if EDUCG=3 then MMSEF = (MMSE <= 25); 
     else if EDUCG=4 then MMSEF = (MMSE <= 26); 
 
     label LHCY='the natural log of HCY'; 
     label HCYGE14='1 for those whose HCY is at least 14; 0 for those whose homocysteine is less than 14';      
     label AGEGRP='1 for age 65-74, 2 for age 75-79, 3 for age 80-84, 4 for age 85-89';  
     label HSDEG='1 for High school degree or higher; 0 for Less than high school degree'; 
     label EXCLUDE='1 for those with missing ADIN7YRS, and 0 for those with non-missing ADIN7YRS';      
     label MMSEF='flags subjects with cognitive deficits according to the MMSE'; 
run; 
 
 
/*Q3: Perform appropriate statistical hypothesis tests to compare those excluded to  
those not excluded*/ 
data question3;  
     set pjt805.question2;  
run; 
 
%macro ttest(indat,clsva,varva); 
   proc ttest data=&indat; 
        class &clsva; 
        var &varva; 
        title "< Q3: T-test for &varva >"; 
   run; 
%mend ttest; 
 
%ttest(question3,EXCLUDE,AGE); 
%ttest(question3,EXCLUDE,PKYRS); 
%ttest(question3,EXCLUDE,MMSE); 
%ttest(question3,EXCLUDE,LHCY); 
 
%macro chisq(indat,clsva,varva); 
   proc freq data = &indat; 
        tables &clsva*&varva / chisq nocol nopercent; 
        title "< Q3: Chi-Square test for &varva >"; 
   run; 
%mend chisq; 
 
%chisq(question3,EXCLUDE,MALE); 
%chisq(question3,EXCLUDE,HSDEG); 
%chisq(question3,EXCLUDE,HCYGE14); 
 
 
/*Q4: create a new, temporary SAS data set excluding those with missing ADIN7YRS*/ 
data question4; 
     set pjt805.question2; 
     if EXCLUDE=0 then output; 
     drop EXCLUDE; 
run; 
 
 
/*Q5: vertical bar charts and generate descriptive statistics*/ 
%macro dscrip(indat,varva); 
   proc UNIVARIATE data= &indat; 
     var &varva; 
     HISTOGRAM/ NORMAL; 
     title "< Q5: vertical bar charts & descriptive statistics for &varva >"; 
   run; 
%mend dscrip; 
 
%dscrip(question4,PKYRS); 
%dscrip(question4,HCY); 
%dscrip(question4,LHCY); 
%dscrip(question4,FOLATE); 
%dscrip(question4,MMSE); 
 
 
/*Q6: Test if LHCY (dependent) linearly associated with continuous age*/ 
%macro reg(indat,depva,indva,Q_number); 
   proc reg data=&indat; 
     model &depva = &indva/ stb scorr2 clb;  
     title "< &Q_number: &depva (dependent) with &indva (independent) >"; 
   run;  
%mend reg; 
 
%reg(question4,LHCY,AGE,Q6); 
%reg(question4,MMSE,AGE,Q6); 
 
 
/*Q7: Test the null hypothesis that mean LHCY is the same in the four age groups*/ 
proc glm data=question4; 
     class AGEGRP; 
     model LHCY = AGEGRP / solution; 
     means AGEGRP;  
     means AGEGRP / tukey cldiff; 
     title '< Q7: Test the null hypothesis that mean LHCY is the same in the four age groups >'; 
run; 
 
 
/*Q8: Piecewise linear model to assess to association of log homocysteine (dependent variable)  
and age*/ 
data question8; 
     set question4; 
     if (65 <= age < 75) then age1=age; 
     else if age >= 75 then age1=75; 
      
     if (65 <= age < 75) then age2=75; 
     else if (75 <= age < 80) then age2=age; 
     else if age >= 80 then age2=80; 
      
     if (65 <= age < 80) then age3=80; 
     else if (80 <= age < 85) then age3=age; 
     else if age >= 85 then age3=85; 
      
     if (65 <= age < 85) then age4=85; 
     else if age>=85 then age4=age; 
run; 
 
proc sort data=question8; by age; run; 
proc sgplot data=question8; 
     series x=age y=age1; 
     series x=age y=age2; 
     series x=age y=age3; 
     series x=age y=age4; 
run; 
proc reg data=question8;  
     model LHCY = age1 age2 age3 age4 /stb scorr2 clb; 
     title '< Q8: Piecewise Linear Regression >'; 
run; 
 
 
/*Q9: Multiple linear regression with interaction with a dummy variable for gender to  
assess the relationship of LHCY to MMSE*/ 
proc sort data=question8; by id; run; 
proc glm data=question8; 
     class MALE; 
     model MMSE = LHCY MALE LHCY*MALE/ solution; 
     title '< Q9.1 : Multiple linear regression with interaction with a dummy variable for gender >'; 
run; 
 
 
/*Q10: linear regression model with MMSE as the outcome and LHCY as the single predictor*/ 
%reg(question8,MMSE,LHCY,Q10); 
 
 
/*Q11: Full Multiple linear regression model with LHCY, MALE, EDUCG (categorical),  
age (using your choice of variable determined above), and PKYRS as predictors of MMSE.*/ 
data question11; 
     set question8; 
     if AGEGRP = 1 then AGEGRP_1 = 1; else AGEGRP_1 = 0; 
     if AGEGRP = 2 then AGEGRP_2 = 1; else AGEGRP_2 = 0; 
     if AGEGRP = 3 then AGEGRP_3 = 1; else AGEGRP_3 = 0; 
     if EDUCG = 1 then EDUCG_1 = 1; else EDUCG_1 = 0; 
     if EDUCG = 2 then EDUCG_2 = 1; else EDUCG_2 = 0; 
     if EDUCG = 3 then EDUCG_3 = 1; else EDUCG_3 = 0; 
run; 
 
proc reg data=question11; 
     model MMSE = LHCY MALE EDUCG_1 EDUCG_2 EDUCG_3 AGEGRP_1 AGEGRP_2 AGEGRP_3 PKYRS/ r stb scorr2 clb tol vif collinoint; 
     id ID; 
     output out=out11 predicted=p_mmse student=r_mmse press=pr_mmse; 
     title '< Q11: Full mutiple linear regression >'; 
run; 
 
proc sgplot data=out11; 
     scatter x=p_mmse y=r_mmse; 
     title 'Residual Plot'; 
run; 
 
proc univariate plots normal data=out11; 
     id ID; 
     var r_mmse; 
     title 'Normality of Residuals'; 
run; 
 
proc univariate plots data=out11;      
     id ID; 
     var pr_mmse; 
     title 'Press Residuals'; 
run; 
 
 
/*Q12: Using PROC GLMSELECT and LASSO with an AIC-based selection criterion,  
identify the best model using LHCY, MALE, EDUCG (categorical),  
age (using you choice of variable determined above), and PKYRS as predictors of MMSE.*/ 
proc glmselect data=question11; 
     class MALE EDUCG AGEGRP; 
     model MMSE = LHCY MALE EDUCG AGEGRP PKYRS / selection=lasso (stop=none choose=aic); 
     title '< Q12: LASSO with AIC selection criterion - Using CLASS statement >'; 
run; 
 
proc glmselect data=question11; 
     model MMSE = LHCY MALE EDUCG_1 EDUCG_2 EDUCG_3 AGEGRP_1 AGEGRP_2 AGEGRP_3 PKYRS / selection=lasso (stop=none choose=aic); 
     title '< Q12: LASSO with AIC selection criterion - Using dummy variables >'; 
run; 
