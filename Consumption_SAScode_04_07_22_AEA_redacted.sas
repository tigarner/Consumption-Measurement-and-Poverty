/**************************************************************************************************************************************
/**************************************************************************************************************************************
/*****************************************           ARMSTRONG et al., AEA P&P 2022       *********************************************                                                         
/**************************************************************************************************************************************
/**************************************************************************************************************************************


/**The purpose of this SAS code is to produce the output presented in the Armstrong et al paper published in the 2022 AEA Papers and Processings (P&P).
   The code was produced in the early months of 2022 and is based on earlier code used to produce the January 2022 AEA conference results.
   The code and data needed to run the SAS program are saved on a server internal to BLS. The internal data are accessible to researchers 
   with approved projects under the BLS Outside Researcher Program; see: https://www.bls.gov/rda/ .
   
   This SAS program is to be used with internal BLS data ONLY; it is not transferrable to be used directly with CE public use data (PUMD).
     
   
   The code is referred to as "Consumption_SAScode_04_07_22_AEA_redacted.sas" 
   The input data are stored on a BLS server in the following folder: "/Consumption_Measure/Data_READin/" 
   
   Some of the variables referred to in this code have the same names as the variables in the CE internal data while other variables 
   were created using other code. In some instances, the CE-specific data file variables based on BLS internal data are the same as 
   the PUMD data files,for example those for demographic variables, while others differ, specifically the expenditure variables. 
   For example, the BLS internal variable for total food expenditures, ZFOODTOT, can be reproduced using the PUMD data by summing the values for
   the following variables: FOODCQ and FOODPQ. 

   Data are referenced following the BLS CE Program format as being from 2014Q2-2021Q1. This means these data were collected during these calendar quarters.

  In this program, relative poverty thresholds are produced for various measures, with poverty rates based on relative thresholds that match the measures.
   The relative poverty thresholds are output into an excel file named t_thresholds.xlsx 
   The poverty rates based on the relative thresholds are output into an excel file named povrate_pure_relative.xlsx
   This excel spreadsheet also includes the consumption_4 poverty rate for 2015 that is used as the anchor rate to produce the absolute thresholds.

   To produce absolute thresholds, poverty rates in 2015 are anchored to be the same for all measures as the poverty rate
   in 2015 when measure is consumption_4 and the associated relative threshold. Poverty thresholds for other measures differ but rates
   are the same for 2015 -- the anchor year.
   Absolute thresholds for 2015 for all measures are output into an excel file names thres.xlsx . For precision, rather than use the 2015 consumption_4
      relative threshold from the proc univariate used to produce thres.xlsx, use the one from spreadsheet t_thresholds.xlsx.  
      For all othher measures, use the 2015 thresholds in excel file thres.xlsx  

   In the following code, several economic measures are produced. For the AEA P&P, only the output for the following are included
    1. tot_exp   which refers to total expenditures as defined by the BLS CE (with the exception that miscellaneous 2 is not included (see text below regarding this)
    2. an income measure which is referred to as Census_income_dis_annual. This is the after tax and transfer income referred to in the AEA P&P, 
    3. a consumpton measure which is referred to a consumption_4 in the code but as consumption in the AEA P&P.

   For Figure 1: Median 2A+2C equivalized 3 measures in 2015 dollars, for 2015 to 2020 exported to excel file "med_3Meas_2015_allyr.xlsx" 
   For Figure 2: Poverty rates based on relative poverty thresholds are exported to excel file "new_3Meas_Povrate_pure_rel.xlsx"
   For Figure 3: Poverty rates based on anchored rates are exported to excel file "new_3Meas_Anchorrate_pov.xlsx"
   For Table: Means for demographic characteristics for 4 unweighted samples:
		All people acorss all CUs: "full_AEA_demo_2015.xlsx" 
		Below absolute thresholds based on being below consumption_4 poverty threshold with anchor rate set 
                 to relative poverty rate for consumption_4 in 2015: "demo_AEA_conspoor_2015.xlsx"  
		Below absolute thresholds based on being below after tax and transfer poverty threshold with anchor rate set 
                 to relative poverty rate for consumption_4 in 2015: "demo_AEA_incpoor_2015.xlsx"  
		Below both absolute thresholds for consumption_4 and after tax and transfers with anchor rate set 
                 to relative poverty rate for consumption_4 in 2015: "demo_AEA_dpconpoor_2015.xlsx"  

   NOTE:  when comparing medians and poverty rates in AEA PP and from program run on 3-9-22, all are the same.
	  When comparing Table 1 percentage distributions to output from program 3-9-22, results are the same as presented in Table 1.	
**/


LIBNAME FOLDER "Consumption_Measure/Data_Short/";   /**for output files when run using internal data**/

*******************************************************************************************************************************;
***** Import CE_vars_and_subsid data file that includes variables downloaded plus imputed rents produced by Munoz 12-22-21     ; 
*****	See variables downloaded using program "TEST_Consumption_New inkind project 03_07_22.sas"                              ;
*****	These data were downloaded to a BLS local directory and copied over to Consumption_Measure folder for archive          ;
***** Import vehicle data, which are also based on internal CE data, and downloaded to a local directory                       ;                                                               ;    
*******************************************************************************************************************************;

DATA WORK.CE_vars_and_subsid; 
	SET "Consumption_Measure/Data_READin/CE_vars_and_subsid.sas7bdat";
	DROP NEWID;
	RENAME FAMID = NEWID;
RUN; 

DATA WORK.VEHICLE_RAW;
     SET "Consumption_Measure/Data_READin/CE_Vehicle_Flows.sas7bdat";
     DROP QTR;
RUN; 


DATA WORK.VEHICLE;
	SET VEHICLE_RAW;
   	DROP NEWID;
	DROP FAMID; 

	NEW2 = INPUT(QINTRVMO, 12.);
	DROP QINTRVMO;
	RENAME NEW2 = QINTRVMO;

	**creation of yr, qtr, and yrqtr **;
  	IF 1<=QINTRVMO<=3 THEN QTR='1';
    ELSE IF 4<=QINTRVMO<=6 THEN QTR='2';
    ELSE IF 7<=QINTRVMO<=9 THEN QTR='3';
    ELSE IF QINTRVMO>=10 THEN QTR='4';
  	IF QINTRVYR<99 OR (QINTRVYR='99' AND 1<=qintrvmo<=3) THEN
        QINTRVYR=QINTRVYR+1900;
  	YR=QINTRVYR;

  	HOLD=YR||QTR;
	YRQTR=INPUT(HOLD,12.);

	NEW3 = INPUT(QINTRVYR, 12.);
	DROP QINTRVYR;
	RENAME NEW3 = QINTRVYR;
	NEW4 = INPUT(COMID, 12.);
	DROP COMID;
	RENAME NEW4 = NEWID;
RUN;

DATA VEHICLE_RESTRICT;
	SET VEHICLE;
	IF YRQTR >= 20142;

	NEW = PUT(YRQTR,5.);
	DROP YRQTR;
	RENAME NEW = YRQTR;
RUN; 


PROC SORT DATA =  CE_vars_and_subsid; 
	BY NEWID;
RUN;

PROC SORT DATA = VEHICLE_RESTRICT;
	BY NEWID;
RUN;

DATA CONSUMPTION; 
	MERGE CE_vars_and_SUBSID (in=yes) VEHICLE_RESTRICT;
	BY NEWID;
	IF YES;
RUN; 

PROC MEANS DATA=CONSUMPTION;
RUN;



proc contents data=consumption;
run;
*******************************************************************************************************************;
** use 3-parameter equivalence scale so all units used to create 60% of median equivalennt consumption threshold **;
****************************************************full data set**************************************************;
 data All_qtrs;
   set consumption;         

   year = input(cyear,12.);
 ** adult, spouse, child, unrelated individuals from input file**;
    famsizex=adultx+spouygx+childx+unrelakx+unrelaax;
    
    adult_cu=adultx+spouygx+unrelaax;
    child_cu=childx+unrelakx;
    
      
      
        
    /** 3-parameter scale**/
     scaleb=0;

      	  IF (adult_cu=1 AND child_cu=0) THEN scaleb=1.0;
          ELSE IF (adult_cu=2 AND child_cu=0) THEN scaleb=1.41;
          ELSE IF (adult_cu>2 AND child_cu=0) THEN scaleb=adult_cu**0.7;
          ELSE IF (adult_cu=1 AND child_cu>0) THEN DO;
    		  IF child_cu=1 THEN scaleb=(adult_cu+0.8*child_cu)**0.7;
          	  ELSE scaleb=(adult_cu+0.8+0.5*(child_cu-1))**0.7;
          END;
          ELSE IF (adult_cu>=2 and child_cu>0) THEN scaleb=(adult_cu+.5*child_cu)**0.7;
    

rent_qtrly_new=0;

if rentx3<0 then rentx3_new=0;
else if rentx3>=0 then rentx3_new=rentx3;

if  cutenure>3 then do;
   rent_qtrly=rentx3_new*3;          /**should be missing for all owners but to make sure, will deal with below**/
   rent_qtrly_new=rent_qtrly;
end;

  if cutenure<3 then do;  /**all owners assigned zero rents for the rental equivalence**/
     rentx3_new=0;
     rent_qtrly_new=0;
  end;
      

imputed_rent_qtry=0;

if imputedrent_rentsub_nom>0 then do;
   imputed_rent_qtry=imputedrent_rentsub_nom*3;
  end;


   /**assignment of imputed rent**/
   rent_difference = 0; 
   rent_imputed_d=0;

   public_imput_d=0;
   govtc_imput_d=0;
   rentcont_imput_d=0; 
   
  
if (4<=cutenure<=5) & (publhous_new ='1' or govtcost_new='1' or rentcont_new_1 = '1' )  
                    & (ZOWNDWLL<=0) & (bathrooms>=1) 
                    & (rentx3_new>0) then do;

   paidrent = 0;                                                  
   rent_difference = sum(0,imputed_rent_qtry)-sum(0,rent_qtrly_new);

   if paidrent = 0 & rent_difference > 0 then rent_imputed_d = 1;
	 
   if paidrent = 0 & rent_difference <= 0 then rent_imputed_d = 0;
	  

   if rent_imputed_d = 1 then  rent_qtrly_new = imputed_rent_qtry;
	  
   if rent_difference <= 0 then     rent_difference = 0; 
	   

   end;

   if rent_qtrly_new<0 then rent_qtrly_new=0;

rent_imp_gp_1=0;
rent_imp_gp_2=0;

If rent_imputed_d =1 then rent_imp_gp_1=1;
   else if rent_imputed_d =0 then rent_imp_gp_2=1;     
  
  if publhous_new ='1' & rent_difference > 0 then   public_imput_d=1;
  if govtcost_new='1' & rent_difference > 0 then    govtc_imput_d=1;
  if rentcont_new = '1' & rent_difference > 0 then  rentcont_imput_d=1;
 
pub_hous_imp_gp_1=0;
pub_hous_imp_gp_2=0;

If public_imput_d=1 then pub_hous_imp_gp_1=1;
   else if public_imput_d=0 then pub_hous_imp_gp_2=1; 
      
gov_cost_imp_gp_1=0;
gov_cost_imp_gp_2=0;

If govtc_imput_d=1 then gov_cost_imp_gp_1=1;
   else if govtc_imput_d=0 then gov_cost_imp_gp_2=1; 

rent_cont_imp_gp_1=0;
rent_cont_imp_gp_2=0;

If rentcont_imput_d=1 then rent_cont_imp_gp_1=1;
   else if rentcont_imput_d=0 then rent_cont_imp_gp_2=1; 
**end of imputed rents**

********************************;
**flow for cars and trucks**;
********************************;

dep_car_qtry= dep_car/4;
dep_trk_qtry= dep_trk/4;
int_car_qtry= int_car/4;
int_trk_qtry= int_trk/4;

vehicle_flow=sum(0,dep_car_qtry,dep_trk_qtry,int_car_qtry,int_trk_qtry);

veh_flow_positive=0;
if vehicle_flow>0 then veh_flow_positive=1;

veh_flow_zero=0;
if vehicle_flow=0 then veh_flow_zero=1;

veh_flow_negative=0;
if vehicle_flow<0 then veh_flow_negative=1;


veh_flow_gp_1=0;
veh_flow_gp_2=0;
veh_flow_gp_3=0;

If veh_flow_positive=1 then veh_flow_gp_1=1;
if veh_flow_zero=1 then veh_flow_gp_2=1; 
if veh_flow_negative=1 then veh_flow_gp_3=1; 


 
/**total expenditures below not including miscellaneous-2 expenditures since these only collected in last scheduled interview
January 20 2022 --set zowndwll for non-owners to zero before producing tot_exp
was told by CE staff on January 7, 2022 that there was a problem in way Zowndwll created in past for nonowners/
new code on January 20, 2022 for zowndwll
and February 9 to  move housing that is reimbursable unitl health insurance back to health from housing. 
See below from how variables created in previous program.

	ZOWNDWLL	ZMORTINT+ZPROPTAX+ZMREPINS
	ZHOUSING	ZSHELTER+ZUTILSPS+ZHOUSEOP+ZHOUSEFE
	ZSHELTER	ZOWNDWLL+ZRENTDWL+ZOTHRLOD
	zhousing_new= sum(0,ZSHELTER,ZUTILSPS,zhouseop_new,zhousefe)

**/

ZTOTAL_new = sum(0,ZFOODTOT,ZALCBEVS,ZHOUSING_new,ZAPPAREL,ZTRANPRT,ZHEALTH_new,ZENTRMNT,ZPERCARE, 
ZREADING,ZEDUCATN,ZTOBACCO,ZMISCEL1/**,ZMISCEL2**/,ZCASHCTB,ZPERLINS);  /**zmisceL2 only in last sch interview**/


/**NOTE: since 2013Q2, in the CE internal and PUMB data, ZMISCEL2 is $0 for all CUs due to a coding issue by CE
	ZMISCELS+ZCASHCTB+ZPERLINS   
	ZMISCELS=ZMISCEL1+ZMISCEL2    
	ZMISCEX4=ZMISCEL1+ZMISCEL2*4
if BLS CE Programs corrects values for Zmiscel2, then code to tot_exp will still work
zmiscel2 includes finance, late, and interest charges on credit cards, other loans, and student loans
the CE public use variables for totexppq and totexpcq do NOT include the UCCs for zmiscel2 
It is possible to create zmiscel2 using ITAB UCCs and then multiplying by 4 for annual but did not do that for this project 
**/

tot_exp=ZTOTAL_new-sum(0,ZMISCEL2); /**Subtracting ZMISCEL2 from here since only in last scheduled interview**/

tot_exp_neg=0;
if tot_exp<0 then do;
  tot_exp_neg=1;
end;


spending_consump = sum(0,ZFOODTOT,ZALCBEVS,ZHOUSING_new,ZAPPAREL,ZTRANPRT,ZHEALTH_new,ZENTRMNT,ZPERCARE, 
ZREADING,ZEDUCATN,ZTOBACCO,ZMISCEL1/**,ZMISCEL2,ZCASHCTB,ZPERLINS**/);


spn_cons_neg=0;
if spending_consump<0 then do;
  spn_cons_neg=1;
end;

/**Here assume expenditures for housing while at school are education expenses. Day care expenditures are also education**/
 
spending_consump_noHED = sum(0,ZFOODTOT,ZALCBEVS,ZHOUSING_new,ZAPPAREL,ZTRANPRT,/**ZHEALTH_new,**/ZENTRMNT,ZPERCARE, 
             ZREADING,/**ZEDUCATN,**/ZTOBACCO,ZMISCEL1/**,ZMISCEL2,ZCASHCTB,ZPERLINS**/)
            - sum(0,Aw_school_housing,day_care); 

spn_cons_noHED_neg=0;
if spending_consump_noHED<0 then do;
  spn_cons_noHED_neg=1;
end;

/**To produce quarterly rent equivalence, when based on MTAB, need to multiply rnteq by 4 for monthly value and then 3 for quarterly**/
/**
       rnteqx_own  monthly rental equivalence primary residence
       renteq_vac_not    monthly rental equivalence vacation home not rented
       renteq_vac_rent   monthly rental equivalence vacation home available for rent
       renteq_time_sh    quarterly rental equivalence for time shares
**/
rnteqx_own_qtr=sum(0,rnteqx_own)*3*4;
if rnteqx_own_qtr <= 0 then rnteqx_own_qtr = 0; 

renteq_vac_not_qtr= sum(0,renteq_vac_not)*3*4;
if renteq_vac_not_qtr <= 0 then renteq_vac_not_qtr = 0;

renteq_vac_rent_qtr=sum(0,renteq_vac_rent)*3*4;
if renteq_vac_rent_qtr <= 0 then renteq_vac_rent_qtr = 0; 

renteq_time_sh_qtr=sum(0,renteq_time_sh);          /**no adjustment needed to make quarterly. JRNTEQ3X (Quarterly 
                                                   expenditure of rental equivalence for properties that are timeshares)**/
if renteq_time_sh_qtr <= 0 then renteq_time_sh_qtr = 0; 

rnteqv_d=0;
rnteq_vac_not_d=0;
rnteq_vac_rent_d=0;
rnteq_timesh_d=0;


if rnteqx_own_qtr>0 then rnteqv_d=1;
if renteq_vac_not_qtr>0 then rnteq_vac_not_d=1;
if renteq_vac_rent_qtr>0 then rnteq_vac_rent_d=1;
if renteq_time_sh_qtr>0 then rnteq_timesh_d=1;

rnteqv_pos_gp_1=0;
rnteqv_pos_gp_2=0;

If rnteqv_d=1 then rnteqv_pos_gp_1=1;
   else if rnteqv_d=0 then rnteqv_pos_gp_2=1; 

rnteqvac_not_gp_1=0;
rnteqvac_not_gp_2=0;

If rnteq_vac_not_d=1 then rnteqvac_not_gp_1=1;
   else if rnteq_vac_not_d=0 then rnteqvac_not_gp_2=1; 

rnteqvac_rent_gp_1=0;
rnteqvac_rent_gp_2=0;

If rnteq_vac_rent_d=1 then rnteqvac_rent_gp_1=1;
   else if rnteq_vac_rent_d=0 then rnteqvac_rent_gp_2=1; 

rnteq_timesh_gp_1=0;
rnteq_timesh_gp_2=0;

If rnteq_timesh_d=1 then rnteq_timesh_gp_1=1;
   else if rnteq_timesh_d=0 then rnteq_timesh_gp_2=1; 


***  below code so only primary residence CUs can get zowndwll***;

if 1<=cutenure<=3 then do;
   ZOWNDWLL_13= ZOWNDWLL;
end;


if cutenure>3 then do;        /**only owners living in primary residence can get rental equivalence for this housing**/
    rnteqx_own_qtr=0;
end;

rentx3_qtr=rentx3_new*3;


if cutenure < 4 then do;        /*replacing rents for renters that might have been assigned to owners. But keeps zrentxrp in zshelter for owners**/
  rentx3_qtr =0;
  rent_only=0; 
end;

if 5<=cutenure<=6 then do;     /**keeping rents based on MTAB UCC to create rent_only, if a value but cutenure indicates living rent free or in student housing**/
    rentx3_qtr =rent_only; 
end;

if cutenure=4 then do;         /**for any rents paid by any renter, replacing the rent only portion**/
    rentx3_qtr =rentx3_qtr; 
end;

**only subtracting zowndwll if cutenure is = 1 or 2 since replacing this with rental equivalence. All other expenses in zowndwll staying in***;
                           
/**creation of variables for vacation properties**/

vac_home_uti=sum(0,fuelwatv);  

vacation_exp=sum(0,vac_mortinto,vac_homeeq_loan_int,vac_homeeq_line_int, vac_zproptax,vac_zmrepins);

/**to consistency with CE PUMB as the following variable not in PUMB -- rental equivalence for vacation home available for rent**/
/**results in inconsistency in the way utilities defined, but UCCs for vacation homes all lumped together and thus cannot distinguish**/                     
                               
/**subtracting out all purchases of new and used cars and trucks, all other vehicles like motor cycles planes etc, finance charges 
for all vehicles. Also subtract expenditures for motorized recreational vehicles that BLS CE include in entertainment variable**/

************in-kind benefits. These are inputes from TEST consumption program**;

if NSLP_sub_full<=0 then NSLP_sub_full=0;

if WIC_sub_full<=0 then WIC_sub_full=0;

if income_WIC_full<=0 then income_WIC_full=0;

if LIHEAP_sub_full<=0 then LIHEAP_sub_full=0;


if NSLP_sub_full>0 then NSLP_yes_d=1;
  else if NSLP_sub_full<=0 then NSLP_yes_d=0;

if WIC_sub_full>0 then WIC_yes_d=1;
  else if WIC_sub_full<=0 then WIC_yes_d=0;

if income_WIC_full>0 then inc_WIC_yes_d=1;
  else if income_WIC_full<=0 then inc_WIC_yes_d=0;

if LIHEAP_sub_full>0 then LIHEAP_yes_d=1;
  else if LIHEAP_sub_full<=0 then LIHEAP_yes_d=0;


NSLP_yes_gp_1=0;
NSLP_yes_gp_2=0;

WIC_yes_gp_1=0;
WIC_yes_gp_2=0;

inc_WIC_yes_gp_1=0;
inc_WIC_yes_gp_2=0;

LIHEAP_yes_gp_1=0;
LIHEAP_yes_gp_2=0;

if NSLP_yes_d=1 then NSLP_yes_gp_1=1;
if NSLP_yes_d=0 then NSLP_yes_gp_2=1;

if WIC_yes_d=1 then WIC_yes_gp_1=1;
if WIC_yes_d=0 then WIC_yes_gp_2=1;

if inc_WIC_yes_d=1 then inc_WIC_yes_gp_1=1;
if inc_WIC_yes_d=0 then inc_WIC_yes_gp_2=1;

if LIHEAP_yes_d=1 then LIHEAP_yes_gp_1=1;
if LIHEAP_yes_d=0 then LIHEAP_yes_gp_2=1;

rental_asst_yes_gp_1=0;
rental_asst_yes_gp_2=0;

if (publhous_new ='1' or govtcost_new='1' or rentcont_new_1 = '1' ) then do;
  rental_asst_yes_gp_1=1;
End;

if rental_asst_yes_gp_1=0 then do;
  rental_asst_yes_gp_2=1;
End;


/**rent_qtrly_new is the imputed rent when rent difference>0 for paidrent NE 1 group**/ 
/**only adding the value of the rent subsidy which is the difference in sum of ((imputed rent based on RENTX3)+renterexp) and zrentxrp**/
 consumption_4= spending_consump_noHED- sum(0,ZOWNDWLL_13,owner_maj_app,renter_maj_app,vac_home_uti,vacation_exp)
                               + sum(0,rnteqx_own_qtr)
                               - sum(0,rent_only)
                               + sum(0,rentx3_qtr)
                               + sum(0,vac_home_uti,renteq_vac_not_qtr/*,renteq_vac_rent_qtr*/)
							   + sum(0,vehicle_flow)-sum(0,ZCARTRKN,ZCARTRKU,ZOTHRVEH,ZVEHFINC,MOTOR_Rec_Veh)
							   + sum(0,rent_difference,NSLP_sub_full,WIC_sub_full,LIHEAP_sub_full);

cons_4_neg=0;
if consumption_3<0 then do;
  cons_4_neg=1;
end;

/**note:  CE defintion of before tax income includes rent and meals as pay so it needs to be subtract to equal Census defintion of income**/

census_inc_official=(fincbtxm-sum(0,JFS_AMT))/4; /**now official income in quarterly values**/

census_inc_neg=0;
if census_inc_official<0 then do;
	census_inc_neg=1;  
end;  


/**after AEA we also subtract FICA that is estimated using TAXSIM -- summed over all tax units in CU**/

Census_income_dis_annual=sum(0,finatxem,rent_difference,NSLP_sub_full,income_WIC_full,LIHEAP_sub_full)-sum(0,FICA);

Census_income_qtr=Census_income_dis_annual/4;  

census_inc_d_neg=0;
if census_income_qtr<0 then do;
   * census_inc_qtr=0;
    census_inc_d_neg=1;
end; 
  
**************expenditure, consumption, income equivalized values*****************;

rent_new=sum(0,rentx3_qtr);

rent_new_equiv=rent_new/scaleb;

zown_oop=sum(0, ZOWNDWLL_13);

zown_oop_equiv=zown_oop/scaleb;

majapp_oop=sum(0,owner_maj_app,renter_maj_app); 

majapp_oop_equiv=majapp_oop/scaleb;

rnteqx_own_qtr_equiv = rnteqx_own_qtr/scaleb;
renteq_vac_not_qtr_equiv = renteq_vac_not_qtr/scaleb;
renteq_vac_rent_qtr_equiv = renteq_vac_rent_qtr/scaleb;
renteq_time_sh_qtr_equiv = renteq_time_sh_qtr/scaleb;
vehicle_flow_equiv= vehicle_flow/scaleb;

cen_off_inc_equiv= census_inc_official/scaleb;

spending_equiv=tot_exp/scaleb;

spen_cons_equiv=spending_consump/scaleb;  

consump_4_equiv =consumption_4/scaleb;   /**see consumption_4 definition earlier**/
   
income_disp_equiv=Census_income_qtr/scaleb; /**qtrly after tax income based on TAXSIM imputations, includes SNAP and all in-kind, minus FICA**/
   
/**person weight variable**/
person_wt=(finlwt21*fam_size);   
   
  
/**CONVERT ADULT EQUIVALIZED VALUES INTO THOSE FOR 2A + 2C**/
/**for a CU with 2 adults and 2 children, scaleb=3**0.7**/
scaleb_2A_2C=3**0.7;


rent_new_2A_2C=rent_new_equiv*scaleb_2A_2c;
zown_oop_2A_2C=zown_oop_equiv*scaleb_2A_2C;
majapp_oop_2A_2C=majapp_oop_equiv*scaleb_2A_2C;


rnteqx_own_qtr_2A_2C = rnteqx_own_qtr_equiv*scaleb_2A_2C;
renteq_vac_not_qtr_2A_2C = renteq_vac_not_qtr_equiv*scaleb_2A_2C;
renteq_vac_rent_qtr_2A_2C = renteq_vac_rent_qtr_equiv*scaleb_2A_2C;
renteq_time_sh_qtr_2A_2C = renteq_time_sh_qtr_equiv*scaleb_2A_2C;
vehicle_flow_2A_2C =vehicle_flow_equiv*scaleb_2A_2C;


   cen_off_inc_2A_2C=cen_off_inc_equiv*scaleb_2A_2C;

   spending_2A_2C= spending_equiv*scaleb_2A_2C;
   
   spen_cons_2A_2C=spen_cons_equiv*scaleb_2A_2C;
   
   consump_4_2A_2C = consump_4_equiv*scaleb_2A_2C;
   
   income_disp_2A_2C=income_disp_equiv*scaleb_2A_2C;
   


   ***********************************************demographic variables****************************************************************;
    /****Education**/
   Education_ref=0;
       educ_1=0;
       educ_2=0;
       educ_3=0;
       educ_4=0;
   Education_ref=0;
       if '00'<=educ_ref<='11' then do;
               educ_1=1;                                        /**less than high school graduate**/
   	Education_ref=1;
	end;
        if educ_ref='12' then do;               /**high school graduate**/
           educ_2=1;
           Education_ref=2;
		end;
        if educ_ref='13' then do;              /**some college no degree**/
           educ_3=1;
           Education_ref=3;
		end;
        if educ_ref>='14' then do;        /**associate degree or higher**/
             educ_4=1;
        Education_ref=4;
       end;
   
   /**race- ethnicity****/  
      Race_eth=0;
       hispanic=0; Hispanic_1=0;
       whiteNH=0;
       blackNH=0;
       other_ori=0;
       
       race_eth_1=0;
       race_eth_2=0;
       race_eth_3=0;
       race_eth_4=0;
       **race and hispanic together**;	  
   **Hispanic**;   
    if hori_ref='B' or hori_ref<1 then hispanic=0;
        else if 1<=hori_ref<=8 then hispanic=1;
    **race + Hispanic**;  
       if ref_race='1' and hispanic=0 then do;
          whiteNH=1;                                                  /**while non-Hispanic reference person**/
         race_eth=1;
         race_eth_1=1;
	end;
    if ref_race='2' and hispanic=0 then do;
        blackNH=1;                                                  /**black non-Hispanic reference person**/
        race_eth=2;
        race_eth_2=1;
	end;
    if whiteNH=0 and blackNH=0 and hispanic=1 then do;
       Hispanic_1=1;                                            /**Hispanic any race**/
       race_eth=3;
       race_eth_3=1;
	end;
    if whiteNH=0 and blackNH=0 and Hispanic_1=0 then do;
          other_ori=1;                                        /**all other race-ethnicity reference person**/
         race_eth=4;
         race_eth_4=1;
   end;
        
       ***Family type making Meyer and Sullivan****************************; 
   Fam_type_gp=0;
   
   Fam_type_1=0;
   Fam_type_2=0;
   Fam_type_3=0;
   Fam_type_4=0;
   Fam_type_5=0;
   
  If fam_type=7 then do;
     fam_type_gp=1;      /**single mom**/
     Fam_type_1=1;
    end;
    if 2<=fam_type<=5 then do;
     fam_type_gp=2;     /**married couple CUs with other members**/
     Fam_type_2=1;
     end;
    if fam_type=8 then do;
     fam_type_gp=3;            /**single CUs**/
     Fam_type_3=1;
     end;

    if fam_type=1 then do;
     fam_type_gp=4;   /**Married couple no children and no other people in cU**/
     Fam_type_4=1;
	end;

    if fam_type=6 or fam_type=9 then do;
     fam_type_gp=5;   /**other cus including male single parents**/
     Fam_type_5=1;
    end;
  
   ************end of family type group variable***************************;
   ***age of reference person***;
   age_ref_65=0;
   age_ref_65_1=0;
   age_ref_65_2=0;
   if age_ref>64 then age_ref_65=1;

   If age_ref<=64 then age_ref_65_1=1;
   else if age_ref>64 then age_ref_65_2=1; 
      
   **end of age of reference person**;
   ***gender of reference person***;
   Gender=0;  
   Gender_1=0;
   Gender_2=0;
   Gender_3=0;

    
   **********vehicles*****************;
   own_vehicle=0;    
   own_auto=0;
   own_car=0;  
   own_trk=0;
   lease_vehicle=0;     
     
     
    /**below from Caleb 12_15_21. sent in an email**/
   if N_veh>0 then own_vehicle=1;              /**total number of all types of vehicles owned by CU**/
   if N_auto>0 then own_auto=1;                 /**total number of automobiles (cars and trucks) owned by CU**/
   if N_car>0 then own_car=1;                   /**total number of cars owned by CU**/
   if N_trk>0 then own_trk=1;                   /**total number of trucks owned by CU**/
   
   *********end of vehicles***********;
   
  
   ***** number of people in 3 age groups***;
   *** when use this variable, will weight by finlwt21 by number in each group****; 
   age_gp_1=0;
   age_gp_2=0;
   age_gp_3=0;
   
   If PERSLT18 >0 then grp_0_17= PERSLT18 ;
   
   if PERSOT64>0 then grp_65_plus= PERSOT64;
       
   grp_18_64=fam_size- sum(0,PERSLT18)- sum(0,PERSOT64);
   ****;


   if grp_0_17>0 then age_gp_1=1;
     else if grp_18_64>0 then age_gp_2=1;
     else if grp_65_plus>0 then age_gp_3=1;
 
     age_0_17_gp_1=0;
	 age_0_17_gp_2=0;

     if grp_0_17>0 then do;
         age_0_17_gp_1=1;
	 end;
     if grp_0_17<=0 then do;
        age_0_17_gp_2=1;
	 end;
      
    age_18_64_gp_1=0;
    age_18_64_gp_2=0; 

    if grp_18_64>0 then  do;
        age_18_64_gp_1=1;
     end;
     if grp_18_64<=0 then do;
        age_18_64_gp_2=1;
	 end;

     age_65_pl_gp_1=0;
     age_65_pl_gp_2=0;
     
     if grp_65_plus>0 then do;
         age_65_pl_gp_1=1;
	 end;
     if grp_65_plus<=0 then do;
        age_65_pl_gp_2=1;
	 end;
 
     
   
   *************************end of number of CU members in age groups***********;
   
   ***********************earner status of members***********************;   
   Earn_gp=0;
   earn_gp_1=0;
   earn_gp_2=0;
   earn_gp_3=0;
   earn_gp_4=0;
   earn_gp_5=0;
   
   If earncomp=1 then do;
      earn_gp=1;     /**reference person only earner**/
      earn_gp_1=1;
     end; 
      if  earncomp=2 then do;
      earn_gp=2;       /**reference and spouse only earner**/
      earn_gp_2=1;
  end;
      if earncomp=5 then do;           /**only spouse works**/
      earn_gp=3;
      earn_gp_3=1;
     end; 
      if 3<=earncomp<=4 or 6<=earncomp<=7  then do;  /**any earners**/
      earn_gp=4;
      earn_gp_4=1;
    end;
      if earncomp=8 then do;          /**no earners**/
      earn_gp=5;      
      earn_gp_5=1;
   end;
   ***********************end of earner status of members*****************;   

 ***********to find out who has owner and renter expenses*******;
 
   rnteq_positive=0;
   
   rent_new_positive=0;

  
   if rnteqx_own_qtr_equiv>0 then rnteq_positive=1;
  
   if rent_new_equiv>0 then rent_new_positive=1;


   *****************end of code for demographic variables*************************;
	HOLD = YR||QTR;
	NEW = INPUT(HOLD,12.);
	DROP YRQTR HOLD;
	RENAME NEW = YRQTR;  
run; 


 ***********************************************************************************************************************;
** DISTRIBUTION BASED ON different defintions of equivalized spending, consumption, income scaleb equivalizerd       **;
***********************************************************************************************************************; 
**create a year variable based on 4 quarters, for example year 2014 is based on quarterly data from 2014Q2-2015Q1**;

data restricted;
  set All_qtrs;
  if 20142<=yrqtr<=20211;
  
  if        20142<=yrqtr<=20151 then year=2014;
    else if 20152<=yrqtr<=20161 then year=2015;
    else if 20162<=yrqtr<=20171 then year=2016;
    else if 20172<=yrqtr<=20181 then year=2017;
    else if 20182<=yrqtr<=20191 then year=2018;
    else if 20192<=yrqtr<=20201 then year=2019;
    else if 20202<=yrqtr<=20211 then year=2020;
run;

PROC MEANS DATA=restricted;
	OUTPUT OUT = restricted_MEANS;
RUN;

PROC TRANSPOSE DATA=restricted_MEANS OUT=restricted_T_MEANS;
	ID _STAT_;
RUN; 


data new_restricted;
  set restricted;
  own_gp=0;
  if 1<=cutenure<=2 then own_gp=1;
  if cutenure>2 then     own_gp=2;
  run;

proc sort data=new_restricted;
  by own_gp;
  run;

proc means data=new_restricted;
  var ZOWNDWLL  zown_oop_equiv majapp_oop_equiv
  rnteqx_own_qtr_equiv rnteqx_own_qtr  rent_new_equiv ;
 by own_gp;
  title "by own_gp. means of own dwll rental equivalence, and renter qtr rent";
  run;

proc means data = new_restricted;
  vars census_inc_d_neg tot_exp_neg spn_cons_neg cons_4_neg;
  title "unweighted means of dummy variables regarding negative values";
  run;


proc sort data=new_restricted;
  by year;
  run;


PROC UNIVARIATE DATA=new_restricted noprint;                              
		  VAR  spending_equiv consump_4_equiv income_disp_equiv ;
		  FREQ person_wt; 
		  by year;
		  OUTPUT OUT=median_base_allcus MEAN=mean1-mean160 N=n1-n160 MEDIAN=med1-med160;   
 		RUN;	


proc transpose data=median_base_allcus  out=median_base_allcus;
run;

proc print data=median_base_allcus;
  title "full sample equivalent values  person weighted. income divided by 4, quarterly expenditures";
run;


/**for adult equivalent output**/
PROC UNIVARIATE DATA=new_restricted noprint;                              
		  VAR  spending_equiv consump_4_equiv income_disp_equiv ;
		  FREQ person_wt; 
		  by year;
		  OUTPUT OUT=base_allcus MEDIAN= sp_eqv_MD  cons_4_eqv_MD inc_dp_eqv_MD
		                         MEAN=sp_eqv_M cons_4_eqv_M inc_dp_eqv_M;      
 
		RUN;	

		
data med_M_output;
  set base_allcus;
run;
		
PROC TRANSPOSE DATA=med_M_output OUT=new_med_M;
RUN;
		 
		 
PROC PRINT DATA=new_med_M;          
		 TITLE1 "median and mean adult equivalized spending, consumption, disposable income";
		 TITLE2 "person weighted based on quarters";
RUN;  


DATA base_allcus;
		  SET base_allcus;
		  one=1.0;
RUN;

PROC SORT DATA=base_allcus;
			BY YEAR;
RUN;

PROC SORT DATA=All_qtrs;
			BY YEAR;
RUN;

DATA allcus_raw;
   			MERGE All_qtrs base_allcus ;
   			BY year;
RUN;

DATA allcus_raw2;
			set allcus_raw ;
			BY one;
RUN;


proc sort data= allcus_raw2;
by year;
run;

proc means data= allcus_raw2;
by year;
 var sp_eqv_MD   cons_4_eqv_MD inc_dp_eqv_MD;
run;

 proc means data=allcus_raw2;
  var rent_difference LIHEAP_sub_full NSLP_sub_full WIC_sub_full income_WIC_full;
   title "means of in-kind";
run;

DATA allcus; 
		SET allcus_raw2;
		
		**inpute annual chained CPI-U all items**;
		
		/**Chained CPI for All Urban Consumers, U.S. city average (C-CPI-U)Series Title:	All items in U.S. city average, all urban consumers, chained, not seasonally adjusted				
		Area:	U.S. city average				
		Item:	All items				
		Base Period:	DECEMBER 1999=100				
		Years:	2011 to 2021				
	
	  	Year	Jan	Feb	Mar	Apr	May	Jun	Jul	Aug	Sep	Oct	Nov	Dec	Annual		computed annual averages	multiplier need to adjust 2014 anchor relative to other years
		2011	126.778	127.363	128.585	129.483	129.999	129.846	129.983	130.351	130.635	130.373	130.196	129.844	129.453		129.453	
		2012	130.438	130.953	131.905	132.284	132.154	131.956	131.731	132.430	132.988	132.892	132.208	131.770	131.976		131.97575	
		2013	132.137	133.204	133.558	133.421	133.626	133.900	133.919	134.098	134.255	133.876	133.596	133.509			133.5915833	
		2014	134.017	134.542	135.375	135.771	136.216	136.433	136.392	136.127	136.211	135.891	135.107	134.207			135.5240833	1
		2015	133.487	134.053	134.969	135.222	135.977	136.391	136.371	136.125	135.837	135.735	135.393	134.788			135.3623333	0.998806485 (2015)
		2016	134.966	134.953	135.655	136.332	136.895	137.329	137.007	137.026	137.328	137.536	137.253	137.221			136.6250833	1.008124017 (2016)
		2017	138.035	138.403	138.461	138.810	138.922	138.989	138.755	139.128	139.878	139.782	139.720	139.546			139.03575	1.025911754 (2017)
		2018	140.239	140.805	141.142	141.662	142.146	142.349	142.317	142.317	142.489	142.700	142.238	141.699			141.8419167	1.046617791 (2018)
		2019	142.001	142.571	143.297	143.926	144.183	144.243	144.409	144.388	144.428	144.629	144.518	144.251			143.9036667	1.061830954 (2019)
		2020	144.813	145.193	144.913	144.142	144.018	144.847	145.747	146.270	146.417	146.382	146.242	146.408			145.4493333	1.073236061 (2020)
		2021	147.054	147.877	148.918	150.150	151.334	152.649	153.352	153.643	154.076	155.317						

                    based 2014   base 2015



2014 135.5240833	1	         1.001194941
2015 135.3623333	0.998806485	1
2016 136.6250833	1.008124017	1.009328666
2017 139.03575	    1.025911754	1.027137658
2018 141.8419167	1.046617791	1.047868437
2019 143.9036667	1.061830954	1.063099779
2020 145.4493333	1.073236061	1.074518515

		
		**/
		
/**first need to derive the poverty rate before can determine the thresholds for other measures            **/;
/** then produce the poverty rate using consumption4 using 60% of th consumption_4 median                  **/;
/**for example, if the consumption_4 poverty rate is 20%, then to hold poverty rate constrant at 20% across**/;
** all my measures, run proc univariable on each                                                           **/;
/**of other measures to find the $value at the 20th percentile in the e.g., spending distribution          **/;
/** that is the new spending based threshold to which i will compare spending                               **/;
/** the anchor 2014 consumption 4 thresholds and each new 2014 threshold will be updated using the C-CCPI   **/ ; 

/**first produce purely relative measures. these are produced implicity each year since medians are for each year**/
		

      

            relpov_sp=0.6*sp_eqv_MD; 
     		relpov_cons_4=0.6* cons_4_eqv_MD;                  
			relpov_inc_di=0.6* inc_dp_eqv_MD;

	
			rel_poor_spen=0;
            if spending_equiv<relpov_sp        then rel_poor_spen=1;
			
			rel_poor_cons_4=0;
			if consump_4_equiv<relpov_cons_4   then rel_poor_cons_4=1;  
			     
			rel_poor_inco_d=0;
			if income_disp_equiv<relpov_inc_di then rel_poor_inco_d=1; 
	 	
		************end purely relative for each year**;

**below if use official poverty thresholds included on CE data file***;
 	   

if 20142 <= yrqtr <= 20151 then do; 

		ccpi_adj=1.001194941;

spending_2A_2C_real   =spending_2A_2C    *(1/ccpi_adj); 
con_4_2A_2C_real      = consump_4_2A_2C  *(1/ccpi_adj);
inc_disp_2A_2C_real   =income_disp_2A_2C  *(1/ccpi_adj);


end;
		
if 20152 <= yrqtr <= 20161 then do;
		     ccpi_adj=1.00;

spending_2A_2C_real   =spending_2A_2C    *(1/ccpi_adj); 
con_4_2A_2C_real      = consump_4_2A_2C  *(1/ccpi_adj);
inc_disp_2A_2C_real   =income_disp_2A_2C  *(1/ccpi_adj);


end;	     

if 20162 <= yrqtr <= 20171 then do;
		     ccpi_adj=1.009328666;

spending_2A_2C_real   =spending_2A_2C    *(1/ccpi_adj); 
con_4_2A_2C_real      = consump_4_2A_2C  *(1/ccpi_adj);
inc_disp_2A_2C_real   =income_disp_2A_2C  *(1/ccpi_adj);

end;
 
if 20172 <= yrqtr <= 20181 then do;
		     ccpi_adj=1.027137658;

spending_2A_2C_real   =spending_2A_2C    *(1/ccpi_adj); 
con_4_2A_2C_real      = consump_4_2A_2C  *(1/ccpi_adj);
inc_disp_2A_2C_real   =income_disp_2A_2C  *(1/ccpi_adj);

end;
				     
if 20182 <= yrqtr <= 20191 then do;	
		     ccpi_adj=	1.047868437;

spending_2A_2C_real   =spending_2A_2C    *(1/ccpi_adj); 
con_4_2A_2C_real      = consump_4_2A_2C  *(1/ccpi_adj);
inc_disp_2A_2C_real   =income_disp_2A_2C  *(1/ccpi_adj);


end;
 
if 20192 <= yrqtr <= 20201 then do;	
		     ccpi_adj=1.063099779;

spending_2A_2C_real   =spending_2A_2C    *(1/ccpi_adj); 
con_4_2A_2C_real      = consump_4_2A_2C  *(1/ccpi_adj);
inc_disp_2A_2C_real   =income_disp_2A_2C  *(1/ccpi_adj);


end;

if 20202 <= yrqtr <= 20211 then do;	
		     ccpi_adj=1.074518515;
	
spending_2A_2C_real   =spending_2A_2C    *(1/ccpi_adj); 
con_4_2A_2C_real      = consump_4_2A_2C  *(1/ccpi_adj);
inc_disp_2A_2C_real   =income_disp_2A_2C  *(1/ccpi_adj);


end;
           
RUN;

proc sort data=allcus;
  by year;
run;

 proc means data=allcus;
  var rent_difference LIHEAP_sub_full NSLP_sub_full WIC_sub_full income_WIC_full;
  title "all cus means of in-kind";
run;

proc means data= allcus;
by year;

 var sp_eqv_MD cons_4_eqv_MD inc_dp_eqv_MD
     sp_eqv_M  cons_4_eqv_M inc_dp_eqv_M

yrqtr 

ccpi_adj

spending_2A_2C    
consump_4_2A_2C   
income_disp_2A_2C  

spending_2A_2C_real  
con_4_2A_2C_real 
inc_disp_2A_2C_real

;      

 run;

proc sort data=allcus;
 by year;
 run;


proc means data=allcus;
  by year;
  var spending_2A_2C_real con_4_2A_2C_real inc_disp_2A_2C_real;
title "unweighted means for each measure by year";
run;

*************************************beginning of medians in 2015 dollars and equivalized to 2A+2C ******************************;
********************************************Real derived by applying chained CPI-U all items*************************************;
************annualize values for AEA P&P*******************************;
data annual_allcus;
  set allcus;
inc_disp_2A_2C_real_an=inc_disp_2A_2C_real*4;
spending_2A_2C_real_an= spending_2A_2C_real*4;
con_4_2A_2C_real_an= con_4_2A_2C_real*4;
run;

proc sort data=annual_allcus;
  by year;
  run;

*************************for Figure 1 in AEA P&P***********************;
proc univariate data=annual_allcus noprint;
  BY year;
   var inc_disp_2A_2C_real_an spending_2A_2C_real_an con_4_2A_2C_real_an ;

FREQ person_wt;
  output out=med_3Meas_2015_allyr MEDIAN=med1-med3;   
run;

proc transpose data=med_3Meas_2015_allyr  out=folder.med_3Meas_2015_allyr;
run;

proc print data=folder.med_3Meas_2015_allyr;
  title "all years with annualized medians in 2015 dollars for 3 measures, person weighted";
run;

proc export 
  data=folder.med_3Meas_2015_allyr 
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/med_3Meas_2015_allyr.xlsx" 
  replace;
run;
***************************end of Figure 1 output************************;

**************************end of medians in 2015 dollars and equivalized to 2A+2C ***********************;
proc sort data=allcus;
  by year;
run;

data thresholds; set allcus;
  keep  year
        relpov_sp relpov_cons_4  relpov_inc_di
  ;
  run; 

PROC SORT DATA=thresholds NODUPKEY;
	BY _all_;
RUN;

proc transpose data=thresholds out=folder.t_thresholds;
run;
  

proc print data=folder.t_thresholds;
  title "relative adult equivalent relative poverty lines by year using quarterly values for each spending, consumption, or income variables";
run;

proc export 
  data=folder.t_thresholds 
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/t_thresholds.xlsx" 
  replace;
run;
******************************************************;

proc sort data=allcus;
   by year;
run;

/** poverty rates based on purely relative measures**/
*************************for Figure 2 in AEA P&P ***********************;
Proc means data=allcus ;
  var rel_poor_spen  rel_poor_cons_4    rel_poor_inco_d;
  by year;
weight person_wt;
  title "person weighted poverty rates based on relative thresholds 3 measures tot_exp, consumption_4, after tax income";
  output out = folder.new_3Meas_Povrate_pure_rel N= mean= / autoname;
run;

proc export 
  data=folder.new_3Meas_Povrate_pure_rel
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/new_3Meas_Povrate_pure_rel.xlsx" 
  replace;
run;
***************************end of Figure 2 output************************;
/** end of purely relative poverty rates output**/


*************Before creating Thresholds that have the same ANCHORED poverty rate in 2015 as the 2015 relative consumption_4 poverty rate**************************;
** need to hardcode the 2015 consumption_4 poverty rate and then run prco univariate to determine percentile point in each distribution *******;
**  that results in the same poverty rate for all in 2015. The 2015 consumption_4 poverty rate is 16.77021*;

proc univariate data=restricted noprint;
   VAR spending_equiv consump_4_equiv income_disp_equiv;

  WHERE year = 2015;
   FREQ person_wt;
   output out=folder.thresh pctlpts= 16.77021 /**the full value includes the additional follow digits.. 25648261 **/  

		     pctlpre= sp_eqv_ cons_4_eqv_ inc_dp_eqv_
		     pctlname=p15_3;
run;

proc export 
  data=folder.thresh 
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/thresh.xlsx" 
  replace;
run;
********************************************************************************;

data allcus_anchor;
  set allcus;

*******The following Thresholds are based on the output from the proc univariate with the percentile for 2015 consumption_4 poverty*****;
** equally the number hardcoded in the proc univariate above. The following can only be derived and run AFTER the relative threshold  **;
** for 2015 consumption_4 is produced **;


sp_eqv_p15_3		=3290;
cons_4_eqv_p15_3	=3369.73901538283;   /**the value for 2015 consumption_4 relative threshold is from output with the relative thresholds produced earlier**/
inc_dp_eqv_p15_3	=3390.55;



/**for general then show by year in output**/

anch_spen_pov    =0;
anch_cons_4_pov  =0;
anch_dp_inc_pov  =0;

if 20142 <= yrqtr <= 20151 then do ; 


cons_4_eqv_p14_3_r=cons_4_eqv_p15_3* ccpi_adj;

/**poverty rates based on anchoring all poverty rates to 2015 consumption_4 relative rate**/

if spending_equiv<sp_eqv_p15_3 * ccpi_adj    then anch_spen_pov=1;
if consump_4_equiv<cons_4_eqv_p15_3* ccpi_adj   then anch_cons_4_pov=1;       
if income_disp_equiv<inc_dp_eqv_p15_3* ccpi_adj then anch_dp_inc_pov=1; 

***********end of anchored rates**************;

end;

if 20152 <= yrqtr <= 20161 then do;

ccpi_adj=1.0;

sp_eqv_p15_3_r=sp_eqv_p15_3* ccpi_adj;
cons_4_eqv_p15_3_r=cons_4_eqv_p15_3* ccpi_adj;
inc_dp_eqv_p15_3_r=inc_dp_eqv_p15_3* ccpi_adj;


****************rates****************;

if spending_equiv<sp_eqv_p15_3 * ccpi_adj    then anch_spen_pov=1;
if consump_4_equiv<cons_4_eqv_p15_3* ccpi_adj   then anch_cons_4_pov=1;       
if income_disp_equiv<inc_dp_eqv_p15_3* ccpi_adj then anch_dp_inc_pov=1; 

***********end of rates**************;


gap_spnd_2015=(max(0,sp_eqv_p15_3_r-(MAX(0,spending_equiv))));
gap_consp_4_2015=(max(0,cons_4_eqv_p15_3_r-(MAX(0,consump_4_equiv))));
gap_disp_inc_2015=(max(0,inc_dp_eqv_p15_3_r-(MAX(0,income_disp_equiv))));
    

end;		   
		     
if 20162 <= yrqtr <= 20171 then  do; 

cons_4_eqv_p16_3_r=cons_4_eqv_p15_3* ccpi_adj;
 

****************rates****************;
if spending_equiv<sp_eqv_p15_3 * ccpi_adj    then anch_spen_pov=1;
if consump_4_equiv<cons_4_eqv_p15_3* ccpi_adj   then anch_cons_4_pov=1;       
if income_disp_equiv<inc_dp_eqv_p15_3* ccpi_adj then anch_dp_inc_pov=1; 

***********end of rates**************; 

end;
     
				     
if 20172 <= yrqtr <= 20181 then  do;
 
cons_4_eqv_p17_3_r=cons_4_eqv_p15_3* ccpi_adj;

****************rates****************;
 
if spending_equiv<sp_eqv_p15_3 * ccpi_adj    then anch_spen_pov=1;
if consump_4_equiv<cons_4_eqv_p15_3* ccpi_adj   then anch_cons_4_pov=1;       
if income_disp_equiv<inc_dp_eqv_p15_3* ccpi_adj then anch_dp_inc_pov=1; 

***********end of rates**************; 

end;
           
 if 20182 <= yrqtr <= 20191 then  do;

cons_4_eqv_p18_3_r=cons_4_eqv_p15_3* ccpi_adj;
 
****************rates****************;
 
if spending_equiv<sp_eqv_p15_3 * ccpi_adj    then anch_spen_pov=1;
if consump_4_equiv<cons_4_eqv_p15_3* ccpi_adj   then anch_cons_4_pov=1;       
if income_disp_equiv<inc_dp_eqv_p15_3* ccpi_adj then anch_dp_inc_pov=1; 

***********end of rates**************;

end;

if 20192 <= yrqtr <= 20201 then do;
   
cons_4_eqv_p19_3_r=cons_4_eqv_p15_3* ccpi_adj;

****************rates****************;
  
if spending_equiv<sp_eqv_p15_3 * ccpi_adj    then anch_spen_pov=1;
if consump_4_equiv<cons_4_eqv_p15_3* ccpi_adj   then anch_cons_4_pov=1;       
if income_disp_equiv<inc_dp_eqv_p15_3* ccpi_adj then anch_dp_inc_pov=1; 

***********end of rates**************;

end;

 if 20202 <= yrqtr <= 20211 then  do;
  

cons_4_eqv_p20_3_r=cons_4_eqv_p15_3* ccpi_adj;

sp_eqv_p20_3_r=sp_eqv_p15_3* ccpi_adj;
cons_4_eqv_p20_3_r=cons_4_eqv_p15_3* ccpi_adj;
inc_dp_eqv_p20_3_r=inc_dp_eqv_p15_3* ccpi_adj;


gap_spnd_2020=(max(0,sp_eqv_p20_3_r-(MAX(0,spending_equiv))));
gap_consp_4_2020=(max(0,cons_4_eqv_p20_3_r-(MAX(0,consump_4_equiv))));
gap_disp_inc_2020=(max(0,inc_dp_eqv_p20_3_r-(MAX(0,income_disp_equiv))));

****************rates****************;

if spending_equiv<sp_eqv_p15_3 * ccpi_adj    then anch_spen_pov=1;
if consump_4_equiv<cons_4_eqv_p15_3* ccpi_adj   then anch_cons_4_pov=1;       
if income_disp_equiv<inc_dp_eqv_p15_3* ccpi_adj then anch_dp_inc_pov=1; 

***********end of rates**************;

end;
 
run;



/** poverty rates based on anchor or rates to Consumption_4 in 2015 relative rate**/

proc sort data=allcus_anchor;
  by year;
run;


*************************for Figure 3 in AEA P&P***********************;

Proc means data=allcus_anchor mean;
  var anch_spen_pov anch_cons_4_pov anch_dp_inc_pov;
  by year;
 weight person_wt;
  title "person weighted poverty rates based on anchor poverty rate 3 measures tot_exp, consumption_4, after tax income";
  output out =  folder.new_3Meas_Anchorrate_pov mean= / autoname;
run;


proc export 
  data=folder.new_3Meas_Anchorrate_pov
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/new_3Meas_Anchorrate_pov.xlsx" 
  replace;
run;
***************************end of Figure 3 output************************;

*******************additional anaysis for transition matrix**************;
DATA both_dpinc_cons_poor;
	SET allcus_anchor;

    if year=2014 then do;
         dpinc_cons_poor_2014=0;
		 dpinc_poor_not_cons_2014=0;
		 cons_poor_not_dpinc_2014=0;
     end;

	  if year=2015 then do;
         dpinc_cons_poor_2015=0;
		 dpinc_poor_not_cons_2015=0;
		 cons_poor_not_dpinc_2015=0;
     end;

	if year=2020 then do; 
        dpinc_cons_poor_2020=0;
        dpinc_poor_not_cons_2020=0;
		cons_poor_not_dpinc_2020=0;
	End;
	
*********************************************;

	if year=2014 and anch_cons_4_pov=1 and anch_dp_inc_pov =1 then do;
        dpinc_cons_poor_2014=1;
	end;

    if year=2014 and anch_cons_4_pov =0 and anch_dp_inc_pov =1 then do;
        dpinc_poor_not_cons_2014=1;
	end;

    if year=2014 and anch_cons_4_pov =1 and anch_dp_inc_pov =0 then do;
        cons_poor_not_dpinc_2014=1;
	end;
*********************************************;
	if year=2015 and anch_cons_4_pov =1 and anch_dp_inc_pov =1 then do;
        dpinc_cons_poor_2015=1;
	end;

    if year=2015 and anch_cons_4_pov =0 and anch_dp_inc_pov =1 then do;
        dpinc_poor_not_cons_2015=1;
	end;

    if year=2015 and anch_cons_4_pov =1 and anch_dp_inc_pov =0 then do;
        cons_poor_not_dpinc_2015=1;
	end;

**************************************************;

	if year=2020 and anch_cons_4_pov =1 and anch_dp_inc_pov =1 then do;
        dpinc_cons_poor_2020=1;
	end;

    if year=2020 and anch_cons_4_pov =0 and anch_dp_inc_pov =1 then do;
        dpinc_poor_not_cons_2020=1;
	end;

    if year=2020 and anch_cons_4_pov =1 and anch_dp_inc_pov =0 then do;
        cons_poor_not_dpinc_2020=1;
	end;



RUN;

data allcus_anchor_2;
  set both_dpinc_cons_poor;
run;
********************************results for 2020**************************************;

*********AEA P&P Table Demographics for 2015 4 Subsamples: full, below consumption threshold, below income threshold, below both*******;

******************************Demographics 2015 full sample***********************************;
proc means data=allcus_anchor_2 noprint;
    WHERE year=2015 ; 
 var race_eth_1-race_eth_4 educ_1-educ_4 fam_type_1-fam_type_5 
     age_0_17_gp_1-age_0_17_gp_2 age_18_64_gp_1-age_18_64_gp_2 age_65_pl_gp_1-age_65_pl_gp_2
     earn_gp_1-earn_gp_5 ;
  weight person_wt;
  output out=full_AEA_demo_2015 N= MEAN= /autoname ;     
run;

proc transpose data=full_AEA_demo_2015  out=folder.full_AEA_demo_2015;
run;


proc export 
  data=folder.full_AEA_demo_2015
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/full_AEA_demo_2015.xlsx" 
  replace;
run; 


********************Demographics 2015 below consumption_4 absolute threshold based on anchored poverty rate*****************************;
proc means data=allcus_anchor_2 noprint;
    WHERE year=2015 and anch_cons_4_pov=1; 
 var race_eth_1-race_eth_4 educ_1-educ_4 fam_type_1-fam_type_5 
     age_0_17_gp_1-age_0_17_gp_2 age_18_64_gp_1-age_18_64_gp_2 age_65_pl_gp_1-age_65_pl_gp_2
     earn_gp_1-earn_gp_5 ;
  weight person_wt;
  output out=demo_AEA_conspoor_2015 N=  MEAN= /autoname ;      
run;

proc transpose data=demo_AEA_conspoor_2015  out=folder.demo_AEA_conspoor_2015;
run;


proc export 
  data=folder.demo_AEA_conspoor_2015
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/demo_AEA_conspoor_2015.xlsx" 
  replace;
run; 

***********Demographics 2015 below after tax and transfer absolute threshold based on anchored consumption_4 poverty rate******************;
proc means data=allcus_anchor_2 noprint;
    WHERE year=2015 and anch_dp_inc_pov=1; 
 var race_eth_1-race_eth_4 educ_1-educ_4 fam_type_1-fam_type_5 
     age_0_17_gp_1-age_0_17_gp_2 age_18_64_gp_1-age_18_64_gp_2 age_65_pl_gp_1-age_65_pl_gp_2
     earn_gp_1-earn_gp_5 ;
  weight person_wt;
  output out=demo_AEA_incpoor_2015 N=  MEAN= /autoname ;  
run;

proc transpose data=demo_AEA_incpoor_2015  out=folder.demo_AEA_incpoor_2015;
run;

proc export 
  data=folder.demo_AEA_incpoor_2015
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/demo_AEA_incpoor_2015.xlsx" 
  replace;
run; 

***********Demographics 2015 below both consumption_r & after tax and transfer absolute thresholds******************;
proc means data=allcus_anchor_2 noprint;
  WHERE year=2015 and dpinc_cons_poor_2015=1; 
 var race_eth_1-race_eth_4 educ_1-educ_4 fam_type_1-fam_type_5 
     age_0_17_gp_1-age_0_17_gp_2 age_18_64_gp_1-age_18_64_gp_2 age_65_pl_gp_1-age_65_pl_gp_2
     earn_gp_1-earn_gp_5 ;
  weight person_wt;
  output out=demo_AEA_dpconpoor_2015 N=  MEAN= /autoname;        
run;

proc transpose data=demo_AEA_dpconpoor_2015  out=folder.demo_AEA_dpconpoor_2015;
run;

proc export 
  data=folder.demo_AEA_dpconpoor_2015
  dbms=xlsx 
  outfile="Consumption_Measure/ExcelAEAPP_Short/demo_AEA_dpconpoor_2015.xlsx" 
  replace;
run; 