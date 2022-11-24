******************
***1.CCUS-RE******
******************
*Sample statistics, use ccus_re.dta*
keep if dis <= 6500
sum house_price treat post house_area age com_area com_builarea com_green ntl scsd dicp gdp_3 pow

*Main results, use ccus_re.dta*
keep if dis <= 6500
reg lhprice D treat post lharea age lcarea lcbarea com_green  /// 
            i.house_yearq i.ccus_id , cluster(county_id)
		
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id , cluster(county_id)

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id , cluster(county_id)
			
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id, cluster(county_id)

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id, cluster(county_id)

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(county_id)

*Buffer, use ccus_re.dta*
keep if dis <= 6500
reg lhprice lharea lcarea lcbarea com_ratio gdp_1 gdp_3 dicp pow age i.house_year i.ccus_id if post==0 , r
predict uhat_pre, residual 

reg lhprice lharea lcarea lcbarea com_ratio gdp_1 gdp_3 dicp pow age i.house_year i.ccus_id if post==1 , r
predict uhat_post, residual 

twoway (lpolyci uhat_pre dis if post==0, lpattern(solid) fcolor(none)) ///
  (lpolyci uhat_post dis if post==1, lpattern(dash) fcolor(none)), ///
  xtitle("Distance to the nearest CCUS project") xlabel(0(1000)6500) ///
  ytitle("Housing price residuals ($)") ///
  legend(order(2 "Before CCUS" 4 "After CCUS" ))  

*Common trend, use ccus_re.dta*
keep if dis <= 6500
gen t = house_year - ccus_year
tab t

forvalues i=10(-1)1{
   gen pre`i' = (t==-`i' & treat==1)
}
gen current=(t== 0 & treat==1)
forvalues i=1(1)11{
  gen post`i'=(t==`i' & treat==1)
}

reg lhprice pre10 pre9 pre8 pre7 pre6 pre5 pre4 pre3 pre2 pre1 current post2 post3 post4 post5 post6 post7 post8 post9 post10 post11 /// 
             lcbarea lcarea com_green com_ratio ltax dicp i.ccus_id i.house_yearq 
			 
coefplot, baselevels ///
keep(pre6 pre5 pre4 pre3 pre2 pre1 current post2 post3 post4 post5 post6) ///
vertical ///
coeflabels(pre6=-6 pre5=-5 pre4=-4 pre3=-3 pre2=-2 pre1=-1 ///
current=0 post1=1 post2=2 post3=3 post4=4 post5=5 post6=6) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(7,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-0.5(0.1)0.5,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
xtitle("Year relative to CCUS") ///
ytitle("Changes in housing price") ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) ///
legend(order(2 "Coef. Estimate" 1 "95% Confidence Interval")) 

*Matching (PSM-DID&CEM-DID), use ccus_re.dta*
keep if dis <= 6500
*PSM-DID* 
global psm "lcbarea com_green com_ratio "
set seed 2020
gen norvar_1 = rnormal()
sort norvar_1
psmatch2 treat $psm i.county_id , outcome(lhprice) logit neighbor(2) ties common ate caliper(0.05)
pstest lcbarea com_green com_ratio, both graph 
psgraph
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if _support ==1 , r
*CEM-DID*
cem ntl ndvi dicp lharea, tr(treat)
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id [iweight = cem_weight] , r						
			
*Range (6200m&6800m), use ccus_re.dta*
*6200m*
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if dis <= 6200, r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if dis <= 6200 , r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dis <= 6200 , r
*6800m*
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if dis <= 6800, r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if dis <= 6800 , r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dis <= 6800 , r

*Falsitest, use ccus_re.dta*
keep if dis <= 6500
gen treat_f =0
replace treat_f =1 if dis > 3600 & dis <= 4200
gen D_f  = treat_f  * post 
reg lhprice D_f treat_f post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id  , r

reg lhprice D_f treat_f post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id  , r

reg lhprice D_f treat_f post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id  , r

*Heterogeneity: nighttime lights, use ccus_re.dta*
keep if dis <= 6500
gen ntl2 =0
replace ntl2 =1 if ntl < 20
replace ntl2 =2 if ntl >=20 & ntl < 40
replace ntl2 =3 if ntl >=40 & ntl < 60
replace ntl2 =4 if ntl >=60 & ntl < 80
replace ntl2 =5 if ntl >=80 

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow    /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==1 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow   /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==2 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow    /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==3 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow   /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==4 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow  /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==5 , r	

*Heterogeneity: dicp, use ccus_re.dta*
keep if dis <= 6500
xtile dicp1 = dicp, nq(8)
reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==1 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==2 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==3 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==4 , r	

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==5 , r		

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==6 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==7 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==8 , r

*Heterogeneity: capacity, use ccus_re.dta*
*Small_pow means the capacity of CCUS is smaller than 50000 tons/year, including 50000 tons/year*
*Big_pow means the capacity of CCUS is bigger than 50000 tons/year*
keep if dis <= 6500
reg lhprice D treat post lharea age lcarea lcbarea com_green scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if small_pow==1

reg lhprice D treat post lharea age lcarea lcbarea com_green scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if big_pow==1

*Heterogeneity: carbon capture mode, use ccus_re.dta*
*Pre_com means the carbon capture mode of ccus is pre_combustion*
*Post_com means the carbon capture mode of ccus is post_combustion*
*Oxy means the carbon capture mode of ccus is oxy-fuel combustion*
keep if dis <= 6500
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if pre_com==1 , r		

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if post_com==1 , r		

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if oxy==1 , r	

*Heterogeneity: type of ccus project, use ccus_re.dta*
*cc means the ccus only capture carbon*
*ccu means the ccus capture and utilize carbon*
keep if dis <= 6500
reg lhprice D treat post lharea lcarea lcbarea com_green scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if cc ==1 , r
			
reg lhprice D treat post lharea lcarea lcbarea com_green scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if cc ==1 , r
			
reg lhprice D treat post lharea lcarea lcbarea com_green scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if cc ==1 , r
			
reg lhprice D treat post lharea lcarea lcbarea com_green scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if ccu ==1 , r
			
reg lhprice D treat post lharea lcarea lcbarea com_green scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if ccu ==1 , r
			
reg lhprice D treat post lharea lcarea lcbarea com_green scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if ccu ==1 , r		
			
*Announcement effecttime (1&2&3&4), use ccus_re.dta*
keep if dis <= 6500
*1*
gen ccus_yearq_f = ccus_year *10 + ccus_q -1 
replace ccus_yearq_f = ccus_year *9 + ccus_q +9 if ccus_q ==1
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(com_id)
*2*
gen ccus_yearq_f = ccus_year *10 + ccus_q -2 
replace ccus_yearq_f = ccus_year *9 + ccus_q +8 if ccus_q ==1
replace ccus_yearq_f = ccus_year *9 + ccus_q +8 if ccus_q ==2
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(com_id)			
*3*
gen ccus_yearq_f = ccus_year *10 + ccus_q -3 
replace ccus_yearq_f = ccus_year *9 + ccus_q +7 if ccus_q ==1
replace ccus_yearq_f = ccus_year *9 + ccus_q +7 if ccus_q ==2
replace ccus_yearq_f = ccus_year *9 + ccus_q +7 if ccus_q ==3
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(com_id)
*4*
gen ccus_yearq_f = ccus_year *10 + ccus_q -10
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(com_id)			
			
			
******************
***2.CCUS-NB******
******************
*Sample statistics, use ccus_nb.dta*
keep if dis <= 6500
sum house_price treat post house_area age com_area com_builarea com_green ntl scsd dicp gdp_3 pow

*Main results, use ccus_nb.dta*
keep if dis <= 6500
reg lhprice D treat post lharea age lcarea lcbarea com_green  /// 
            i.house_yearq i.ccus_id , cluster(county_id)
		
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id , cluster(county_id)

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id , cluster(county_id)
			
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id, cluster(county_id)

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id, cluster(county_id)

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(county_id)

*Common trend, use ccus_nb.dta*
keep if dis <= 6500	
gen t = house_year - ccus_year
tab t

forvalues i=8(-1)1{
   gen pre`i' = (t==-`i' & treat==1)
}
gen current=(t== 0 & treat==1)
forvalues i=1(1)7{
  gen post`i'=(t==`i' & treat==1)
}
 

reg lhprice pre4 pre3 pre2 pre1 current post2 post3 post4 post5 post6 post7 /// 
            lharea lcbarea lcarea ntl ndvi com_green age ltax dicp  i.ccus_id#i.house_yearq

coefplot, baselevels ///
keep(pre* current post*) ///
vertical ///
coeflabels(pre4=-4 pre3=-3 pre2=-2 pre1=-1 ///
current=0 post1=1 post2=2 post3=3 post4=4 post5=5 post6=6 post7=7) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(5,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-0.5(0.1)0.5,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
xtitle("Year relative to CCUS") ///
ytitle("Changes in housing price") ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) /// 
legend(order(2 "Coef. Estimate" 1 "95% Confidence Interval")) 			
			
*Matching (PSM-DID&CEM-DID), use ccus_nb.dta*
keep if dis <= 6500
*PSM-DID* 
global psm "lcbarea com_green com_ratio "
set seed 2020
gen norvar_1 = rnormal()
sort norvar_1
psmatch2 treat $psm i.county_id , outcome(lhprice) logit neighbor(2) ties common ate caliper(0.05)
pstest lcbarea com_green com_ratio, both graph 
psgraph
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if _support ==1 , r
*CEM-DID*
cem ntl ndvi dicp lharea, tr(treat)
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id [iweight = cem_weight] , r				
	
*Range (6200m&6800m), use ccus_nb.dta*
*6200*
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if dis <= 6200, r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if dis <= 6200 , r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dis <= 6200 , r
*6800*
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if dis <= 6800, r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if dis <= 6800 , r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dis <= 6800 , r

*Falsitest, use ccus_nb.dta*
keep if dis <= 6500
gen treat_f =0
replace treat_f =1 if dis > 3600 & dis <= 4200
gen D_f  = treat_f  * post 
reg lhprice D_f treat_f post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id  , r

reg lhprice D_f treat_f post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id  , r

reg lhprice D_f treat_f post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id  , r

*Heterogeneity: nighttime lights, use ccus_nb.dta*
keep if dis <= 6500
gen ntl2 =0
replace ntl2 =1 if ntl < 20
replace ntl2 =2 if ntl >=20 & ntl < 40
replace ntl2 =3 if ntl >=40 & ntl < 60
replace ntl2 =4 if ntl >=60 & ntl < 80
replace ntl2 =5 if ntl >=80 

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow    /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==1 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow   /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==2 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow    /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==3 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow   /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==4 , r		

reg lhprice D treat post lharea age lcbarea com_green scsd dicp  gdp_3 pow  /// 
            i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id if ntl2==5 , r	

*Heterogeneity: dicp, use ccus_nb.dta*
keep if dis <= 6500
xtile dicp1 = dicp, nq(8)
reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==1 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==2 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==3 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==4 , r	

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==5 , r		

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==6 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==7 , r

reg lhprice D treat post lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if dicp1==8 , r

*Heterogeneity: capacity, use ccus_nb.dta*
*Small_pow means the capacity of CCUS is smaller than 50000 tons/year, including 50000 tons/year*
*Big_pow means the capacity of CCUS is bigger than 50000 tons/year*
keep if dis <= 6500
reg lhprice D treat post lharea age lcarea lcbarea com_green scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if small_pow==1

reg lhprice D treat post lharea age lcarea lcbarea com_green scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if big_pow==1

*Heterogeneity: carbon capture mode, use ccus_nb.dta*
*Pre_com means the carbon capture mode of ccus is pre_combustion*
*Post_com means the carbon capture mode of ccus is post_combustion*
*Oxy means the carbon capture mode of ccus is oxy-fuel combustion*
keep if dis <= 6500
reg lhprice D treat post lharea age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if pre_com==1 , r		

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow  /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if post_com==1 , r			

*Heterogeneity: type of ccus project, use ccus_nb.dta*
*cc means the ccus only capture carbon*
*ccu means the ccus capture and utilize carbon*
keep if dis <= 6500			
reg lhprice D treat post age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if cc ==1 , r

reg lhprice D treat post age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow    /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if cc ==1 , r

reg lhprice D treat post age lcarea lcbarea com_green ntl ndvi scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if cc ==1 , r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow     /// 
            i.house_yearq i.ccus_id i.house_yearq#i.ccus_id if ccu ==1 , r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow     /// 
            i.house_yearq i.county_id i.house_yearq#i.county_id if ccu ==1 , r

reg lhprice D treat post lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow     /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id if ccu ==1 ,r
		
*Announcement effecttime (1 quarter earlier &2 quarter earlier &3quarter earlier &4 quarter earlier), use ccus_nb.dta*
keep if dis <= 6500			
*1 quarter earlier*
gen ccus_yearq_f = ccus_year *10 + ccus_q -1 
replace ccus_yearq_f = ccus_year *9 + ccus_q +9 if ccus_q ==1
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(ccus_id)			
*2 quarter earlier*
gen ccus_yearq_f = ccus_year *10 + ccus_q -2 
replace ccus_yearq_f = ccus_year *9 + ccus_q +8 if ccus_q ==1
replace ccus_yearq_f = ccus_year *9 + ccus_q +8 if ccus_q ==2
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(ccus_id)			
*3 quarter earlier*
gen ccus_yearq_f = ccus_year *10 + ccus_q -3 
replace ccus_yearq_f = ccus_year *9 + ccus_q +7 if ccus_q ==1
replace ccus_yearq_f = ccus_year *9 + ccus_q +7 if ccus_q ==2
replace ccus_yearq_f = ccus_year *9 + ccus_q +7 if ccus_q ==3
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(ccus_id)
*4 quarter earlier*
gen ccus_yearq_f = ccus_year *10 + ccus_q -10
gen post_f =1
replace post_f =0 if house_yearq  < ccus_yearq_f
gen D_f = post_f* treat

reg lhprice D_f treat post_f lharea age lcarea lcbarea com_green ntl scsd dicp gdp_3 pow   /// 
            i.house_yearq i.ccus_id i.county_id i.house_yearq#i.ccus_id , cluster(ccus_id)
		
***********************
***3.CCUS-NB-CCUS******
***********************
*Sample statistics, use ccus_nb_ccus.dta*
keep if dis <= 6500
sum house_price treat post ccus house_area age com_area com_builarea com_green com_ratio ntl dicp gdp_3 gdp
			
*Main results, use ccus_nb_ccus.dta*
keep if dis <= 6500			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio /// 
            i.house_year i.pro_id , cluster(county_id)	

reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_year i.pro_id , cluster(county_id)

reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_year i.county_id i.pro_id , cluster(county_id)
				
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_year i.pro_id i.house_year#i.pro_id , cluster(county_id)
	
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_year i.county_id i.pro_id i.house_year#i.county_id , cluster(county_id)
	
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id , cluster(county_id)
			
*Common trend, use ccus_nb_ccus.dta*			
keep if dis <= 6500			
gen t = house_year - pro_year
tab t

forvalues i=8(-1)1{
   gen ddd_pre`i' = (t==-`i' & treat==1 & ccus==1)
   gen treat_pre`i' = (t==-`i' & treat==1)
   gen ccus_pre`i' = (t==-`i' & ccus==1)
   gen pre`i' = (t==-`i')
}
gen ddd_current=(t== 0 & treat==1 & ccus==1)
gen treat_current=(t== 0 & treat==1)
gen ccus_current=(t== 0 & ccus==1)
gen current=(t== 0 & ccus==1)
forvalues i=1(1)10{
   gen ddd_post`i'=(t==`i' & treat==1 & ccus==1)
   gen treat_post`i' = (t==`i' & treat==1)
   gen ccus_post`i' = (t==`i' & ccus==1)
   gen post`i' = (t==`i')
}

global ddd ddd_pre4 ddd_pre3 ddd_pre2 ddd_pre1 ///
           ddd_current ///
		   ddd_post2 ddd_post3 ddd_post4 ddd_post5 ddd_post6 ddd_post7 
global dd treat_pre8 treat_pre7 treat_pre6 treat_pre5 treat_pre4 treat_pre3 treat_pre2 treat_pre1 ///
          treat_post2 treat_post3 treat_post4 treat_post5 treat_post6 treat_post6 treat_post7 treat_post8 treat_post9 treat_post10 ///
		  ccus_pre8 ccus_pre7 ccus_pre6 ccus_pre5 ccus_pre4 ccus_pre3 ccus_pre2 ccus_pre1 ///
		  ccus_post2 ccus_post3 ccus_post4 ccus_post5 ccus_post6 ccus_post7 ccus_post8 ccus_post9 ccus_post10 treat_current ccus_current
global d pre8 pre7 pre6 pre5 pre4 pre3 pre2 pre1 current post2 post3 post4 post5 post6 post7 post8 post9 post10

reg lhprice $ddd $dd $d lharea lcarea com_green com_ratio gdp_3 lgdp dicp i.house_yearq i.county_id i.county_id#i.house_yearq
coefplot, baselevels ///
keep(ddd_pre* ddd_current ddd_post*) ///
vertical ///
coeflabels(ddd_pre8=-8 ddd_pre7=-7 ddd_pre6=-6 ddd_pre5=-5 ddd_pre4=-4 ddd_pre3=-3 ddd_pre2=-2 ddd_pre1=-1 ///
ddd_current=0 ddd_post1=1 ddd_post2=2 ddd_post3=3 ddd_post4=4 ddd_post5=5 ddd_post6=6 ddd_post7=7 ddd_post8=8 ddd_post9=9 ddd_post10=10) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(5,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-0.4(0.2)0.8,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
xtitle("Year relative to CCUS") ///
ytitle("Changes in housing price") ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) /// 
legend(order(2 "Coef. Estimate" 1 "95% Confidence Interval")) 
			
*Matching (PSM-DID&CEM-DID), use ccus_nb_ccus.dta*			
keep if dis <= 6500
*PSM-DID* 			
global psm " com_ratio ntl"
set seed 2020
gen norvar_1 = rnormal()
sort norvar_1
psmatch2 treat $psm i.county_id , outcome(lhprice) logit neighbor(2) ties common ate caliper(0.05)
pstest com_ratio ntl, both graph 
psgraph
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if _support ==1 , cluster(county_id)
*CEM-DID*
cem  age com_builarea lharea , tr(tri)
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id [iweight = cem_weight] , cluster(county_id)		
			
*Range (6200m&6800m), use ccus_nb_ccus.dta*
*6200m*			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl lgdp gdp_3 /// 
            i.house_yearq i.pro_id i.house_year#i.pro_id if dis <= 6200, cluster(county_id)
			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl lgdp gdp_3 /// 
            i.house_yearq i.county_id  i.pro_id i.house_year#i.county_id if dis <= 6200, cluster(county_id)
			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl lgdp gdp_3 /// 
            i.house_yearq i.pro_id i.county_id i.house_year#i.pro_id if dis <= 6200 , cluster(county_id)		
*6800m*
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio gdp_3 dicp /// 
            i.house_year i.pro_id i.house_year#i.pro_id if dis <= 6800, cluster(county_id)	
			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio gdp_3 dicp /// 
            i.house_year i.county_id i.pro_id i.house_year#i.county_id if dis <= 6800, cluster(county_id)	
			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green com_ratio gdp_3 dicp /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dis <= 6800, cluster(county_id)

*Falsitest, use ccus_nb_ccus.dta*
keep if dis <= 6500			
gen treat_f =0
replace treat_f =1 if dis > 3600 & dis <= 4200
gen d1_f  = treat_f  * post 
gen d3_f  = treat_f  * ccus
gen tri_f = post * treat_f * ccus

reg lhprice tri_f d1_f d2 d3_f post treat_f ccus lharea lcbarea ntl com_ratio gdp_3 dicp age /// 
            i.house_year i.pro_id i.house_year#i.pro_id , r

reg lhprice tri_f d1_f d2 d3_f post treat_f ccus lharea lcbarea ntl com_ratio gdp_3 dicp age /// 
            i.house_year i.county_id i.house_year#i.county_id, r

reg lhprice tri_f d1_f d2 d3_f post treat_f ccus lharea lcbarea ntl com_ratio gdp_3 dicp age /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id ,r
		
*Heterogeneity: nighttime lights, use ccus_nb_ccus.dta*
keep if dis <= 6500			
gen ntl2 =0
replace ntl2 =1 if ntl < 20
replace ntl2 =2 if ntl >=20 & ntl < 40
replace ntl2 =3 if ntl >=40 & ntl < 60
replace ntl2 =4 if ntl >=60 & ntl < 80
replace ntl2 =5 if ntl >=80 

reg lhprice tri d1 d2 d3 post treat ccus lcarea lcbarea ntl age com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_yearq i.pro_id i.county_id if ntl2==1 , cluster(county_id)
			
reg lhprice tri d1 d2 d3 post treat ccus lcarea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_yearq i.pro_id i.county_id i.house_yearq#i.pro_id if ntl2==2 , cluster(county_id)

reg lhprice tri d1 d2 d3 post treat ccus lcarea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_yearq i.pro_id i.county_id i.house_yearq#i.pro_id if ntl2==3 , cluster(county_id)

reg lhprice tri d1 d2 d3 post treat ccus lcarea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_yearq i.pro_id i.county_id i.house_yearq#i.pro_id if ntl2==4 , cluster(county_id)
			
reg lhprice tri d1 d2 d3 post treat ccus lcarea lcbarea ntl com_green com_ratio lgdp gdp_3 dicp /// 
            i.house_yearq i.pro_id i.county_id i.house_yearq#i.pro_id if ntl2==5 , cluster(county_id)
		
*Heterogeneity: dicp, use ccus_nb_ccus.dta*
keep if dis <= 6500
xtile dicp1 = dicp, nq(8)			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl com_green age lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==1 , r

reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==2 , r

reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl com_green age lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==3 , r

reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl com_green age lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==4 , r

reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl com_green age lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==5 , r

reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl com_green age lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==6 , r

reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl com_green age lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==7 , r
			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl com_green age lgdp gdp_3 /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if dicp1==8 , r			
			
*Heterogeneity: capacity, use ccus_nb_ccus.dta*
*no_pow means no ccus for the high carbon emission facilicities*
*Small_pow means the capacity of CCUS is smaller than 50000 tons/year, including 50000 tons/year*
*Big_pow means the capacity of CCUS is bigger than 50000 tons/year*	
keep if dis <= 6500
reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl lcbarea com_green gdp_3 dicp age ///
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if no_pow==1, r

reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl lcbarea com_green gdp_3 dicp age /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if small_pow==1, r
			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea ntl lcbarea com_green gdp_3 dicp age /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if big_pow==1, r

*Heterogeneity: carbon capture mode, use ccus_nb_ccus.dta*
*Pre_com means the carbon capture mode of ccus is pre_combustion*
*Post_com means the carbon capture mode of ccus is post_combustion*
*Oxy means the carbon capture mode of ccus is oxy-fuel combustion*
keep if dis <= 6500
reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea com_ratio ntl lgdp gdp_3 dicp /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if pre_com==1 , r		

reg lhprice tri d1 d2 d3 post treat ccus lharea lcarea com_ratio ntl lgdp gdp_3 dicp /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id if post_com==1 , r		

*Heterogeneity: type of ccus project, use ccus_nb_ccus.dta*
*cc means the ccus only capture carbon*
*ccu means the ccus capture and utilize carbon*
keep if dis <= 6500
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green gdp_3 dicp ///
            i.house_year i.pro_id i.house_year#i.pro_id if cc==1 , cluster(com_id)
	
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green gdp_3 dicp ///
            i.house_year i.county_id i.pro_id i.house_year#i.county_id  if cc==1 , cluster(com_id)
	
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green gdp_3 dicp ///
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id  if cc==1, cluster(com_id)
			
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green gdp_3 dicp ///
            i.house_year i.pro_id i.house_year#i.pro_id  if ccu==1 , cluster(com_id)
	
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green gdp_3 dicp ///
            i.house_year i.county_id i.pro_id i.house_year#i.county_id  if ccu==1 , cluster(com_id)
	
reg lhprice tri d1 d2 d3 post treat ccus lharea lcbarea ntl com_green gdp_3 dicp /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id  if ccu==1 , cluster(com_id)

*Announcement effecttime (1 quarter earlier &2 quarter earlier &3quarter earlier &4 quarter earlier), use ccus_nb_ccus.dta*
keep if dis <= 6500			
*1 quarter earlier*
gen pro_yearq_f = pro_year *10 + pro_q -1 
replace pro_yearq_f = pro_year *9 + pro_q +9 if pro_q ==1
gen post_f =1
replace post_f =0 if house_yearq  < pro_yearq_f
gen tri_f = post_f* treat * ccus
gen d1_f = post_f * treat
gen d2_f = post_f * ccus

reg lhprice tri_f d1_f d2_f d3 post treat ccus lharea lcbarea ntl com_ratio gdp_3 dicp age      /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id , cluster(com_id)		

*2 quarter earlier*
gen pro_yearq_f = pro_year *10 + pro_q -2
replace pro_yearq_f = pro_year *9 + pro_q +8 if pro_q ==1 
replace pro_yearq_f = pro_year *9 + pro_q +8 if pro_q ==2
gen post_f =1
replace post_f =0 if house_yearq  < pro_yearq_f
gen tri_f = post_f* treat * ccus
gen d1_f = post_f * treat
gen d2_f = post_f * ccus

reg lhprice tri_f d1_f d2_f d3 post treat ccus lharea lcbarea ntl com_ratio gdp_3 dicp age      /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id , cluster(com_id)
est store a2			
*3 quarter earlier*
gen pro_yearq_f = pro_year *10 + pro_q -3
replace pro_yearq_f = pro_year *9 + pro_q +7 if pro_q ==1 
replace pro_yearq_f = pro_year *9 + pro_q +7 if pro_q ==2
replace pro_yearq_f = pro_year *9 + pro_q +7 if pro_q ==3
gen post_f =1
replace post_f =0 if house_yearq  < pro_yearq_f
gen tri_f = post_f* treat * ccus
gen d1_f = post_f * treat
gen d2_f = post_f * ccus

reg lhprice tri_f d1_f d2_f d3 post treat ccus lharea lcbarea ntl com_ratio gdp_3 dicp age      /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id , cluster(com_id)
est store a3
*4 quarter earlier*
gen pro_yearq_f = pro_year *10 + pro_q -10
gen post_f =1
replace post_f =0 if house_yearq  < pro_yearq_f
gen tri_f = post_f* treat * ccus
gen d1_f = post_f * treat
gen d2_f = post_f * ccus

reg lhprice tri_f d1_f d2_f d3 post treat ccus lharea lcbarea ntl com_ratio gdp_3 dicp age      /// 
            i.house_year i.pro_id i.county_id i.house_year#i.pro_id , cluster(com_id)

*****************
***4.Air pollutants******
*****************
*Sample statistics, use ccus_airpo.dta*
sum no2 so2 o3 co pm25 pm10

*Common trend (no2 so2 o3 co), use ccus_airpo.dta*
*no2*
gen t = house_year - ccus_year
tab t

forvalues i=10(-1)1{
   gen pre`i' = (t==-`i' & treat==1)
}
gen current=(t== 0 & treat==1)
forvalues i=1(1)11{
  gen post`i'=(t==`i' & treat==1)
}

reg no2 pre6 pre5 pre4 pre3 pre2 current post1 post2 post3 post4 post5 pre6 /// 
             com_green com_ratio ntl ndvi scsd dicp pow i.house_yearq i.county_id , cluster(city_id)
			 
coefplot, baselevels ///
keep(pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4 post5 post6) ///
vertical ///
coeflabels(pre6=-6 pre5=-5 pre4=-4 pre3=-3 pre2=-2 pre1=-1 ///
current=0 post1=1 post2=2 post3=3 post4=4 post5=5 post6=6) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(4,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-50(20)30,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
xtitle("Year relative to CCUS") ///
ytitle("Changes in housing price") ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) ///
legend(order(2 "Coef. Estimate" 1 "95% Confidence Interval")) 

*so2*
gen t = house_year - ccus_year
tab t

forvalues i=10(-1)1{
   gen pre`i' = (t==-`i' & treat==1)
}
gen current=(t== 0 & treat==1)
forvalues i=1(1)11{
  gen post`i'=(t==`i' & treat==1)
}

reg so2 pre6 pre5 pre4 pre3 pre2 current post1 post2 post3 post4 post5 pre6 /// 
             ntl ndvi scsd dicp pow  i.house_yearq i.ccus_id , cluster(county_id)
			 
coefplot, baselevels ///
keep(pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4 post5 post6) ///
vertical ///
coeflabels(pre6=-6 pre5=-5 pre4=-4 pre3=-3 pre2=-2 pre1=-1 ///
current=0 post1=1 post2=2 post3=3 post4=4 post5=5 post6=6) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(4,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-15(5)15,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
xtitle("Year relative to CCUS") ///
ytitle("Changes in housing price") ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) ///
legend(order(2 "Coef. Estimate" 1 "95% Confidence Interval")) 

*o3*
gen t = house_year - ccus_year
tab t

forvalues i=10(-1)1{
   gen pre`i' = (t==-`i' & treat==1)
}
gen current=(t== 0 & treat==1)
forvalues i=1(1)11{
  gen post`i'=(t==`i' & treat==1)
}

reg o3 pre6 pre5 pre4 pre3 pre2 current post1 post2 post3 post4 post5 pre6 /// 
             ntl ndvi scsd pow  i.house_year i.ccus_id , cluster(county_id)
			 
coefplot, baselevels ///
keep(pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4 post5 post6) ///
vertical ///
coeflabels(pre6=-6 pre5=-5 pre4=-4 pre3=-3 pre2=-2 pre1=-1 ///
current=0 post1=1 post2=2 post3=3 post4=4 post5=5 post6=6) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(4,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-60(20)20,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
xtitle("Year relative to CCUS") ///
ytitle("Changes in housing price") ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) ///
legend(order(2 "Coef. Estimate" 1 "95% Confidence Interval")) 

*co*
gen t = house_year - ccus_year
tab t

forvalues i=10(-1)1{
   gen pre`i' = (t==-`i' & treat==1)
}
gen current=(t== 0 & treat==1)
forvalues i=1(1)11{
  gen post`i'=(t==`i' & treat==1)
}

reg co pre6 pre5 pre4 pre3 pre2 current post1 post2 post3 post4 post5 pre6 /// 
            ntl ndvi scsd gdp_1 i.house_yearq i.ccus_id i.house_yearq#i.city_id  , r
			 
coefplot, baselevels ///
keep(pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4 post5 post6) ///
vertical ///
coeflabels(pre6=-6 pre5=-5 pre4=-4 pre3=-3 pre2=-2 pre1=-1 ///
current=0 post1=1 post2=2 post3=3 post4=4 post5=5 post6=6) /// 
yline(0,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
xline(4,lwidth(vthin) lpattern(solid) lcolor(teal)) ///
ylabel(-0.3(0.2)0.5,labsize(*0.85) angle(0)) xlabel(,labsize(*0.85)) ///
xtitle("Year relative to CCUS") ///
ytitle("Changes in housing price") ///
msymbol(O) msize(small) mcolor(gs1) ///
addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ///
ciopts(recast(rline) lwidth(thin) lpattern(dash) lcolor(gs2)) ///
graphregion(color(white)) ///
legend(order(2 "Coef. Estimate" 1 "95% Confidence Interval")) 	
	
*Main results (no2 so2 o3 co), use ccus_airpo.dta*	
*no2*	
reg no2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id , cluster(county_id)

reg no2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id , cluster(county_id)
			
reg no2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.house_year#i.ccus_id, cluster(county_id)

reg no2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id i.house_year#i.county_id, cluster(county_id)

reg no2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id , cluster(county_id)

*so2*
reg so2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id , cluster(county_id)

reg so2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id , cluster(county_id)
			
reg so2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.house_year#i.ccus_id, cluster(county_id)

reg so2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id i.house_year#i.county_id, cluster(county_id)

reg so2 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id , cluster(county_id)

*o3*
reg o3 D treat post com_green scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id , cluster(county_id)

reg o3 D treat post com_green scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id , cluster(county_id)
			
reg o3 D treat post com_green scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.house_year#i.ccus_id, cluster(county_id)

reg o3 D treat post com_green scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id i.house_year#i.county_id, cluster(county_id)

reg o3 D treat post com_green scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id , cluster(county_id)

*co*
reg co D treat post com_green com_ratio ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id , cluster(county_id)

reg co D treat post com_green com_ratio ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id , cluster(county_id)
			
reg co D treat post com_green com_ratio ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.house_year#i.ccus_id, cluster(county_id)

reg co D treat post com_green com_ratio ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id i.house_year#i.county_id, cluster(county_id)

reg co D treat post com_green com_ratio ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id , cluster(county_id)

*pm2.5*
reg pm25 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id , cluster(county_id)

reg pm25 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id , cluster(county_id)
			
reg pm25 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.house_year#i.ccus_id, cluster(county_id)

reg pm25 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id i.house_year#i.county_id, cluster(county_id)

reg pm25 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id , cluster(county_id)

*pm10*
reg pm10 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id , cluster(county_id)

reg pm10 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id , cluster(county_id)
			
reg pm10 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.house_year#i.ccus_id, cluster(county_id)

reg pm10 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.county_id i.house_year#i.county_id, cluster(county_id)

reg pm10 D treat post com_green ntl ndvi scsd dicp gdp_3 gdp_2 pow i.house_year i.ccus_id i.county_id i.house_year#i.ccus_id , cluster(county_id)		
			
*****************
*4.Fig***********
*****************
*Main results*
*CCUS-RE*
matrix mean = (-0.1170239, -0.0747629, -0.0741601, -0.059409, -0.0594526, -0.059409)

matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean

matrix CI = (-0.2391505, -0.1469557, -0.1466212, -0.1300331, -0.1301082, -0.1300331\ ///
0.0051027, -0.00257, -0.0016989, 0.0112151, 0.011203, 0.0112151)
matrix colnames CI = model1 model2 model3 model4 model5 model6

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.072042, -0.0719694, -0.0719694, -0.0730295, -0.0719536, -0.0730295)

matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean

matrix CI = (-0.0763809, -0.0723706, -0.0723706, -0.0730295, -0.0719536, -0.0730295\ ///
-0.0677032, -0.0715683, -0.0715683, -0.0730295, -0.0719536, -0.0730295)
matrix colnames CI = model1 model2 model3 model4 model5 model6

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = (-.0716897 , -.0681608 , -.054605, -.0547761, -.0551021, -.0588453)

matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean

matrix CI = (-.1140778 , -.1050661, -.0816515, -.0823602, -.0820669 , -.0932592\ ///
-.0293017, -.0312554,  -.0275585, -.027192, -.0281373, -.0244314)
matrix colnames CI = model1 model2 model3 model4 model5 model6

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Matching*
*CCUS-RE*
matrix mean = (-0.059409, -0.0590201, -0.0549189)

matrix colnames mean = model1 model2 model3
matrix rownames mean = mean

matrix CI = (-0.0869507, -0.0865603, -0.0888907\ ///
-0.0318674, -0.0314798, -0.0209471)
matrix colnames CI = model1 model2 model3

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.0730295, -0.0726516, -0.0855159)

matrix colnames mean = model1 model2 model3
matrix rownames mean = mean

matrix CI = (-0.1371766, -0.1432969, -0.1670098\ ///
-0.0088824, -0.0020062, -0.004022)
matrix colnames CI = model1 model2 model3

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = (-.0588453, -.0608867 , -.0508456)

matrix colnames mean = model1 model2 model3
matrix rownames mean = mean

matrix CI = (-.0908837, -.0924688, -.0508456 \ ///
-.0444255, -.0293046, -.0508456)
matrix colnames CI = model1 model2 model3

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Range*
*CCUS-RE*
matrix mean = (-0.06727, -0.06727, -0.06727, -0.0485204, -0.0497874, -0.0496578)
matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean
matrix CI = (-0.0947085, -0.0947085, -0.0947085, -0.0764573, -0.0777182, -0.0775234\ ///
-0.0398315, -0.0398315, -0.0398315, -0.0205835, -0.0218566, -0.0217921)
matrix colnames CI = model1 model2 model3 model4 model5 model6
matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.0731667, -0.0733658, -0.0731667, -0.0632075, -0.0632887, -0.0632075)
matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean
matrix CI = (-0.1372656, -0.1392102, -0.1372656, -0.1269297, -0.1278052, -0.1269297\ ///
-0.0090677, -0.0075214, -0.0090677, 0.0005146, 0.0030278, 0.0005146)
matrix colnames CI = model1 model2 model3 model4 model5 model6
matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = (-.070793, -.0748634, -.0721301 , -.0428563 , -.0435016 , -.0479579)
matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean
matrix CI = (-.0937772 , -.0987986 , -.0952488, -.0684048 , -.0684246 , -.0815771\ ///
-.0478087, -.0509283, -.0490113, -.0173078, -.0185786, -.0143387)
matrix colnames CI = model1 model2 model3 model4 model5 model6
matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Falsitest*
*CCUS-RE*
matrix mean = (-0.059409, 0.0025098, 0.0024821, 0.0024824)

matrix colnames mean = model1 model2 model3 model4
matrix rownames mean = mean

matrix CI = (-0.0869513, -0.0322051, -0.0322339, -0.0322344\ ///
-0.0318667, 0.0372246, 0.0371981, 0.0371992)
matrix colnames CI = model1 model2 model3 model

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.0730295, 0.0166774, 0.0213914, 0.0166774)

matrix colnames mean = model1 model2 model3 model4
matrix rownames mean = mean

matrix CI = (-0.1371766, -0.010111, -0.0055794, -0.010111\ ///
-0.0088824, 0.0434658, 0.0483622, 0.0434658)
matrix colnames CI = model1 model2 model3 model4

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = (-.0588453, .0163705 , .0224491 , .0164109)

matrix colnames mean = model1 model2 model3 model4
matrix rownames mean = mean

matrix CI = (-.0908837, -.0109926,  -.0057264 , -.0109569 \ ///
-.0444255, .0437337, .0506247, .0437787)
matrix colnames CI = model1 model2 model3 model4

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Heterogeneity: nighttime lights*
*CCUS-RE*
matrix mean = (-0.031323, -0.0709102, -0.0600804, -0.2630476)

matrix colnames mean = model1 model2 model3 model4
matrix rownames mean = mean

matrix CI = (-0.0815426, -0.1128139, -0.1182519, -0.4170924\ ///
0.0188965, -0.0290064, -0.0019089, -0.1090029)
matrix colnames CI = model1 model2 model3 model4

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.0120774, -0.0776848, -0.1404314)

matrix colnames mean = model1 model2 model3 
matrix rownames mean = mean

matrix CI = (-0.0937113, -0.1461296, -0.2265116\ ///
0.0695565, -0.0092399, -0.0543511)
matrix colnames CI = model1 model2 model3 

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = (-.0146502, -.0750924 , -.1009601)

matrix colnames mean = model1 model2 model3 
matrix rownames mean = mean

matrix CI = (-.0299944 , -.1015717 , -.1009601 \ ///
.000694, -.0486131, -.1009601)
matrix colnames CI = model1 model2 model3 

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Heterogeneity: dicp*
*CCUS-RE*
matrix mean = (0.0072621, 0.0593897, -0.0907248, -0.0951157, -0.0755445, -0.1065216, -0.1017639, -0.1320936)

matrix colnames mean = model1 model2 model3 model4 model5 model6 model7 model8
matrix rownames mean = mean

matrix CI = (-0.028429, -0.0184211, -0.1084987, -0.1116785, -0.0983194, -0.1297998, -0.1216571, -0.1525358\ ///
0.0429532, 0.1372005, -0.0729509, -0.0785528, -0.0527695, -0.0832433, -0.0818706, -0.1116515)
matrix colnames CI = model1 model2 model3 model4 model5 model6 model7 model8

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.0382425, -0.0834254, -0.0876567, -0.1619802, -0.1522442)

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (-0.130335, -0.1272312, -0.1921161, -0.2318608, -0.2148351\ ///
 0.05385, -0.0396195, 0.0168028, -0.0920996, -0.0896533 )
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = (.029141 , -.1689472 , -.1031914, -.049519 , -.0829896 )

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (-.0768163,  -.2367877, -.2021978 , -.1570653 , -.1266085 \ ///
 .1350983, -.1011067, -.004185, .0580274, -.0393706 )
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Heterogeneity: capacity*
*CCUS-RE*
matrix mean = (-0.0154546, -0.2489451)

matrix colnames mean = model1 model2
matrix rownames mean = mean

matrix CI = (-0.0545136, -0.3006574\ ///
0.0236045, -0.1972328)
matrix colnames CI = model1 model2

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.0254222, -0.3033973)

matrix colnames mean = model1 model2
matrix rownames mean = mean

matrix CI = (-0.1121332, -0.3709827\ ///
0.612889, -0.235812)
matrix colnames CI = model1 model2

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = (-.0220426, -.2881749)

matrix colnames mean = model1 model2
matrix rownames mean = mean

matrix CI = (-.0873471, -.3786943 \ ///
.0432619,  -.1976554)
matrix colnames CI = model1 model2

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Heterogeneity: carbon capture mode*
*CCUS-RE*
matrix mean = (-0.1176899, -0.0876299, -0.039049)

matrix colnames mean = model1 model2 model3
matrix rownames mean = mean

matrix CI = (-0.2201641, -0.1209057, -0.1037303\ ///
-0.0152156, -0.054354, 0.0256322)
matrix colnames CI = model1 model2 model3

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-0.2509406, -0.0155083)

matrix colnames mean = model1 model2 
matrix rownames mean = mean

matrix CI = (-0.3508805, -0.0784364\ ///
-0.1510007, 0.0474197 )
matrix colnames CI = model1 model2 

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = ( -.2698971 , .023738 )

matrix colnames mean = model1 model2 
matrix rownames mean = mean

matrix CI = (-.3663812 ,  -.0357812 \ ///
-.1734131, .0832573)
matrix colnames CI = model1 model2 

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Heterogeneity: type of ccus project*
*CCUS-RE*
matrix mean = (-.0451146 , -.0451146  , -.0451146, -.1186164, -.1186164 , -.1186164 )

matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean

matrix CI = ( -.0779215  , -.0779215 , -.0779215 , -.1678138, -.1678052 , -.1678138 \ ///
-.0123078, -.0123078,  -.0123078 , -.069419, -.0694276 , -.069419)
matrix colnames CI = model1 model2 model3 model4 model5 model6

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB*
matrix mean = (-.0374584  ,-.0374584 ,-.0374584 ,-.0174078 , -.0174078  , -.0174078 )

matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean

matrix CI = (-.2165345  ,-.2165345 , -.2165345, -.0805209 , -.0805209  , -.0805209 \ ///
.1416177, .1416177, .1416177, .0457054, .0457054 , .0457054)
matrix colnames CI = model1 model2 model3 model4 model5 model6

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*CCUS-NB-CCUS*
matrix mean = ( -.0817949,-.080024 ,-.0891899  ,-.0216806 , -.0216806 , -.0216806 )

matrix colnames mean = model1 model2 model3 model4 model5 model6
matrix rownames mean = mean

matrix CI = (-.2969125  ,-.2996053 ,-.3030628 ,-.1762757 ,-.1762757  , -.1762757 \ ///
.1333228, .1395574,.124683, .1329144, .1329144, .1329144)
matrix colnames CI = model1 model2 model3 model4 model5 model6

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1) ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))

*Air pollution*
*no2*
matrix mean = (2.388116  , 2.373296  , 2.086522 , 2.05175  , 2.05175 )

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (.2133348,  .2863997 , -.2447599 , -.3175198 , -.3175198 \ ///
 4.562898, 4.460193, 4.417804, 4.42102, 4.42102 )
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))
*so2*
matrix mean = (1.723877 , 1.724869 , 2.034721, 2.003561  , 2.003561 )

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (.0830703 ,.0844178,1.896564 ,1.964861 , 1.964861 \ ///
 3.364683, 3.36532,2.172879, 2.042261, 2.042261 )
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))
*o3*
matrix mean = (2.012162, 2.097407  ,.9330221 ,.9330221 ,.9330221 )

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (-.27951 ,-.649789,-.3008909,-.3019505,-.3019505  \ ///
 4.303835,4.844602,2.166935, 2.167995,2.167995) 
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))
*co*
matrix mean = (.0132959 , .0081993 ,.0072502 ,.0064972,.0064972 )

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (-.0257244  ,-.0182395 ,.0005386 , -.0003815,-.0003815  \ ///
 .0523162,.0346381,.0139617, .013376,.013376) 
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))
*pm25*
matrix mean = (3.694115 ,  3.104848 ,2.391499 , 2.312843 , 2.312843  )

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (-1.546635  ,-1.502331 ,-2.009582 , -2.174424 , -2.174424  \ ///
 8.934865,7.712027,6.79258, 6.80011,6.80011) 
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))
*pm10*
matrix mean = (1.70474 , .9712112 ,.6734121,  .5526117  ,.5526117  )

matrix colnames mean = model1 model2 model3 model4 model5

matrix CI = (-2.988803 , -2.688198 ,-3.063271,  -3.298996 ,-3.298996  \ ///
 6.398283,  4.63062, 4.410095, 4.40422, 4.40422) 
matrix colnames CI = model1 model2 model3 model4 model5

matrix rownames CI = ll95 ul95
coefplot matrix(mean), pstyle(p1)  ci(CI) vertical legend(order(2 "Estimation" 1 "95% confidence intervals"))







