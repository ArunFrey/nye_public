*******************************************************
* PURPOSE: MULTILEVEL MODELING + PREDICTIONS IN STATA *
*******************************************************

cd "path-to-repo"

*ssc install estout, replace
*ssc install combomarginsplot, replace
*ssc install coefplot, replace

clear
capture log close
set more off

log using logs/analysis, replace

timer clear 

set scheme s1mono

*** LOAD DATA

use "data/processed/district_attack_date.dta", replace

*** ATTACH LABELS TO VARIABLES
run "src/analysis/attaching_labels.do"

*** SELECT SAMPLE
* sample 15

** SPECIFYING PANEL DESIGN
egen keynr = group(key)
format date %td

xtset keynr date 


*** SELECT MIXED EFFECTS OR SIMPLE LOGIT 
*local model "logit"
local model "xtlogit"

** SPECIFYING ACCURACY OF MODELS (TO SAVE TIME)
*local accuracy "low"
local accuracy "high"


if "`model'" == "xtlogit" {
	local reffects ", re vce(robust) intpoints(10)"
	local pred_spec "predict(pu0)" 
}
if "`model'" == "logit" {
	local reffects ", vce(cluster keynr)"
	local pred_spec ""
	
}
if "`accuracy'" == "low" {
	local itt = 100
	local npd = 2
}
if "`accuracy'" == "high" {
	local itt = 10
	local npd = 0.5
}

*** SELECT CONTROLS
local X_i "z_ss_total east_bin z_unemp_r_n z_nonEU_p z_turnout14 z_afd14 z_npd14 z_logpop city z_bip_pp z_mf_ratio z_hom_r"

local X_i_noz "z_ss_total east_bin z_unemp_r_n z_nonEU_p z_turnout14 z_afd14 npd14 z_logpop city z_bip_pp z_mf_ratio z_hom_r"

local Z_t "z_easy_registration i.day z_count_day"

local Z_t_itt  "z_easy_registration i.day"

local D_w1 "Col_w1 Paris_Jan_15_w1 Brussels_14_w1 Copenhagen_15_w1 Paris_Apr_15_w1 StQuent_15_w1 Paris_Nov_15_w1 Brussels_16_w1 Magnanville_16_w1 Nice_16_w1 StEtienne_16_w1 Berlin_16_w1"

local D_allw "Col_w1 Col_w2 Col_w3 Col_w4 Paris_Jan_15_w1 Paris_Jan_15_w2 Paris_Jan_15_w3 Brussels_14_w1 Copenhagen_15_w1 Paris_Apr_15_w1 StQuent_15_w1 Paris_Nov_15_w1 Brussels_16_w1 Magnanville_16_w1 Nice_16_w1 StEtienne_16_w1 Berlin_16_w1"

local D_allwg "Col_negw1 Col_w1 Col_w2 Col_w3 Col_w4 Paris_Jan_15_w1 Paris_Jan_15_w2 Paris_Jan_15_w3 Brussels_14_w1 Copenhagen_15_w1 Paris_Apr_15_w1 StQuent_15_w1 Paris_Nov_15_w1 Brussels_16_w1 Magnanville_16_w1 Berlin_16_w1 Essen_16_w1 Essen_16_w2 Wuerzburg_16_w1 Wuerzburg_16_w2 Wuerzburg_16_w3 Berlin_15_w1"



*** SORT DATA
sort key year date



***************************  Models  ***************************

eststo clear 

foreach y in attacks_s attacks_s_shelt attacks_s_nogov {
	timer on 1
	
	* model 1
	`model' `y'_bin ///
		`X_i' ///
		`Z_t' ///
		z_tdif_key_`y' ///
		z_tdif_other_`y' ///
		`D_w1' ///
		`reffects'
		
	est store m1_`y'
	predict phat_m1_`y'
	
	* model 2
	`model' `y'_bin ///
		`X_i' ///
		`Z_t' ///
		z_tdif_key_`y' ///
		z_tdif_other_`y' ///
		`D_allw' ///
		`reffects'
	
	est store m2_`y'
	predict phat_m2_`y'

	* model 3
	`model' `y'_bin ///
		`X_i' ///
		`Z_t' ///
		z_tdif_key_`y' ///
		z_tdif_other_`y' ///
		`D_allwg' ///
		`reffects'
		
	est store m3_`y'
	predict phat_m3_`y'

	* model 4
	`model' `y'_bin ///
		`X_i' ///
		`Z_t_itt' ///
		z_tdif_key_`y' ///
		z_tdif_other_`y' ///
		c.days_Col##i.nye ///
		`reffects'
		
	est store m4_`y'
	predict phat_m4_`y'

	* model 5
	`model' `y'_bin ///
		`X_i' ///
		`Z_t_itt' ///
		z_tdif_key_`y' ///
		z_tdif_other_`y' ///
		i.`y'_p90##i.nye##c.days_Col ///
		c.z_dist##i.nye##c.days_Col ///
		`reffects'
		
	est store m5_`y'
	predict phat_m5_`y'

	* model 6
	`model' `y'_bin ///
		`X_i' ///
		`Z_t_itt' ///
		z_tdif_key_`y' ///
		z_tdif_other_`y' ///
		c.z_npd14##i.nye##c.days_Col ///
		c.z_dist##i.nye##c.days_Col ///
		`reffects'
		
	est store m6_`y'
	predict phat_m6_`y'

	* model 7
	`model' `y'_bin ///
		`X_i_noz' ///
		`Z_t_itt' ///
		z_tdif_key_`y' ///
		z_tdif_other_`y' ///
		c.npd14##i.nye##c.days_Col ///
		c.z_dist##i.nye##c.days_Col ///
		`reffects'
		
	est store m7_`y'
	predict phat_m7_`y'
	
	timer off 1
	timer list 1
}






******************************** Tables ********************************

local format "replace label varlabels(`e(labels)') eqlabels(none) nobaselevel se(2) b(2) eform compress lines nogap aic noomitted nonumber nonotes star(+ 0.1 * 0.05 ** 0.01 *** 0.001) sca(sigma_u sd(district)) stats(N aic, fmt(0 0) labels(Observations AIC)) varwidth(40)"

local format_tex "booktabs alignment(....)"

local rename "rename(z_tdif_key_attacks_nd z_tdif_key z_tdif_key_attacks_s z_tdif_key z_tdif_key_attacks_s_nogov z_tdif_key z_tdif_key_attacks_a_shelt z_tdif_key z_tdif_key_attacks_s_shelt z_tdif_key z_tdif_other_attacks_nd z_tdif_other z_tdif_other_attacks_s z_tdif_other z_tdif_other_attacks_s_nogov z_tdif_other z_tdif_other_attacks_a_shelt z_tdif_other z_tdif_other_attacks_s_shelt z_tdif_other days_Col Daysbefore 1.nye NYE 1.nye#c.days_Col Daysafter 1.attacks_nd_p90 Hostile 1.attacks_s_p90 Hostile 1.attacks_s_nogov_p90 Hostile 1.attacks_a_shelt_p90 Hostile 1.attacks_s_shelt_p90 Hostile 1.attacks_nd_p90#c.days_Col HostilexDaysbefore 1.attacks_s_p90#c.days_Col HostilexDaysbefore 1.attacks_s_nogov_p90#c.days_Col HostilexDaysbefore 1.attacks_a_shelt_p90#c.days_Col HostilexDaysbefore 1.attacks_s_shelt_p90#c.days_Col HostilexDaysbefore 1.attacks_nd_p90#1.nye HostilexNYE 1.attacks_s_p90#1.nye HostilexNYE 1.attacks_s_nogov_p90#1.nye HostilexNYE 1.attacks_a_shelt_p90#1.nye HostilexNYE 1.attacks_s_shelt_p90#1.nye HostilexNYE 1.attacks_nd_p90#1.nye#c.days_Col HostilexDaysafter 1.attacks_s_p90#1.nye#c.days_Col HostilexDaysafter 1.attacks_s_nogov_p90#1.nye#c.days_Col HostilexDaysafter 1.attacks_a_shelt_p90#1.nye#c.days_Col HostilexDaysafter  1.attacks_s_shelt_p90#1.nye#c.days_Col HostilexDaysafter c.z_npd14#c.days_Col NPDxDaysbefore 1.nye#c.z_npd14 NPDxNYE 1.nye#c.z_npd14#c.days_Col NPDxDaysafter)"


forvalues i=1/6 {
	
	if `i' <= 3 {
		local vars "Col*"
	} 
	if `i' == 4 {
		local vars "Daysbefore NYE Daysafter"
	} 
	if `i' == 5 {
		local vars "Daysbefore NYE Daysafter Hostile HostilexDaysbefore HostilexNYE HostilexDaysafter"
	} 
	if `i' == 6 {
		local vars "Daysbefore NYE Daysafter z_npd14 NPDxDaysbefore NPDxNYE NPDxDaysafter"
	}
	
	* All 
	esttab m`i'_attacks_s m`i'_attacks_s_shelt m`i'_attacks_s_nogov, ///
		`rename' ///
		`format'
	
	esttab m`i'_attacks_s m`i'_attacks_s_shelt m`i'_attacks_s_nogov using output/tables/m`i'.txt, ///
		`rename' ///
		`format'
	
	esttab m`i'_attacks_s m`i'_attacks_s_shelt m`i'_attacks_s_nogov using output/tables/m`i'.tex, ///
		`rename' ///
		`format' ///
		`format_tex'
	
	* Select
	esttab m`i'_attacks_s m`i'_attacks_s_shelt m`i'_attacks_s_nogov, ///
		`rename' ///
		keep(`vars') ///
		order(`vars') ///
		`format' 
	
	esttab m`i'_attacks_s m`i'_attacks_s_shelt m`i'_attacks_s_nogov using output/tables/m`i'_select.txt, ///
		`rename' ///
		keep(`vars') ///
		order(`vars') ///
		`format'

	esttab m`i'_attacks_s m`i'_attacks_s_shelt m`i'_attacks_s_nogov using output/tables/m`i'_select.tex, ///
		`rename' ///		
		keep(`vars') ///
		order(`vars') ///
		`format' ///
		`format_tex'
}

* Plots ****
foreach y in attacks_s attacks_s_shelt attacks_s_nogov  {
	
	timer on 1
	
	* ITT Plot
	est restore m4_`y'
	
	quietly margins, ///
	at(days_Col=(-730(`itt')0) nye = 0) ///
	atmeans saving(tmp/linear_before, replace) `pred_spec'
	quietly margins, ///
	at(days_Col=(0(`itt')365) nye = 1) ///
	atmeans saving(tmp/linear_after, replace) `pred_spec' 

	combomarginsplot tmp/linear_before tmp/linear_after, ///
	plotdim(_filenumber) labels("Before NYE" "After NYE") ///
	legend(off) ///
	recast(line) ciopt(color(gs11%30) lcolor(gs8%80)) recastci(rarea) ///
	xlabel(-750(150)350) ///
	xtitle(Days relative to NYE) ///
	ytitle(Probability of attack in district/day) title("") ///
	xline(0, lstyle(xyline) lcolor("red")) ///
	plot1opts(lpattern("1") lcolor(black)) ///
	plot2opts(lpattern("1") lcolor(black))
	
	graph save output/plots/m4_`y', replace
	graph export output/plots/m4_`y'.pdf, replace

	* Hostility plot
	est restore m5_`y'

	quietly margins, ///
	at(days_Col=(-730(`itt')0) nye = 0 `y'_p90 = 0) ///
	atmeans saving(tmp/before_nohigh, replace) `pred_spec'
	quietly margins, ///
	at(days_Col=(0(`itt')365) nye = 1 `y'_p90 = 0) ///
	atmeans saving(tmp/after_nohigh, replace) `pred_spec'

	quietly margins, ///
	at(days_Col=(-730(`itt')0) nye = 0 `y'_p90 = 1) ///
	atmeans saving(tmp/before_high, replace) `pred_spec'
	quietly margins, ///
	at(days_Col=(0(`itt')365) nye = 1 `y'_p90 = 1) ///
	atmeans saving(tmp/after_high, replace) `pred_spec'
	
	combomarginsplot tmp/before_nohigh tmp/after_nohigh tmp/before_high tmp/after_high, ///
	plotdim(_filenumber) ///
	legend(order(5 "Not hostile" 7 "Hostile") ring(0) position(11)) ///
	recast(line) ciopt(color(gs11%30) lcolor(gs8%80)) recastci(rarea) ///
	xlabel(-750(150)350) ///
	xtitle(Days relative to NYE) ///
	ytitle(Probability of attack in district/day) title("") ///
	xline(0, lstyle(xyline) lcolor("red")) ///
	plot1opts(lpattern("1") lcolor(black)) ///
	plot2opts(lpattern("1") lcolor(black)) ///
	plot3opts(lpattern("shortdash") lcolor(black)) ///
	plot4opts(lpattern("shortdash") lcolor(black)) 
	
	graph save output/plots/m5_`y', replace
	graph export output/plots/m5_`y'.pdf, replace

	* NPD Plot
	est restore m7_`y'
	
	quietly margins, ///
	dydx(nye) at(npd14=(0(`npd')6) days_Col = 0) ///
	atmean saving(tmp/npd_margins, replace) `pred_spec'
	
	marginsplot , ///
	recast(line) ciopt(color(gs11%30) lcolor(gs8%80)) ///
	recastci(rarea) yline(0, lpattern("-")) ///
	xtitle("NPD Share (2014 EP)") ///
	ytitle("Effect of NYE on Pr(Attack)") ///
	title("") ///
	legend(off) ///
	addplot(hist npd14, yaxis(2) color(gs11%30) lcolor(gs8%80)) 

	graph save output/plots/m7_`y', replace
	graph export output/plots/m7_`y'.pdf, replace
	
	timer off 1
	timer list 1
}


* 	CLOSE LOG FILE

log close



*END 
