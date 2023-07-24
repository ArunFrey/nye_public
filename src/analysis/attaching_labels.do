*******************************************************
* PAPER: 	PHD PAPER 1								  *
* PURPOSE: ATTACHING VAR LABELS AND NAMES			  *
*******************************************************


* recode variables
drop week
gen week = week(date)

*
order key date 

* recode factor vars to numeric
recode attacks_s_p90 (1=0) (2=1)
recode attacks_s_nogov_p90 (1=0) (2=1)
recode attacks_s_shelt_p90 (1=0) (2=1)

lab var z_ss_total "Refugee share" 
lab var z_easy_registration "Monthly arrivals"
lab var east_bin "East" 
lab var z_unemp_r_n "Unemployment rate"
lab var z_nonEU_p "Non-EU Foreign pop." 
lab var z_turnout14 "Voting turnout"
lab var z_afd14 "AfD Strength" 
lab var z_npd14 "NPD Strength" 
lab var npd14 "NPD Strength" 

gen z_tdif_key=1
lab var z_tdif_key "Attacks in district (4 weeks)"
lab var z_tdif_key_attacks_s "Attacks in district (4 weeks)" 
lab var z_tdif_key_attacks_s_nogov "Attacks in district (4 weeks)" 
lab var z_tdif_key_attacks_s_shelt "Attacks in district (4 weeks)" 

gen z_tdif_other=1
lab var z_tdif_other "Attacks elsewhere (4 weeks)"
lab var z_tdif_other_attacks_s "Attacks elsewhere (4 weeks)"
lab var z_tdif_other_attacks_s_nogov "Attacks elsewhere (4 weeks)"
lab var z_tdif_other_attacks_s_shelt "Attacks elsewhere (4 weeks)"


lab var z_logpop "log(Population)" 
lab var city "City" 
lab var z_bip_pp "GDP per capita" 
lab var z_mf_ratio "Male-Female Ratio" 
lab var z_hom_r "Homicide rate" 
lab var z_count_day "Time"

lab var Col_w1 "NYE 2015 (W1)"
lab var Col_w2 "NYE 2015 (W2)"
lab var Col_w3 "NYE 2015 (W3)"
lab var Col_w4 "NYE 2015 (W4)"
lab var Col_negw1 "NYE 2015 (W-1)"

lab var Paris_Jan_15_w1 "Paris Jan 2015 (W1)"
lab var Paris_Jan_15_w2 "Paris Jan 2015 (W2)"
lab var Paris_Jan_15_w3 "Paris Jan 2015 (W3)"

lab var Essen_16_w1 "Essen 2016 (W1)"
lab var Essen_16_w2 "Essen 2016 (W2)"

lab var Wuerzburg_16_w1 "Würzburg/Ansbach 2016 (W1)"
lab var Wuerzburg_16_w2 "Würzburg/Ansbach 2016 (W2)"

lab var Brussels_16_w1 "Brussels/Glasgow 2016 (W1)"
lab var Brussels_16_w2 "Brussels/Glasgow 2016 (W2)"
lab var Brussels_16_w3 "Brussels/Glasgow 2016 (W3)"
lab var Brussels_16_w4 "Brussels/Glasgow 2016 (W4)"


lab var Brussels_14_w1 "Brussels 2014 (W1)"
lab var Copenhagen_15_w1 "Copenhagen 2015 (W1)"
lab var Paris_Apr_15_w1 "Paris Apr 2015 (W1)"
lab var StQuent_15_w1 "St Quentin 2015 (W1)"
lab var Paris_Nov_15_w1 "Paris Nov 2015 (W1)"
lab var Brussels_16_w1 "Brussels/Glasgow 2016 (W1)"
lab var Magnanville_16_w1 "Magnanville 2016 (W1)"
lab var Nice_16_w1 "Nice 2016 (W1)"
lab var Berlin_16_w1 "Berlin 2016 (W1)"
lab var Berlin_15_w1 "Berlin 2015 (W1)"
lab var StEtienne_16_w1 "St Etienne 2016 (W1)"


lab var days_Col "Time"

lab var nye "NYE"
lab var z_dist "Dist. from Cologne"
lab var attacks_s_p90 "Hostile"
lab var attacks_s_nogov_p90 "Hostile"
lab var attacks_s_shelt_p90 "Hostile"

lab var attacks_s_bin "Attacks (all)"
lab var attacks_s_nogov_bin "Attacks (non-gov)"
lab var attacks_s_shelt_bin "Attacks (at shelter)"
