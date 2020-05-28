********************************************************************************
* name: Assignment 5
* author: Weiyi Li (baylor) 
* description: assignment 5 do files
* last updated: may 27, 2020
********************************************************************************

regress y d unit i.time
outreg2 using "/Users/weiyi/Desktop/BU/summer 2020/assmen5.doc", word append

regress demeanedy demeanedd i.time
outreg2 using "/Users/weiyi/Desktop/BU/summer 2020/assmen5.doc", word append



********************************************************************************
* name: baker.do
* author: Weiyi Li adopting simulation.do file
* description: shows what a piece of shit TWFE is with differential timing and
*              heterogenous treatment effects over time
* last updated: may 27, 2020
********************************************************************************


clear
capture log close
set seed 20200403

* 1,000 firms (25 per state), 50 states, 4 groups (250 per groups), 40 years
* First create the states
set obs 50
gen state = _n

* Finally generate 1000 firms.  These are in each state. So 25 per state.
expand 25
bysort state: gen firms=runiform(0,5)
label variable firms "Unique firm fixed effect per state"

* Second create the years
expand 40
sort state
bysort state firms: gen year = _n
gen n=year
replace year = 1980 if year==1
replace year = 1981 if year==2
replace year = 1982 if year==3
replace year = 1983 if year==4
replace year = 1984 if year==5
replace year = 1985 if year==6
replace year = 1986 if year==7
replace year = 1987 if year==8
replace year = 1988 if year==9
replace year = 1989 if year==10
replace year = 1990 if year==11
replace year = 1991 if year==12
replace year = 1992 if year==13
replace year = 1993 if year==14
replace year = 1994 if year==15
replace year = 1995 if year==16
replace year = 1996 if year==17
replace year = 1997 if year==18
replace year = 1998 if year==19
replace year = 1999 if year==20
replace year = 2000 if year==21
replace year = 2001 if year==22
replace year = 2002 if year==23
replace year = 2003 if year==24
replace year = 2004 if year==25
replace year = 2005 if year==26
replace year = 2006 if year==27
replace year = 2007 if year==28
replace year = 2008 if year==29
replace year = 2009 if year==30
replace year = 2010 if year==31
replace year = 2011 if year==32
replace year = 2012 if year==33
replace year = 2013 if year==34
replace year = 2014 if year==35
replace year = 2015 if year==36
replace year = 2016 if year==37
replace year = 2017 if year==38
replace year = 2018 if year==39
replace year = 2019 if year==40

egen id =group(state firms)

* Add 250 firms treated every period with the treatment effect still 5 on average
* Cohort years 1986, 1992, 1998, 2004
su state, detail
gen     group=0
replace group=1 if state<=`r(p5)'
replace group=2 if state>`r(p5)' & state<=`r(p10)'
replace group=3 if state>`r(p50)' & state<=`r(p75)'
replace group=4 if state>`r(p75)' & `r(p90)'!=.
replace group=5 if state>`r(p90)' & `r(p90)'!=.
gen     treat_date = 0 
replace treat_date = 1986 if group==1
replace treat_date = 1992 if group==2
replace treat_date = 1998 if group==3
replace treat_date = 2004 if group==4
replace treat_date = 2010 if group==5
gen     treat=0  
replace treat=1 if group==1 & year>=1986
replace treat=1 if group==2 & year>=1992
replace treat=1 if group==3 & year>=1998
replace treat=1 if group==4 & year>=2004
replace treat=1 if group==5 & year>=2010

* Data generating process
gen e 	= rnormal(0,(0.5)^2)
gen te1 = rnormal(10,(0.2)^2) 
gen te2 = rnormal(8,(0.2)^2)
gen te3 = rnormal(6,(0.2)^2)
gen te4 = rnormal(4,(0.2)^2)
gen te5 = rnormal(2,(0.2)^2)
gen te = .

replace te = te1 if group == 1
replace te = te2 if group == 2
replace te = te3 if group == 3
replace te = te4 if group == 4
replace te = te5 if group == 5

summarize te
* generate constant treatment effects
gen y = firms + n + te*treat + e 

* Data generating process with heterogeneity over time
gen y2 = firms + n + treat*te*(year - treat_date + 1) + e 

* Estimation using TWFE - constant treatment effects
areg y i.year treat, a(id) robust 



* Create group dummies
gen	    g1=0
replace g1=1 if group==1

gen 	g2=0
replace g2=1 if group==2

gen 	g3=0
replace g3=1 if group==3

gen 	g4=0
replace g4=1 if group==4

gen 	g5=0
replace g5=1 if group==5

gen     time_til=year-treat_date
ta time_til, gen(dd)


net install ddtiming, from(https://tgoldring.com/code/)
areg y i.year treat, a(id) robust
ddtiming y treat, i(id) t(year)

logit g4 if ((year==2009) & ((g4==1) | (time_til<0)))
predict pg4_2009


gen ypost = y if year==2006
gen ypre = y if year==2003

* Generate Denominators of the weights

bysort year: egen g4_mean = mean(g4)
bysort year: egen g4_cont_2009mean = mean((1 - g1)*pg4_2009/(1 - pg4_2009))


* Get weights
gen w4= g4/g4_mean
gen w0 = ((1 - g4)*pg4_2009/(1 - pg4_2009))/g4_cont_2009mean

* Generate each component of the DID
egen att_11 = mean(w4*ypost)
egen att_10 = mean(w4*ypre)
egen att_01 =  mean(w0*ypost)
egen att_00 = mean(w0*ypre)

* Get the ATT(2004,2006)
gen att2004_2006 = (att_11-att_10) -(att_01 -att_00)
* ATT(2004,2006)=9.181719

** (2004,2007)
drop ypost ypre g4_mean g4_cont_2009mean w4 w0 att_*

gen ypost = y if year==2007
gen ypre = y if year==2003

* Generate Denominators of the weights

bysort year: egen g4_mean = mean(g4)
bysort year: egen g4_cont_2009mean = mean((1 - g1)*pg4_2009/(1 - pg4_2009))


* Get weights
gen w4= g4/g4_mean
gen w0 = ((1 - g4)*pg4_2009/(1 - pg4_2009))/g4_cont_2009mean

* Generate each component of the DID
egen att_11 = mean(w4*ypost)
egen att_10 = mean(w4*ypre)
egen att_01 =  mean(w0*ypost)
egen att_00 = mean(w0*ypre)

* Get the ATT(2004,2007)
gen att2004_2007 = (att_11-att_10) -(att_01 -att_00)
* ATT(2004,2007)=9.979418


* Q7
* Estimation using TWFE - heterogenous treatment effects over time
areg y2 i.year treat, a(id) robust 
outreg2 using "/Users/weiyi/Desktop/BU/summer 2020/assignment 5/Q7.doc", word append



