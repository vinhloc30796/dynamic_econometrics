*----------------------*
* Dynamic Econometrics *
*    Research Paper    *
*----------------------*

clear all
set more off

cap log close

*log using researchpaper.txt, text replace

/*import excel "C:\Users\Kris Rama\Desktop\Econometrics\Dynamic Econometrics\Research Paper\FREDMD_march2019.xlsx", sheet("2019-03(1)") firstrow allstring
import excel "X:\My Downloads\FREDMD_march2019.xlsx", sheet("2019-03(1)") firstrow allstring

destring INDPRO T10YFFM, replace

gen daten = daten(sasdate, "DMY")
drop if daten > daten("200701","YM")
format daten %td*/

import fred INDPRO T10YFFM, daterange(1959-01-01 2006-12-31) aggregate(monthly) clear
freddescribe INDPRO T10YFFM, detail

describe
sum

gen date = mofd(daten)
format date %tm

keep date INDPRO T10YFFM
order date

tsset date

*--------*
* INDPRO *
*--------*

tsline INDPRO

gen ln.INDPRO = log(INDPRO)

dfuller INDPRO, lags(4) trend

varsoc d.INDPRO, maxlag(12)
dfuller d.INDPRO, lags(3)

corrgram d.ln.INDPRO
ac d.INDPRO
pac d.INDPRO

tokenize `varlist'
foreach var of local varlist{
	forvalues i=1/`lags'{
		//display "ADF(`i') of `var'"
		qui dfuller `var', constant lags(`i') 
 		local list storage(`i')
		estimates store adf_`i'_`var'
		}
	qui estimates stats `storage(`i')'
 	matrix s=r(S)
 	matrix criteria=J(`lags',2,.)
	forvalues i=1/`lags' {
		matrix criteria[`i',1]=s[`i',5]
		matrix criteria[`i',2]=s[`i',6]
	}
 if "`bic'" != "" {
 	matrix criteria = criteria[1...,2]
 	local crit  "BIC"
 	}
 else {
 	matrix criteria = criteria[1...,1]
 	local crit  "AIC"
 	}
}

mata: min_row(st_matrix("criteria"))
local min = min[1,1]
display " "
display in gr "Optimal lag by `crit' criteria = " `min'
dfuller `varlist', lag(`min') constant
end

mata:
	matrix function min_row(matrix A){
 	B = colmin(A)
  	C = B:==A
  	A2 = (0, 0)
  	maxindex(C, 1, C, A2)
  	st_matrix("min",C) 	
	}
end
	
arima ln.INDPRO, arima(1,1,0) //AR(1)
predict res, residuals
ac res
pac res

arima ln.INDPRO, arima(2,1,0) //AR(2)
predict res, residuals
ac res
pac res

arima lnINDPRO, arima(3,1,0) //AR(3)
predict res, residuals
ac res
pac res

varsoc d.INDPRO, maxlag(12)
dfuller d.INDPRO, lags(3)

varsoc d.lnINDPRO, maxlag(12)
dfuller d.lnINDPRO, lags(3)

*---------*
* T10YFFM *
*---------*

tsline T10YFFM

ac T10YFFM
pac T10YFFM

arima T10YFFM, arima(3,0,0)
predict res, residuals
ac res
pac res




