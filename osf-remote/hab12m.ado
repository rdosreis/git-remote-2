
program define hab12m
  version 8.0
* set trace on
* pause on

*** Sintaxe
  syntax , Lote(integer) Versao(integer) [V1]

*** Vars temporárias para execução da rotina
*  tempvar _use _rank _temp _sum _dec _concxw _wt _area

/*
*** Temporário
   di "Lote contains   |`lote'|"
   di "Versao contains |`versao'|"
   di "V1 contains |`v1'|"
*/


clear
local camin "c:\c2004\Hab 12M"
if "`v1'" == "v1" {
    local camin "c:\c2004\Hab 12M" 
}

* Vai para a pasta certa
local zeros=substr("000",1,3-length("`lote'"))
cd "`camin'\l`zeros'`lote'"

* Transforma os dados para Stata
!C:\Arquiv~1\StatTransfer6\st hab12m`lote'd?v`versao'.rec *.dta

use hab12m`lote'd2v`versao'.dta, clear
g obs=_n
sort nquest
g nsort=_n
save, replace

use hab12m`lote'd1v`versao'.dta, clear
g obs=_n
sort nquest
g nsort=_n
save, replace

capture compdta using hab12m`lote'd2v`versao', sort keep(min)
ren _id nsort
sort nsort
save temp, replace
use hab12m`lote'd1v`versao'.dta, clear
sort nsort
save,replace
use temp, clear
merge nsort using hab12m`lote'd1v`versao'.dta, keep(nquest obs) unique nokeep
drop  _compdta nsort _merge
order nquest obs
sort obs

set more off
set linesize 80
log using "VAL`lote'V`versao'.log", append

unab vars : _all

local N = _N
forvalues quest = 1/`N'  {
    
    display 
    display as text _dup(80) "-"
    display "OBS: " obs[`quest'] "              NQUEST: " nquest[`quest'] 
    display 

    foreach var of local vars {
        local comp = length("`var'")
        local var2 = substr("`var'",1,7)+"_"
        if "`var'"=="nquest" | "`var'"=="obs" | substr("`var'",`comp',1)=="_"  {
            continue
        } /*end if*/

        if missing(`var'[`quest']) & missing(`var2'[`quest']) {
            continue
        }

       *list `var' `var2' in `quest'  if  ~missing(`var'), clean noobs
        display %12s "`var': " `fmt' `var'[`quest'] " // " `fmt' `var2'[`quest']
    } /*end foreach*/
   
} /*end forvalues*/

di 
di 
log close
!del *.dta
clear


end
