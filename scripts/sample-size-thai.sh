### sample size calculation thaise ###
### May 7th, 2023 ###
### Lucas Helal ###

install.packages("epiR")
library(epiR)

## sample size 1 - at 80% power, and 95% CI ##

sample_size_1 <-  epi.ssxsectn(N = NA, pdexp1 = 0.65, pdexp0 = 0.35, pexp = NA, n = NA, power = 0.80, r = 1, 
             design = 1, sided.test = 2, nfractional = FALSE, conf.level = 0.95)

sample_size_1

## sample size 2 - at 80% power, and 95% CI, at 0.6/0.4 ##

sample_size_2 <-  epi.ssxsectn(N = NA, pdexp1 = 0.60, pdexp0 = 0.40, pexp = NA, n = NA, power = 0.80, r = 1, 
                               design = 1, sided.test = 2, nfractional = FALSE, conf.level = 0.95)
sample_size_2

## sample size 3 - at 90% power, and 95% CI, at 0.65/0.35 ##

sample_size_3 <-  epi.ssxsectn(N = NA, pdexp1 = 0.65, pdexp0 = 0.35, pexp = NA, n = NA, power = 0.90, r = 1, 
                               design = 1, sided.test = 2, nfractional = FALSE, conf.level = 0.95)
sample_size_3

## final sample size - sample size 3 plus 20% ##

sample_size_final <- 114*1.2

sample_size_final 
# n = 136 
# nA = 68
# nB = 68

# > sample_size_3
$n.total
[1] 114

$n.exp1
[1] 57

$n.exp0
[1] 57

$power
[1] 0.9

$pr
[1] 1.857143

$or
[1] 3.44898

### continuous ###

install.packages("epiR")
library(epiR)

epi.sssimpleestb(N = 875, Py = 0.70, epsilon = 0.60, error = "relative", se = 1, sp = 1, nfractional = FALSE, conf.level = 0.95)


### sample size 1 ###

/usr/bin/env bash
set -eu

prefix="/usr/local"

if [ "${PREFIX:-}" != "" ] ; then
prefix=${PREFIX:-}
elif [ "${BOXEN_HOME:-}" != "" ] ; then
prefix=${BOXEN_HOME:-}
fi

mkdir -p $prefix/bin
rm -rf $prefix/bin/git-lfs*
  
  pushd "$( dirname "${BASH_SOURCE[0]}" )" > /dev/null
for g in git*; do
install $g "$prefix/bin/$g"
done
popd > /dev/null

