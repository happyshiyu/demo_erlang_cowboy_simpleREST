#!/bin/bash

# Do it like Travis, to debug testman

set -o errexit
set -o errtrace
set -o nounset
set -o pipefail
set -x

declare -a YMLs Vs Ts
declare -i i=0

YMLs[$i]=.coveredci__start_reset_stop_docker.yml
Vs[$i]=0
Ts[$i]=0
((i+=1)) # Funny Bash thing: ((i++)) returns 1 only when i=0

YMLs[$i]=.coveredci__start_reset_stop.yml
Vs[$i]=0
Ts[$i]=0
((i++))

YMLs[$i]=.coveredci.yml
Vs[$i]=0
Ts[$i]=0
((i++))

YMLs[$i]=.coveredci__env.yml
Vs[$i]=0
Ts[$i]=0
((i++))

YMLs[$i]=.coveredci__doc_typo.yml
Vs[$i]=2
Ts[$i]=2
((i++))


testman=${TESTMAN:-testman}
$testman --version
rebar3 as prod release

for i in "${!YMLs[@]}"; do
    YML=${YMLs[$i]}
    V=${Vs[$i]}
    T=${Ts[$i]}

    if [[ $YML != .coveredci.yml ]]; then cp $YML .coveredci.yml; fi
    if [[ $YML == .coveredci__doc_typo.yml ]]; then
        sed -i s/consumes:/consume:/ priv/openapi2v1.yml
    fi

    set +e
    $testman validate; code=$?
    set -e
    [[ $code -eq $V ]]
    set +e
    $testman test; code=$?
    set -e
    [[ $code -eq $V ]]
    set +e

done
