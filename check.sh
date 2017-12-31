#!/bin/bash

# Do it like Travis, to debug testman

set -o errexit
set -o errtrace
set -o nounset
set -o pipefail
set -o xtrace

declare -a YMLs Vs Ts
declare -i i=0

YMLs[$i]=.coveredci__start_reset_stop_docker.yml
Vs[$i]=0
Ts[$i]=0
((i+=1)) # Funny Bash thing: ((i++)) returns 1 only when i=0

YMLs[$i]=.coveredci__start_reset_stop.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.coveredci__start_reset_stop_json.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.coveredci__start_reset_stop_failing_script.yml
Vs[$i]=0
Ts[$i]=1
((i+=1))

YMLs[$i]=.coveredci.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.coveredci__env.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.coveredci__doc_typo.yml
Vs[$i]=2
Ts[$i]=2
((i+=1))

YMLs[$i]=.coveredci__doc_typo_json.yml
Vs[$i]=2
Ts[$i]=2
((i+=1))


testman=${TESTMAN:-testman}
$testman --version
rebar3 as prod release

check() {
    printf '\e[1;3m%s\e[0m\n' "$YML"
    if [[ $YML != .coveredci.yml ]]; then cp $YML .coveredci.yml; fi
    if [[ $YML == .coveredci__doc_typo.yml ]]; then
        sed -i s/consumes:/consume:/ priv/openapi2v1.yml
    fi
    if [[ $YML == .coveredci__doc_typo_json.yml ]]; then
        sed -i 's/"consumes":/"consume":/' priv/openapi2v1.json
    fi

    set +e
    $testman validate; code=$?
    set -e
    [[ $code -eq $V ]]
    set +e
    $testman test; code=$?
    set -e
    [[ $code -eq $T ]]
    set +e

    git checkout -- .coveredci.yml
    git checkout -- priv/openapi2v1.yml
    git checkout -- priv/openapi2v1.json

    [[ 0 -eq $(docker ps -q | wc -l) ]]
    ! curl --output /dev/null --silent --fail --head http://localhost:6773/api/1/items
}

YML=${YML:-}
for i in "${!YMLs[@]}"; do
    V=${Vs[$i]}
    T=${Ts[$i]}

    if [[ -z "$YML" ]]; then
        YML=${YMLs[$i]} V=$V T=$T check
    else
        if [[ $YML = ${YMLs[$i]} ]]; then
            YML=$YML V=$V T=$T check
        fi
    fi
done
