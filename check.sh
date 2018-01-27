#!/bin/bash -eux

# Bend it like Travis, to debug monkey

set -o errtrace
set -o pipefail

declare -a YMLs Vs Ts
declare -i i=0

YMLs[$i]=.fuzzymonkey__start_reset_stop_docker.yml
Vs[$i]=0
Ts[$i]=0
((i+=1)) # Funny Bash thing: ((i++)) returns 1 only when i=0

YMLs[$i]=.fuzzymonkey__start_reset_stop.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.fuzzymonkey__start_reset_stop_json.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.fuzzymonkey__start_reset_stop_failing_script.yml
Vs[$i]=0
Ts[$i]=7
((i+=1))

YMLs[$i]=.fuzzymonkey.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.fuzzymonkey__env.yml
Vs[$i]=0
Ts[$i]=0
((i+=1))

YMLs[$i]=.fuzzymonkey__doc_typo.yml
Vs[$i]=2
Ts[$i]=2
((i+=1))

YMLs[$i]=.fuzzymonkey__doc_typo_json.yml
Vs[$i]=2
Ts[$i]=2
((i+=1))


monkey=${MONKEY:-monkey}
$monkey --version
rebar3 as prod release
branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
info() {
    printf '\e[1;3m%s\e[0m\n' "$*"
}

setup() {
    info $branch $YML V=$V T=$T
    if [[ $YML != .fuzzymonkey.yml ]]; then cp $YML .fuzzymonkey.yml; fi
    if [[ $YML == .fuzzymonkey__doc_typo.yml ]]; then
        sed -i s/consumes:/consume:/ priv/openapi2v1.yml
    fi
    if [[ $YML == .fuzzymonkey__doc_typo_json.yml ]]; then
        sed -i 's/"consumes":/"consume":/' priv/openapi2v1.json
    fi
}

check() {
    set +e
    $monkey validate; code=$?
    set -e
    if  [[ $code -ne $V ]]; then
        info $branch $YML V=$V T=$T ...failed
        return 1
    fi
    set +e
    $monkey fuzz; code=$?
    set -e
    if  [[ $code -ne $T ]]; then
        info $branch $YML V=$V T=$T ...failed
        return 1
    fi
    info $branch $YML V=$V T=$T ...passed
    return 0
}

cleanup() {
    git checkout -- .fuzzymonkey.yml
    git checkout -- priv/openapi2v1.yml
    git checkout -- priv/openapi2v1.json

    if docker ps | grep my_image; then
        docker stop --timeout 0 $(docker ps | grep my_image | awk '{print $1;}')
    fi
    if ! curl --output /dev/null --silent --fail --head http://localhost:6773/api/1/items; then
        info Some instance is still running somewhere!
        return 1
    fi
}

errors=0
YML=${YML:-}
for i in "${!YMLs[@]}"; do
    V=${Vs[$i]}
    T=${Ts[$i]}

    if [[ -z "$YML" ]]; then
        YML=${YMLs[$i]} V=$V T=$T setup
        YML=${YMLs[$i]} V=$V T=$T check || ((errors+=1))
        YML=${YMLs[$i]} V=$V T=$T cleanup
    else
        if [[ $YML = ${YMLs[$i]} ]]; then
            YML=$YML V=$V T=$T setup
            YML=$YML V=$V T=$T check || ((errors+=1))
            YML=$YML V=$V T=$T cleanup
            break
        fi
    fi
done
exit $errors
