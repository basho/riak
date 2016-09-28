#!/usr/bin/env bash

set -o nounset
set -o errexit

declare -r build_status="$(mktemp)"
declare -r otp_name='OTP_R16B02_basho10'
declare -r otp_build_log_dir="$HOME/.kerl/builds/$otp_name"
declare -r otp_install_dir="$HOME/otp-basho"

function onexit
{
    rm -vf "$build_status"
}

trap onexit EXIT

function build_ticker
{
    local status="$(< $build_status)"
    while [[ $status == 'true' ]]
    do
        echo '------------------------------------------------------------------------------------------------------------------------------------------------'
        echo "$(date) building $otp_name ..."
        if ls $otp_build_log_dir/otp_build*.log > /dev/null
        then
            tail $otp_build_log_dir/otp_build*.log
        fi
        sleep 10
        status="$(< $build_status)"
    done
    echo '.'
}

if [[ -f $otp_install_dir/activate ]]
then
    echo "Found OTP installation at $otp_install_dir"
else
    export KERL_CONFIGURE_OPTIONS='--enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --without-odbc'
    rm -rf "$otp_install_dir"
    mkdir -p "$otp_install_dir"

    echo -n 'true' > "$build_status"
    build_ticker &
    kerl build git https://github.com/basho/otp.git "$otp_name" "$otp_name"
    echo -n 'false' > "$build_status"
    wait

    kerl install "$otp_name" "$otp_install_dir"
fi

exit 0
