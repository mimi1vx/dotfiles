# Simplify my life 

function mkpac() {
    local name
    name=$(cblrpm spec $2 $1) || return 1 
    mkdir ${name}
    mv ${name}.spec ${name}/
    osc add ${name}
    cd ${name}
    osc vc -m "initial commit"
    osc ar
}

function obs_submit() {
    for i in *; do
        echo ${i}
        sleep 1
        cd ${i}
        osc sr ${*}
        cd ..
    done
}

    
