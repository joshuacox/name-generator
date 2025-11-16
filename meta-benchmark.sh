#!/usr/bin/env bash
: ${counto:=12}
export counto=${counto}
set -eux
meta_runnr () {
  ./benchmark.sh
}

main () {
export counto=1
meta_runnr
export counto=3
meta_runnr
export counto=5
meta_runnr
export counto=8
meta_runnr
export counto=11
meta_runnr
export counto=25
meta_runnr
export counto=50
export SPEED=fast
meta_runnr
export counto=100
meta_runnr
export counto=250
meta_runnr
export SPEED=faster
export counto=500
meta_runnr
export counto=1000
meta_runnr
export counto=10000
meta_runnr 
export SPEED=fastest
export counto=100000
meta_runnr 
export counto=1000000
meta_runnr 
export counto=10000000
meta_runnr 
}

time main

exit 0

# WIP
  # 'export counto={counto} ./name-generator.sh' \
  # 'export counto={counto} ./name-generator.bash' \
  # 'export counto={counto} ./name-generator.zsh' \
  # 'export counto={counto} ./name-generator.py'
  #--parameter-scan counto 100 1000 \
  #--parameter-step-size 100 \
  #'export ${counto} ${commands}'
  #--parameter-list commands ./name-generator.sh,./name-generator.bash,./name-generator.zsh,./name-generator.py \
  #--parameter-list commands ./name-generator.sh,./name-generator.bash,./name-generator.zsh,./name-generator.py \
