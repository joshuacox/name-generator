#!/usr/bin/env bash
: ${counto:=12}
export counto=${counto}
set -eux
runnr () {
  hyperfine \
    --warmup 5 \
    --runs 20 \
    './name-generator' \
    './name-generator.sh' \
    './name-generator.bash' \
    './name-generator.zsh' \
    './name-generator.js' \
    './name-generator-sync.js' \
    './name-generator.py'
}
export counto=12
runnr
export counto=25
runnr
export counto=50
runnr
export counto=100
runnr
export counto=250
runnr
export counto=500
runnr
export counto=100
runnr

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
