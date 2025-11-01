#!/usr/bin/env bash
export counto=250
set -eux
hyperfine \
  --warmup 5 \
  --runs 20 \
  './name-generator.sh' \
  './name-generator.bash' \
  './name-generator.zsh' \
  './name-generator.js' \
  './name-generator-sync.js' \
  './name-generator.py'

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
