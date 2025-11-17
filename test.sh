#!/usr/bin/env bash
: ${FILES:='name-generator.sh'}
: ${TEST:="make test"}
: ${MESSAGE:="/test ${TEST}"}
: "${counto:=24}"
#: "${MODEL:=ollama_chat/qwen2.5-coder:32b-base-fp16}"
#: "${MODEL:=ollama_chat/qwen3:32b-fp16}"
#: "${MODEL:=ollama_chat/llama4:scout}"
#: ${MODEL:=ollama_chat/gemma3:27b-it-fp16}
: ${MODEL:=ollama_chat/gpt-oss:120b}
#: ${MODEL:=ollama_chat/deepseek-r1:70b-llama-distill-q8_0}
#: ${MODEL:=ollama_chat/granite4:32b-a9b-h}
echo TEST
#$TEST
if [[ ! $? -eq 0 ]]; then
  aider \
    --config config/test.conf.yml \
    --model "ollama_chat/${MODEL}" \
    --test-cmd "${TEST}" \
    --message "${MESSAGE}" \
    --file "${FILES}"
fi
