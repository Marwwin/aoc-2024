#!/bin/bash

# Check if a day number is provided
if [ $# -eq 0 ]; then
    echo "Error: Please provide a day number."
    echo "Usage: $0 <day_number>"
    exit 1
fi

DAY=$1

BASE_PATH="./src/aoc_2024/days/day${DAY}"
TEMPLATE_PATH="./src/aoc_2024/day-template.clj"
SOLUTION_PATH="${BASE_PATH}/solution.clj"

mkdir -p "${BASE_PATH}"

touch "${BASE_PATH}/input"
touch "${BASE_PATH}/test"

sed "1c(ns aoc-2024.days.day${DAY}.solution" "${TEMPLATE_PATH}" > "${SOLUTION_PATH}"

echo "Created directories and solution file for Day ${DAY}"
