#!/bin/bash

for run in {1..25}
do
  smac --validation false --scenario-file ./colors/color-scenario.txt | grep "SMAC's final incumbent"
done

