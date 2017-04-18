#!/bin/bash

for run in {1..100}
do
  smac --scenario-file ./example_scenarios/branin/branin-scenario-ordinal.txt | grep "Estimated mean quality of final incumbent"
done

