#!/bin/bash

scalac ColorScheme.scala
rm -r smac/colors/com
mv com smac/colors
