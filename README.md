# ContainAnt

Companion repository for the article
"On Dependency Injection and Programming by Optimization".

**ContainAnt** is an IoC container on steroids: by placing advanced search-based
heuristics at the heart of dependency injection, it can automatically configure
components for optimal performance and behavior.

## Usage

To reproduce the results of the article:

Make sure that you have the right Java version (1.8.0_121) by running
`java -version`. You will need a recent version of the 
[sbt](http://www.scala-sbt.org/) build tool. Executing `sbt run` will perform
the experiments and generate a report on the standard output.

## Gallery: Adaptive Syntax Highlighting

In the third case study, we use **ContainAnt** to automatically generate and
instantiate readable syntax highlighting schemes for any pre-determined
background color. Here's a gallery of the generated color schemes:

![Blue Pastelized Scheme](/res/cs3/0.png?raw=true "Blue Pastelized Scheme")
![Dark Purple Pastelized Scheme](/res/cs3/1.png?raw=true "Dark Purple Pastelized Scheme")
![Purple Pastelized Scheme](/res/cs3/2.png?raw=true "Purple Pastelized Scheme")
![Bright Purple Pastelized Scheme](/res/cs3/3.png?raw=true "Bright Purple Pastelized Scheme")
