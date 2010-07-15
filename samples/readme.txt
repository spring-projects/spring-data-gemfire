Spring GemFire Samples
----------------------

This folder contains various various demo applications and samples for Spring GemFire.

Please see each folder for detailed instructions (readme.txt).

As a general rule, each demo provides an integration tests that bootstraps
the GemFire platform, installs the demo and its dependencies and interacts with 
the application.

SAMPLES OVERVIEW
----------------

* hello-world
A simple demo for configuring and interacting with the GemFire grid.


BUILDING AND DEPLOYMENT
-----------------------

All demos require Maven 2.0.7+ and JDK 1.5+.

Each module should be run from its top folder using maven:

# mvn clean install