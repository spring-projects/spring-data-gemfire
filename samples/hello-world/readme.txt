======================
== Hello World Demo ==
======================

1. MOTIVATION

As the name implies, this is a simple demo that illustrates the configuration
and interaction with the GemFire through the Spring container.

The demo starts and configures the GemFire grid and open up a basic shell for
executing commands against the grid.
Multiple nodes can be started which will share and exchange information transparently.

2. BUILD AND DEPLOYMENT

This directory contains the source files.
For building, JDK 1.5+ are required

To build the sample, use the following command:

*nix/BSD OS:
# ../../gradlew -q

Windows OS:
# ..\..\gradlew -q

If you have Gradle installed and available in your classpath, you can simply type:
# gradle -q
