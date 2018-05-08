Spring Data for Pivotal GemFire
===============================

The primary goal of the [Spring Data for Pivotal GemFire](http://projects.spring.io/spring-data-gemfire) project
is to make it easier to build highly scalable, _Spring_ powered applications using [Pivotal GemFire](https://pivotal.io/pivotal-gemfire)
as the underlying distributed, in-memory data management platform.

# Examples

For examples on using the _Spring Data for Pivotal GemFire_, see the
[spring-gemfire-examples](https://github.com/SpringSource/spring-gemfire-examples) project.

# Getting Help

Read the main project [website](http://projects.spring.io/spring-data-gemfire/) along with
the [User Guide](http://docs.spring.io/spring-data-gemfire/docs/current/reference/html/).

Look at the source code and the [JavaDocs](http://docs.spring.io/spring-data-gemfire/docs/current/api/).

For more detailed questions, visit [_StackOverflow_](https://stackoverflow.com/questions/tagged/spring-data-gemfire).

If you are new to _Spring_ as well as _Spring Data for Pivotal GemFire_, look for information about
[_Spring_ projects](http://spring.io/projects).

Quick Start
-----------

For developers in a hurry, you can download the JAR using:

* Maven:

~~~~~ xml
<dependency>
  <groupId>org.springframework.data</groupId>
  <artifactId>spring-data-gemfire</artifactId>
  <version>${version}</version>
</dependency>

<!-- nightly builds -->
<repository>
  <id>spring-maven-snapshot</id>
  <name>Spring Maven SNAPSHOT Repository</name>
  <url>http://repo.spring.io/snapshot</url>
  <snapshots><enabled>true</enabled></snapshots>
</repository>

<!-- milestones/release candidates-->
<repository>
  <id>spring-maven-milestone</id>
  <name>Spring Maven Milestone Repository</name>
  <url>http://repo.spring.io/milestone</url>
</repository>
~~~~~

* Gradle:

~~~~~ groovy
repositories {
   mavenRepo name: "spring-snapshot", urls: "http://repo.spring.io/snapshot"
   mavenRepo name: "spring-milestone", urls: "http://repo.spring.io/milestone"
   mavenRepo name: "spring-plugins" , urls: "http://repo.spring.io/plugins-release"
}

dependencies {
   compile "org.springframework.data:spring-data-geode:${version}"
}
~~~~~

* Configure a Pivotal GemFire cache and Region (REPLICATE, PARTITION and so on):

~~~~~ xml
<beans xmlns="http://www.springframework.org/schema/beans"
  xmlns:gfe="http://www.springframework.org/schema/gemfire"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance
  xsi:schemaLocation="
    http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
    http://www.springframework.org/schema/gemfire http://www.springframework.org/schema/gemfire/spring-gemfire.xsd">

  <gfe:cache/>

  <gfe:partitioned-region id="ExampleRegion" copies="2">

  <bean id="gemfireTemplate" class="org.springframework.data.gemfire.GemfireTemplate" p:region-ref="ExampleRegion"/>
</beans>
~~~~~

* Use the Region to read/write data:

~~~~~ java
region.put(Long.valueOf(1), new Person("Jon", "Doe"));
~~~~~

* And/Or `GemFireTemplate` to interact with Pivotal GemFire:

~~~~~ java
template.query("person = 1");
~~~~~

# Building

_Spring Data for Pivotal GemFire_ uses Maven as its build system. To compile the project, simply type
the following Maven command from the root folder:

    mvn clean install

# Contributing


Here are some ways for you to get involved in the community:

* Get involved with the _Spring_ community on the _Spring_ Community Forums (_StackOverflow_).
Please help out on the [forum](https://stackoverflow.com/questions/tagged/spring-data-gemfire)
by responding to questions and joining the debate.
* Create [JIRA](https://jira.spring.io/browse/SGF) tickets for bugs and new features and comment and vote on the bugs
you are interested in.
* GitHub is for social coding. If you want to write code, we encourage contributions through pull requests
from [forks of this repository](http://help.github.com/forking/). If you want to contribute code this way,
please reference a JIRA ticket as well covering the specific issue you are addressing.
* Watch for upcoming articles on _Spring_ by [subscribing](http://spring.io/blog) to spring.io.

Before we accept a non-trivial patch or pull request we will need you to
[sign the Contributor License Agreement](https://cla.pivotal.io/sign/spring). Signing the contributor’s agreement
does not grant anyone commit rights to the main repository, but it does mean that we can accept your contributions,
and you will get an author credit if we do. If you forget to do so, you'll be reminded when you submit a pull request.
Active contributors might be asked to join the core team, and given the ability to merge pull requests.
