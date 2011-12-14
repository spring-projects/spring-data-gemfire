Spring Data GemFire
===================

The primary goal of the [Spring GemFire](http://www.springsource.org/spring-gemfire) project is to make it easier to build highly scalable Spring-powered applications using 
[GemFire](http://www.gemstone.com/products/gemfire) as a distributed [data management](http://www.springsource.com/products/data-management) platform.

# Examples

For examples on using the Spring GemFire, see the samples project.

# Getting Help


Read the main project [website](http://www.springsource.org/spring-gemfire) and the [User Guide](http://static.springsource.org/spring-gemfire/docs/current/reference/html/). Look at the source code and the [JavaDocs](http://static.springsource.org/spring-gemfire/docs/current/api/). For more detailed questions, use the [forum](http://forum.springsource.org/forumdisplay.php?f=80). If you are new to Spring as well as to Spring GemFire, look for information about [Spring projects](http://www.springsource.org/projects). 

Quick Start
-----------

For those in a hurry:

* Download the jar through

* Maven:

~~~~~ xml
<dependency>
  <groupId>org.springframework.data</groupId>
  <artifactId>spring-data-gemfire</artifactId>
  <version>${version}</version>
</dependency> 

<!-- used for nightly builds -->
<repository>
  <id>spring-maven-snapshot</id>
  <snapshots><enabled>true</enabled></snapshots>
  <name>Springframework Maven SNAPSHOT Repository</name>
  <url>http://maven.springframework.org/snapshot</url>
</repository> 

<!-- used for milestone/rc releases -->
<repository>
  <id>spring-maven-milestone</id>
  <name>Springframework Maven Milestone Repository</name>
  <url>http://maven.springframework.org/milestone</url>
</repository> 
~~~~~

* Gradle: 

~~~~~ groovy
repositories {
   mavenRepo name: "spring-snapshot", urls: "http://maven.springframework.org/snapshot"
   mavenRepo name: "spring-milestone", urls: "http://maven.springframework.org/milestone"
}

dependencies {
   compile "org.springframework.data:spring-data-gemfire:${version}"
}
~~~~~

Latest GA release is _1.1.0.RELEASE_  
Latest nightly build is _1.1.1.BUILD-SNAPSHOT_


* Configure a GemFire cache and Region (replicated, partitioned, client and so on):

~~~~~ xml
<beans xmlns="http://www.springframework.org/schema/beans"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns:gfe="http://www.springframework.org/schema/gemfire"
  xsi:schemaLocation="
    http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
    http://www.springframework.org/schema/gemfire http://www.springframework.org/schema/gemfire/spring-gemfire.xsd">

  <gfe:cache />
  
  <gfe:partitioned-region id="partition" copies="2" total-buckets="4">

  <bean id="gemfireTemplate" class="org.springframework.data.gemfire.GemfireTemplate" p:region-ref="someRegion"/>
</beans>
~~~~~

* Use the Region to read/write data:

~~~~~ java
region.put(Long.valueOf(1), new Person("Jane", "Smith"));
~~~~~

* Or/And `GemFireTemplate` to interact with GemFire:

~~~~~ java
template.query("person = 1");
~~~~~

# Building

Spring GemFire uses Gradle as its building system. To compile the project, simply type from the root folder

    gradlew

To generate IDE-specific files, use

    gradlew eclipse
 
or

    gradlew idea 
    
depending on your editor.

# Contributing


Here are some ways for you to get involved in the community:

* Get involved with the Spring community on the Spring Community Forums.  Please help out on the [forum](http://forum.springsource.org/forumdisplay.php?f=80) by responding to questions and joining the debate.
* Create [JIRA](https://jira.springframework.org/browse/SGF) tickets for bugs and new features and comment and vote on the ones that you are interested in.  
* Github is for social coding: if you want to write code, we encourage contributions through pull requests from [forks of this repository](http://help.github.com/forking/). If you want to contribute code this way, please reference a JIRA ticket as well covering the specific issue you are addressing.
* Watch for upcoming articles on Spring by [subscribing](http://www.springsource.org/node/feed) to springframework.org

Before we accept a non-trivial patch or pull request we will need you to sign the [contributor's agreement](https://support.springsource.com/spring_committer_signup).  Signing the contributor's agreement does not grant anyone commit rights to the main repository, but it does mean that we can accept your contributions, and you will get an author credit if we do.  Active contributors might be asked to join the core team, and given the ability to merge pull requests.
