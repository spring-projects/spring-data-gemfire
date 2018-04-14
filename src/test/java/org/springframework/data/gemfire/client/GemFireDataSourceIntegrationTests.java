/*
 * Copyright 2002-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.Pool;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for a GemFire DataSource.
 *
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
// TODO: merge with o.s.d.g.client.GemfireDataSoruceIntegrationTest
public class GemFireDataSourceIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper gemfireServer;

	@Autowired
	private ApplicationContext applicationContext;

	@BeforeClass
	public static void startGemFireServer() throws Exception {

		int availablePort = findAvailablePort();

		gemfireServer = run(ServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort),
			getServerContextXmlFileLocation(GemFireDataSourceIntegrationTests.class));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	private void assertRegion(Region<?, ?> region, String name, DataPolicy dataPolicy) {
		assertRegion(region, name, GemfireUtils.toRegionPath("simple"), dataPolicy);
	}

	private void assertRegion(Region<?, ?> region, String name, String fullPath, DataPolicy dataPolicy) {

		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(name);
		assertThat(region.getFullPath()).isEqualTo(fullPath);
		assertThat(region.getAttributes()).isNotNull();
		assertThat(region.getAttributes().getDataPolicy()).isEqualTo(dataPolicy);
	}

	@Test
	public void gemfireServerDataSourceCreated() {

		Pool pool = this.applicationContext.getBean("gemfirePool", Pool.class);

		assertThat(pool).isNotNull();
		assertThat(pool.getSubscriptionEnabled()).isTrue();

		List<String> regionList = Arrays.asList(this.applicationContext.getBeanNamesForType(Region.class));

		assertThat(regionList).hasSize(3);
		assertThat(regionList.contains("r1")).isTrue();
		assertThat(regionList.contains("r2")).isTrue();
		assertThat(regionList.contains("simple")).isTrue();

		Region<?, ?> simple = this.applicationContext.getBean("simple", Region.class);

		assertRegion(simple, "simple", DataPolicy.EMPTY);
	}

	@Test
	public void repositoryCreatedAndFunctional() {

		Person daveMathews = new Person(1L, "Dave", "Mathews");

		PersonRepository repository = this.applicationContext.getBean(PersonRepository.class);

		assertThat(repository.save(daveMathews)).isSameAs(daveMathews);

		Optional<Person> result = repository.findById(1L);

		assertThat(result.isPresent()).isTrue();
		assertThat(result.map(Person::getFirstname).orElse(null)).isEqualTo("Dave");
	}
}
