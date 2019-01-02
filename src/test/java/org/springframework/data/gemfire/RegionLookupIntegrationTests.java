/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Optional;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionExistsException;
import org.apache.geode.cache.Scope;
import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.fork.SpringContainerProcess;

/**
 * The RegionLookupIntegrationTests class is a test suite of test cases testing the lookup functionality for various
 * peer Region types.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see SpringContainerProcess
 * @see org.apache.geode.cache.Region
 * @since 1.4.0
 * @link https://jira.spring.io/browse/SGF-204
 */
// TODO: slow test; can this test use mocks?
public class RegionLookupIntegrationTests {

	private void assertNoRegionLookup(String configLocation) {

		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = createApplicationContext(configLocation);
			fail("Spring ApplicationContext should have thrown a BeanCreationException caused by a RegionExistsException!");
		}
		catch (BeanCreationException expected) {

			assertTrue(expected.getMessage(), expected.getCause() instanceof RegionExistsException);

			throw (RegionExistsException) expected.getCause();
		}
		finally {
			closeApplicationContext(applicationContext);
		}
	}

	private ConfigurableApplicationContext createApplicationContext(String configLocation) {
		return new ClassPathXmlApplicationContext(configLocation);
	}

	private void closeApplicationContext(ConfigurableApplicationContext applicationContext) {
		Optional.ofNullable(applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	@Test
	public void testAllowRegionBeanDefinitionOverrides() {

		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = createApplicationContext(
				"/org/springframework/data/gemfire/allowRegionBeanDefinitionOverridesTest.xml");

			assertNotNull(applicationContext);
			assertTrue(applicationContext.containsBean("regionOne"));

			Region appDataRegion = applicationContext.getBean("regionOne", Region.class);

			assertNotNull(appDataRegion);
			assertEquals("AppDataRegion", appDataRegion.getName());
			assertEquals("/AppDataRegion", appDataRegion.getFullPath());
			assertNotNull(appDataRegion.getAttributes());
			assertEquals(DataPolicy.PERSISTENT_REPLICATE, appDataRegion.getAttributes().getDataPolicy());
			assertFalse(appDataRegion.getAttributes().getMulticastEnabled());
			assertEquals(Scope.DISTRIBUTED_ACK, appDataRegion.getAttributes().getScope());
			assertEquals(101, appDataRegion.getAttributes().getInitialCapacity());
			assertEquals(new Float(0.85f), new Float(appDataRegion.getAttributes().getLoadFactor()));
			assertTrue(appDataRegion.getAttributes().getCloningEnabled());
			assertTrue(appDataRegion.getAttributes().getConcurrencyChecksEnabled());
			assertEquals(Integer.class, appDataRegion.getAttributes().getKeyConstraint());
			assertEquals(String.class, appDataRegion.getAttributes().getValueConstraint());
		}
		finally {
			closeApplicationContext(applicationContext);
		}
	}

	@Test(expected = RegionExistsException.class)
	public void testNoDuplicateRegionDefinitions() {
		assertNoRegionLookup("/org/springframework/data/gemfire/noDuplicateRegionDefinitionsTest.xml");
	}

	@Test(expected = RegionExistsException.class)
	public void testNoClientRegionLookups() {
		assertNoRegionLookup("/org/springframework/data/gemfire/noClientRegionLookupTest.xml");
	}

	@Test(expected = RegionExistsException.class)
	public void testNoClientSubRegionLookups() {
		assertNoRegionLookup("/org/springframework/data/gemfire/noClientSubRegionLookupTest.xml");
	}

	@Test(expected = RegionExistsException.class)
	public void testNoLocalRegionLookups() {
		assertNoRegionLookup("/org/springframework/data/gemfire/noLocalRegionLookupTest.xml");
	}

	@Test(expected = RegionExistsException.class)
	public void testNoPartitionRegionLookups() {
		assertNoRegionLookup("/org/springframework/data/gemfire/noPartitionRegionLookupTest.xml");
	}

	@Test(expected = RegionExistsException.class)
	public void testNoReplicateRegionLookups() {
		assertNoRegionLookup("/org/springframework/data/gemfire/noReplicateRegionLookupTest.xml");
	}

	@Test(expected = RegionExistsException.class)
	public void testNoSubRegionLookups() {
		assertNoRegionLookup("/org/springframework/data/gemfire/noSubRegionLookupTest.xml");
	}

	@Test
	public void testEnableRegionLookups() {

		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = createApplicationContext("/org/springframework/data/gemfire/enableRegionLookupsTest.xml");

			assertNotNull(applicationContext);
			assertTrue(applicationContext.containsBean("NativeLocalRegion"));
			assertTrue(applicationContext.containsBean("NativePartitionRegion"));
			assertTrue(applicationContext.containsBean("NativeReplicateRegion"));
			assertTrue(applicationContext.containsBean("NativeParentRegion"));
			assertTrue(applicationContext.containsBean("/NativeParentRegion/NativeChildRegion"));
			assertTrue(applicationContext.containsBean("SpringReplicateRegion"));

			Region nativeLocalRegion = applicationContext.getBean("NativeLocalRegion", Region.class);

			assertNotNull(nativeLocalRegion);
			assertEquals("NativeLocalRegion", nativeLocalRegion.getName());
			assertEquals("/NativeLocalRegion", nativeLocalRegion.getFullPath());
			assertNotNull(nativeLocalRegion.getAttributes());
			assertEquals(DataPolicy.NORMAL, nativeLocalRegion.getAttributes().getDataPolicy());
			assertFalse(nativeLocalRegion.getAttributes().getCloningEnabled());
			assertFalse(nativeLocalRegion.getAttributes().getConcurrencyChecksEnabled());
			assertEquals(80, nativeLocalRegion.getAttributes().getConcurrencyLevel());
			assertEquals(101, nativeLocalRegion.getAttributes().getInitialCapacity());
			assertEquals(Integer.class, nativeLocalRegion.getAttributes().getKeyConstraint());
			assertEquals(new Float(0.95f), new Float(nativeLocalRegion.getAttributes().getLoadFactor()));
			assertEquals(String.class, nativeLocalRegion.getAttributes().getValueConstraint());

			Region nativePartitionRegion = applicationContext.getBean("NativePartitionRegion", Region.class);

			assertNotNull(nativePartitionRegion);
			assertEquals("NativePartitionRegion", nativePartitionRegion.getName());
			assertEquals("/NativePartitionRegion", nativePartitionRegion.getFullPath());
			assertNotNull(nativePartitionRegion.getAttributes());
			assertEquals(DataPolicy.PERSISTENT_PARTITION, nativePartitionRegion.getAttributes().getDataPolicy());
			assertTrue(nativePartitionRegion.getAttributes().getCloningEnabled());
			assertTrue(nativePartitionRegion.getAttributes().getConcurrencyChecksEnabled());
			assertEquals(40, nativePartitionRegion.getAttributes().getConcurrencyLevel());
			assertEquals(51, nativePartitionRegion.getAttributes().getInitialCapacity());
			assertEquals(Integer.class, nativePartitionRegion.getAttributes().getKeyConstraint());
			assertEquals(new Float(0.85f), new Float(nativePartitionRegion.getAttributes().getLoadFactor()));
			assertFalse(nativePartitionRegion.getAttributes().getMulticastEnabled());
			assertEquals(String.class, nativePartitionRegion.getAttributes().getValueConstraint());

			Region nativeReplicateRegion = applicationContext.getBean("NativeReplicateRegion", Region.class);

			assertNotNull(nativeReplicateRegion);
			assertEquals("NativeReplicateRegion", nativeReplicateRegion.getName());
			assertEquals("/NativeReplicateRegion", nativeReplicateRegion.getFullPath());
			assertNotNull(nativeReplicateRegion.getAttributes());
			assertEquals(DataPolicy.PERSISTENT_REPLICATE, nativeReplicateRegion.getAttributes().getDataPolicy());
			assertFalse(nativeReplicateRegion.getAttributes().getCloningEnabled());
			assertTrue(nativeReplicateRegion.getAttributes().getConcurrencyChecksEnabled());
			assertEquals(23, nativeReplicateRegion.getAttributes().getInitialCapacity());
			assertEquals(new Float(0.75f), new Float(nativeReplicateRegion.getAttributes().getLoadFactor()));
			assertEquals(Integer.class, nativeReplicateRegion.getAttributes().getKeyConstraint());
			assertFalse(nativeReplicateRegion.getAttributes().getMulticastEnabled());
			assertEquals(Scope.DISTRIBUTED_NO_ACK, nativeReplicateRegion.getAttributes().getScope());
			assertEquals(String.class, nativeReplicateRegion.getAttributes().getValueConstraint());

			Region nativeChildRegion = applicationContext.getBean("/NativeParentRegion/NativeChildRegion", Region.class);

			assertNotNull(nativeChildRegion);
			assertEquals("NativeChildRegion", nativeChildRegion.getName());
			assertEquals("/NativeParentRegion/NativeChildRegion", nativeChildRegion.getFullPath());
			assertNotNull(nativeChildRegion.getAttributes());
			assertEquals(DataPolicy.REPLICATE, nativeChildRegion.getAttributes().getDataPolicy());

			Region springReplicateRegion = applicationContext.getBean("SpringReplicateRegion", Region.class);

			assertNotNull(springReplicateRegion);
			assertEquals("SpringReplicateRegion", springReplicateRegion.getName());
			assertEquals("/SpringReplicateRegion", springReplicateRegion.getFullPath());
			assertNotNull(springReplicateRegion.getAttributes());
			assertEquals(DataPolicy.REPLICATE, springReplicateRegion.getAttributes().getDataPolicy());
		}
		finally {
			closeApplicationContext(applicationContext);
		}
	}

	@Test
	public void testEnableClientRegionLookups() {

		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = createApplicationContext("/org/springframework/data/gemfire/enableClientRegionLookupsTest.xml");

			assertNotNull(applicationContext);
			assertTrue(applicationContext.containsBean("NativeClientRegion"));
			assertTrue(applicationContext.containsBean("NativeClientParentRegion"));
			assertTrue(applicationContext.containsBean("/NativeClientParentRegion/NativeClientChildRegion"));

			Region nativeClientRegion = applicationContext.getBean("NativeClientRegion", Region.class);

			assertNotNull(nativeClientRegion);
			assertEquals("NativeClientRegion", nativeClientRegion.getName());
			assertEquals("/NativeClientRegion", nativeClientRegion.getFullPath());
			assertNotNull(nativeClientRegion.getAttributes());
			assertFalse(nativeClientRegion.getAttributes().getCloningEnabled());
			assertEquals(DataPolicy.NORMAL, nativeClientRegion.getAttributes().getDataPolicy());

			Region nativeClientChildRegion = applicationContext.getBean("/NativeClientParentRegion/NativeClientChildRegion",
				Region.class);

			assertNotNull(nativeClientChildRegion);
			assertEquals("NativeClientChildRegion", nativeClientChildRegion.getName());
			assertEquals("/NativeClientParentRegion/NativeClientChildRegion", nativeClientChildRegion.getFullPath());
			assertNotNull(nativeClientChildRegion.getAttributes());
			assertEquals(DataPolicy.NORMAL, nativeClientChildRegion.getAttributes().getDataPolicy());
		}
		finally {
			closeApplicationContext(applicationContext);
		}
	}
}
