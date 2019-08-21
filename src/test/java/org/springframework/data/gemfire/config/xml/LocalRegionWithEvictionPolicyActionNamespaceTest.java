/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import javax.annotation.Resource;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.Scope;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The LocalRegionWithEvictionPolicyActionNamespaceTest class is a test suite of tests cases testing the Eviction Policy
 * Actions on Local Regions defined in a Spring context configuration file.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0.M1
 * @link https://jira.spring.io/browse/SGF-295
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class LocalRegionWithEvictionPolicyActionNamespaceTest {

	@Resource(name = "LocalDestroy")
	private Region localDestroyRegion;

	@Resource(name = "None")
	private Region noneRegion;

	@Resource(name = "Overflow")
	private Region overflowRegion;

	@Test
	public void testLocalRegionConfigurationWithEvictionPolicyActionSetToLocalDestroy() {
		assertNotNull("The 'LocalDestroy' Region was not properly configured and initialized!", localDestroyRegion);
		assertEquals("LocalDestroy", localDestroyRegion.getName());
		assertEquals("/LocalDestroy", localDestroyRegion.getFullPath());
		assertNotNull(localDestroyRegion.getAttributes());
		assertEquals(DataPolicy.NORMAL, localDestroyRegion.getAttributes().getDataPolicy());
		assertEquals(Scope.LOCAL, localDestroyRegion.getAttributes().getScope());
		assertNotNull(localDestroyRegion.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.LOCAL_DESTROY, localDestroyRegion.getAttributes().getEvictionAttributes().getAction());
	}

	@Test
	public void testLocalRegionConfigurationWithEvictionPolicyActionSetToNone() {
		assertNotNull("The 'None' Region was not properly configured and initialized!", noneRegion);
		assertEquals("None", noneRegion.getName());
		assertEquals("/None", noneRegion.getFullPath());
		assertNotNull(noneRegion.getAttributes());
		assertEquals(DataPolicy.NORMAL, noneRegion.getAttributes().getDataPolicy());
		assertEquals(Scope.LOCAL, noneRegion.getAttributes().getScope());
		assertNotNull(noneRegion.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.NONE, noneRegion.getAttributes().getEvictionAttributes().getAction());
	}

	@Test
	public void testLocalRegionConfigurationWithEvictionPolicyActionSetToOverflowToDisk() {
		assertNotNull("The 'Overflow' Region was not properly configured and initialized!", overflowRegion);
		assertEquals("Overflow", overflowRegion.getName());
		assertEquals("/Overflow", overflowRegion.getFullPath());
		assertNotNull(overflowRegion.getAttributes());
		assertEquals(DataPolicy.NORMAL, overflowRegion.getAttributes().getDataPolicy());
		assertEquals(Scope.LOCAL, overflowRegion.getAttributes().getScope());
		assertNotNull(overflowRegion.getAttributes().getEvictionAttributes());
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, overflowRegion.getAttributes().getEvictionAttributes().getAction());
	}

}
