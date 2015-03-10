/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;

/**
 * The RawRegionBeanDefinitionTest class is a test suite of test cases testing the contract and functionality
 * of the SDG RegionFactoryBean class, and specifically the specification of the GemFire Region DataPolicy,
 * when used as raw bean definition in Spring XML configuration meta-data.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.PartitionAttributesFactoryBean
 * @see org.springframework.data.gemfire.RegionAttributesFactoryBean
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.DataPolicy
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.6.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class RegionDefinitionUsingBeansNamespaceTest {

	@Resource(name = "Example")
	private Region<?, ?> example;

	@Test
	public void testExampleRegionBeanDefinitionConfiguration() {
		assertNotNull("The 'Example' Region was not properly configured and initialized!", example);
		assertEquals("Example", example.getName());
		assertEquals("/Example", example.getFullPath());
		assertNotNull(example.getAttributes());
		assertEquals(DataPolicy.PERSISTENT_PARTITION, example.getAttributes().getDataPolicy());
		assertTrue(example.getAttributes().getStatisticsEnabled());
		assertNotNull(example.getAttributes().getPartitionAttributes());
		assertEquals(1, example.getAttributes().getPartitionAttributes().getRedundantCopies());
		assertEquals(0, example.getAttributes().getPartitionAttributes().getRecoveryDelay());
	}

}
