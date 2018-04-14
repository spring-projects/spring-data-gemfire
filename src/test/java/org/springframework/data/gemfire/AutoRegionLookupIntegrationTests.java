/*
 * Copyright 2010-2018 the original author or authors.
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

import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests to test the contract and functionality of Spring Data GemFire's Auto Region Lookup functionality.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class AutoRegionLookupIntegrationTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void testAutoRegionLookup() {
		assertTrue(applicationContext.containsBean("SpringPartitionedRegion"));
		assertTrue(applicationContext.containsBean("SpringReplicateParent"));
		assertTrue(applicationContext.containsBean("/SpringReplicateParent/SpringReplicateChild"));
		assertTrue(applicationContext.containsBean("NativePartitionedRegion"));
		assertTrue(applicationContext.containsBean("NativeReplicateParent"));
		assertTrue(applicationContext.containsBean("/NativeReplicateParent/NativeReplicateChild"));
		assertTrue(applicationContext.containsBean("/NativeReplicateParent/NativeReplicateChild/NativeReplicateGrandchild"));
	}
}
