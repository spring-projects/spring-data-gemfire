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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.apache.geode.cache.AttributesFactory;
import org.apache.geode.cache.RegionFactory;
import org.junit.Test;

/**
 * The IndexMaintenanceTypeTest class is a test suite of test cases testing the contract and functionality of the
 * IndexMaintenanceType enum type.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see IndexMaintenancePolicyType
 * @since 1.6.0
 */
public class IndexMaintenancePolicyTypeTest {

	@Test
	public void testDefault() {
		assertSame(IndexMaintenancePolicyType.SYNCHRONOUS, IndexMaintenancePolicyType.DEFAULT);
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(IndexMaintenancePolicyType.SYNCHRONOUS, IndexMaintenancePolicyType.valueOfIgnoreCase("SYNCHRONOUS"));
		assertEquals(IndexMaintenancePolicyType.SYNCHRONOUS, IndexMaintenancePolicyType.valueOfIgnoreCase("Synchronous"));
		assertEquals(IndexMaintenancePolicyType.SYNCHRONOUS, IndexMaintenancePolicyType.valueOfIgnoreCase("synchronous"));
		assertEquals(IndexMaintenancePolicyType.SYNCHRONOUS, IndexMaintenancePolicyType.valueOfIgnoreCase("SynCHrOnOus"));
		assertEquals(IndexMaintenancePolicyType.ASYNCHRONOUS, IndexMaintenancePolicyType.valueOfIgnoreCase("ASYNChronous"));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidValues() {
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase("synchronicity"));
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase("SYNC"));
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase("ASYNC"));
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase("Concurrent"));
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase("parallel"));
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase("  "));
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase(""));
		assertNull(IndexMaintenancePolicyType.valueOfIgnoreCase(null));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testAttributesFactorySetIndexMaintenanceAsynchronous() {
		AttributesFactory mockAttributesFactory = mock(AttributesFactory.class,
			"testAttributesFactorySetIndexMaintenanceAsynchronous");

		IndexMaintenancePolicyType.ASYNCHRONOUS.setIndexMaintenance(mockAttributesFactory);

		verify(mockAttributesFactory).setIndexMaintenanceSynchronous(eq(false));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testAttributesFactorySetIndexMaintenanceSynchronous() {
		AttributesFactory mockAttributesFactory = mock(AttributesFactory.class,
			"testAttributesFactorySetIndexMaintenanceAsynchronous");

		IndexMaintenancePolicyType.SYNCHRONOUS.setIndexMaintenance(mockAttributesFactory);

		verify(mockAttributesFactory).setIndexMaintenanceSynchronous(eq(true));
	}

	@Test
	public void testRegionFactorySetIndexMaintenanceAsynchronous() {
		RegionFactory mockRegionFactory = mock(RegionFactory.class, "testRegionFactorySetIndexMaintenanceAsynchronous");

		IndexMaintenancePolicyType.ASYNCHRONOUS.setIndexMaintenance(mockRegionFactory);

		verify(mockRegionFactory).setIndexMaintenanceSynchronous(eq(false));
	}

	@Test
	public void testRegionFactorySetIndexMaintenanceSynchronous() {
		RegionFactory mockRegionFactory = mock(RegionFactory.class, "testRegionFactorySetIndexMaintenanceSynchronous");

		IndexMaintenancePolicyType.SYNCHRONOUS.setIndexMaintenance(mockRegionFactory);

		verify(mockRegionFactory).setIndexMaintenanceSynchronous(eq(true));
	}

}
