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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * The IndexMaintenanceTypeTest class is a test suite of test cases testing the contract and functionality of the
 * IndexMaintenanceType enum type.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.IndexMaintenanceType
 * @since 1.6.0
 */
public class IndexMaintenanceTypeTest {

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(IndexMaintenanceType.SYNCHRONOUS, IndexMaintenanceType.valueOfIgnoreCase("SYNCHRONOUS"));
		assertEquals(IndexMaintenanceType.SYNCHRONOUS, IndexMaintenanceType.valueOfIgnoreCase("Synchronous"));
		assertEquals(IndexMaintenanceType.SYNCHRONOUS, IndexMaintenanceType.valueOfIgnoreCase("synchronous"));
		assertEquals(IndexMaintenanceType.SYNCHRONOUS, IndexMaintenanceType.valueOfIgnoreCase("SynCHrOnOus"));
		assertEquals(IndexMaintenanceType.ASYNCHRONOUS, IndexMaintenanceType.valueOfIgnoreCase("ASYNChronous"));
	}

	@Test
	public void testValueOfIgnoreCaseIsNull() {
		assertNull(IndexMaintenanceType.valueOfIgnoreCase("synchronicity"));
		assertNull(IndexMaintenanceType.valueOfIgnoreCase("SYNC"));
		assertNull(IndexMaintenanceType.valueOfIgnoreCase("ASYNC"));
		assertNull(IndexMaintenanceType.valueOfIgnoreCase("CONCURRENT"));
		assertNull(IndexMaintenanceType.valueOfIgnoreCase("parallel"));
		assertNull(IndexMaintenanceType.valueOfIgnoreCase("  "));
		assertNull(IndexMaintenanceType.valueOfIgnoreCase(""));
		assertNull(IndexMaintenanceType.valueOfIgnoreCase(null));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testAttributesFactorySetIndexMaintenanceAsynchronous() {
		AttributesFactory mockAttributesFactory = mock(AttributesFactory.class,
			"testAttributesFactorySetIndexMaintenanceAsynchronous");

		IndexMaintenanceType.ASYNCHRONOUS.setIndexMaintenance(mockAttributesFactory);

		verify(mockAttributesFactory).setIndexMaintenanceSynchronous(eq(false));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testAttributesFactorySetIndexMaintenanceSynchronous() {
		AttributesFactory mockAttributesFactory = mock(AttributesFactory.class,
			"testAttributesFactorySetIndexMaintenanceAsynchronous");

		IndexMaintenanceType.SYNCHRONOUS.setIndexMaintenance(mockAttributesFactory);

		verify(mockAttributesFactory).setIndexMaintenanceSynchronous(eq(true));
	}

	@Test
	public void testRegionFactorySetIndexMaintenanceAsynchronous() {
		RegionFactory mockRegionFactory = mock(RegionFactory.class, "testRegionFactorySetIndexMaintenanceAsynchronous");

		IndexMaintenanceType.ASYNCHRONOUS.setIndexMaintenance(mockRegionFactory);

		verify(mockRegionFactory).setIndexMaintenanceSynchronous(eq(false));
	}

	@Test
	public void testRegionFactorySetIndexMaintenanceSynchronous() {
		RegionFactory mockRegionFactory = mock(RegionFactory.class, "testRegionFactorySetIndexMaintenanceSynchronous");

		IndexMaintenanceType.SYNCHRONOUS.setIndexMaintenance(mockRegionFactory);

		verify(mockRegionFactory).setIndexMaintenanceSynchronous(eq(true));
	}

}
