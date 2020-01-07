/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

/**
 * The JndiDataSourceTypeTest class is a test suite of test cases testing the contract and functionality
 * of the JndiDataSourceType enumerated values.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.JndiDataSourceType
 * @since 1.7.0
 */
public class JndiDataSourceTypeTest {

	@Test
	public void testNames() {
		assertEquals("ManagedDataSource", JndiDataSourceType.MANAGED.getName());
		assertEquals("PooledDataSource", JndiDataSourceType.POOLED.getName());
		assertEquals("SimpleDataSource", JndiDataSourceType.SIMPLE.getName());
		assertEquals("XAPooledDataSource", JndiDataSourceType.XA.getName());
	}

	@Test
	public void testValueOfIgnoreCase() {
		assertEquals(JndiDataSourceType.MANAGED, JndiDataSourceType.valueOfIgnoreCase("managedDataSource  "));
		assertEquals(JndiDataSourceType.MANAGED, JndiDataSourceType.valueOfIgnoreCase("   ManAGEd"));
		assertEquals(JndiDataSourceType.POOLED, JndiDataSourceType.valueOfIgnoreCase("POOLedDataSource"));
		assertEquals(JndiDataSourceType.POOLED, JndiDataSourceType.valueOfIgnoreCase("PoolED "));
		assertEquals(JndiDataSourceType.SIMPLE, JndiDataSourceType.valueOfIgnoreCase(" SIMPLEDATASOURCE"));
		assertEquals(JndiDataSourceType.SIMPLE, JndiDataSourceType.valueOfIgnoreCase(" SIMPLE "));
		assertEquals(JndiDataSourceType.XA, JndiDataSourceType.valueOfIgnoreCase(" xapooleddatasource  "));
		assertEquals(JndiDataSourceType.XA, JndiDataSourceType.valueOfIgnoreCase("  xa  "));
	}

	@Test
	public void testValueOfIgnoreCaseWithInvalidNames() {
		assertNull(JndiDataSourceType.valueOfIgnoreCase("ManageDataSource"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("ManagedDataSink"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("ManedDataSrc"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("PoolingDataSource"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("ComplexDataSource"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("SimplifiedDataSource"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("XA Pooled DataSource"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("X A"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("XADATASOURCE"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("XAPOOLED"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("XA POOLED"));
		assertNull(JndiDataSourceType.valueOfIgnoreCase("  "));
		assertNull(JndiDataSourceType.valueOfIgnoreCase(""));
		assertNull(JndiDataSourceType.valueOfIgnoreCase(null));
	}

}
