/*
 * Copyright 2011-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;

import javax.annotation.Resource;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.query.Index;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The IndexNamespaceTest is a test suite of test cases testing the functionality of GemFire Index creation using
 * the Spring Data GemFire XML namespace (XSD).
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.data.gemfire.config.xml.IndexParser
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="index-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings({ "deprecation", "unused" })
public class IndexNamespaceTest {

	private static final String TEST_REGION_NAME = "IndexedRegion";

	@Resource(name = "simple")
	private Index simple;

	@Resource(name = "complex")
	private Index complex;

	@Test
	public void testBasicIndex() throws Exception {
		assertEquals("simple", simple.getName());
		assertEquals("status", simple.getIndexedExpression());
		assertEquals(Region.SEPARATOR + TEST_REGION_NAME, simple.getFromClause());
		assertEquals(TEST_REGION_NAME, simple.getRegion().getName());
		assertEquals(com.gemstone.gemfire.cache.query.IndexType.FUNCTIONAL, simple.getType());
	}

	@Test
	public void testComplexIndex() throws Exception {
		assertEquals("complex-index", complex.getName());
		assertEquals("tsi.name", complex.getIndexedExpression());
		assertEquals(Region.SEPARATOR + TEST_REGION_NAME + " tsi", complex.getFromClause());
		assertEquals(TEST_REGION_NAME, complex.getRegion().getName());
		assertEquals(com.gemstone.gemfire.cache.query.IndexType.HASH, complex.getType());
	}

}
