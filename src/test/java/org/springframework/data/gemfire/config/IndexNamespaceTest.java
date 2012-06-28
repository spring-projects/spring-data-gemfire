/*
 * Copyright 2011-2012 the original author or authors.
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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexType;

/**
 * 
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("index-ns.xml")
public class IndexNamespaceTest {

	private String name = "test-index";
	@Autowired
	private ApplicationContext context;


	@Before
	public void setUp() throws Exception {
		Cache cache = (Cache) context.getBean("gemfire-cache");
		if (cache.getRegion(name) == null) {
			cache.createRegionFactory().create("test-index");
		}
	}

	@Test
	public void testBasicIndex() throws Exception {
		Index idx = (Index) context.getBean("simple");

		assertEquals("/test-index", idx.getFromClause());
		assertEquals("status", idx.getIndexedExpression());
		assertEquals("simple", idx.getName());
		assertEquals(name, idx.getRegion().getName());
		assertEquals(IndexType.FUNCTIONAL, idx.getType());
	}

	@Test
	public void testComplexIndex() throws Exception {
		Index idx = (Index) context.getBean("complex");

		assertEquals("/test-index tsi", idx.getFromClause());
		assertEquals("tsi.name", idx.getIndexedExpression());
		assertEquals("complex-index", idx.getName());
		assertEquals(name, idx.getRegion().getName());
		assertEquals(IndexType.PRIMARY_KEY, idx.getType());
	}
}
