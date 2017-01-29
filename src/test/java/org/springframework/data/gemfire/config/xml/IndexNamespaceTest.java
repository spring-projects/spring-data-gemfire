/*
 * Copyright 2011-2018 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;

import javax.annotation.Resource;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.query.Index;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests with test cases testing the functionality of GemFire Index creation using
 * the Spring Data GemFire XML namespace (XSD) and the {@link IndexParser}.
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
@RunWith(SpringRunner.class)
@ContextConfiguration(locations="index-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings({ "deprecation", "unused" })
public class IndexNamespaceTest {

	private static final String TEST_REGION_NAME = "IndexedRegion";

	@Autowired
	private ApplicationContext applicationContext;

	@Resource(name = "basic")
	private Index basic;

	@Resource(name = "complex")
	private Index complex;

	@Test
	public void basicIndexIsCorrect() throws Exception {
		assertEquals("basic", basic.getName());
		assertEquals("status", basic.getIndexedExpression());
		assertEquals(Region.SEPARATOR + TEST_REGION_NAME, basic.getFromClause());
		assertEquals(TEST_REGION_NAME, basic.getRegion().getName());
		assertEquals(org.apache.geode.cache.query.IndexType.FUNCTIONAL, basic.getType());
	}

	@Test
	public void basicIndexFactoryBeanIsCorrect() {

		IndexFactoryBean basicIndexFactoryBean = applicationContext.getBean("&basic", IndexFactoryBean.class);

		assertThat(basicIndexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(basicIndexFactoryBean.isOverride()).isFalse();
	}

	@Test
	public void complexIndexIsCorrect() throws Exception {
		assertEquals("complex-index", complex.getName());
		assertEquals("tsi.name", complex.getIndexedExpression());
		assertEquals(Region.SEPARATOR + TEST_REGION_NAME + " tsi", complex.getFromClause());
		assertEquals(TEST_REGION_NAME, complex.getRegion().getName());
		assertEquals(org.apache.geode.cache.query.IndexType.HASH, complex.getType());
	}

	@Test
	public void indexWithIgnoreAndOverrideIsCorrect() {

		IndexFactoryBean indexFactoryBean =
			applicationContext.getBean("&index-with-ignore-and-override", IndexFactoryBean.class);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isTrue();
		assertThat(indexFactoryBean.isOverride()).isTrue();
	}
}
