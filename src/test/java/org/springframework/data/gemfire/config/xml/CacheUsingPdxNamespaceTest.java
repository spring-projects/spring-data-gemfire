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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.apache.geode.pdx.PdxSerializer;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.config.support.PdxDiskStoreAwareBeanFactoryPostProcessor;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The CacheUsingPdxNamespaceTest class is a test suite of test case testing the Spring Data GemFire XML namespace
 * when PDX is configured in GemFire.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.3
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "cache-using-pdx-ns.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class CacheUsingPdxNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testApplicationContextHasPdxDiskStoreAwareBeanFactoryPostProcessor() {
		PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor = context.getBean(
			PdxDiskStoreAwareBeanFactoryPostProcessor.class);

		// NOTE the postProcessor reference will not be null as the ApplicationContext.getBean(:Class) method (getting
		// a bean by Class type) will throw a NoSuchBeanDefinitionException if no bean of type
		// PdxDiskStoreAwareBeanFactoryPostProcessor could be found, or throw a NoUniqueBeanDefinitionException if
		// our PdxDiskStoreAwareBeanFactoryPostProcessor bean is not unique!
		assertNotNull(postProcessor);
		assertEquals("pdxStore", postProcessor.getPdxDiskStoreName());
	}

	@Test
	public void testCachePdxConfiguration() {
		final CacheFactoryBean cacheFactoryBean = context.getBean("&gemfireCache", CacheFactoryBean.class);

		assertNotNull(cacheFactoryBean);
		assertEquals("pdxStore", cacheFactoryBean.getPdxDiskStoreName());
		assertTrue(Boolean.TRUE.equals(cacheFactoryBean.getPdxPersistent()));
		assertTrue(Boolean.TRUE.equals(cacheFactoryBean.getPdxReadSerialized()));

		final PdxSerializer autoSerializer = context.getBean("autoSerializer", PdxSerializer.class);

		assertNotNull(autoSerializer);
		assertSame(autoSerializer, cacheFactoryBean.getPdxSerializer());
	}

}
