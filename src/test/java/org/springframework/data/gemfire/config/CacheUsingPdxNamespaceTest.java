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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.pdx.PdxSerializer;

/**
 * The CacheUsingPdxNamespaceTest class is a test suite of test case testing the Spring Data GemFire XML namespace
 * when PDX is configured in GemFire.
 * <p/>
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.3.3
 */
@ContextConfiguration(locations = "/org/springframework/data/gemfire/config/cache-using-pdx-ns.xml",
	initializers = GemfireTestApplicationContextInitializer.class)
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class CacheUsingPdxNamespaceTest {

	@Autowired
	private ApplicationContext context;

	protected PdxDiskStoreAwareBeanFactoryPostProcessor getPdxDiskStoreAwareBeanFactoryPostProcessor(AbstractApplicationContext context) {
		for (BeanFactoryPostProcessor postProcessor : context.getBeanFactoryPostProcessors()) {
			if (postProcessor instanceof PdxDiskStoreAwareBeanFactoryPostProcessor) {
				return (PdxDiskStoreAwareBeanFactoryPostProcessor) postProcessor;
			}
		}

		return null;
	}

	@Test
	public void testApplicationContextHasPdxDiskStoreAwareBeanFactoryPostProcessor() {
		assumeTrue(context instanceof AbstractApplicationContext);

		final PdxDiskStoreAwareBeanFactoryPostProcessor postProcessor = getPdxDiskStoreAwareBeanFactoryPostProcessor(
			(AbstractApplicationContext) context);

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
