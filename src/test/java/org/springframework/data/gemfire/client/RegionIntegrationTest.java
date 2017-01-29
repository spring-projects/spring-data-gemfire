/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import java.util.Arrays;

import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.PartitionAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.util.CacheListenerAdapter;
import org.apache.geode.cache.util.CacheWriterAdapter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Costin Leau
 * @author David Turanski
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "basic-region.xml" },
	initializers=GemfireTestApplicationContextInitializer.class)
public class RegionIntegrationTest {

	private static class CacheList<K, V> extends CacheListenerAdapter<K, V> {
	}

	private static class CacheLoad<K, V> implements CacheLoader<K, V> {

		@Override
		public V load(LoaderHelper<K, V> arg0) throws CacheLoaderException {
			return null;
		}

		@Override
		public void close() {
		}
	}

	private static class CacheWrite<K, V> extends CacheWriterAdapter<K, V> {
	}

	@Autowired
	private ApplicationContext ctx;


	@Test
	public void testBasicRegion() throws Exception {
		@SuppressWarnings("rawtypes")
		Region region = ctx.getBean("basic", Region.class);
		assertEquals("basic", region.getName());
	}

	@Test
	public void testExistingRegion() throws Exception {
		@SuppressWarnings("rawtypes")
		Region region = ctx.getBean("root", Region.class);
		// the name property seems to be ignored
		assertEquals("root", region.getName());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testRegionWithListeners() throws Exception {
		Region region = ctx.getBean("listeners", Region.class);
		assertEquals("listeners", region.getName());
		CacheListener[] listeners = region.getAttributes().getCacheListeners();
		assertEquals(2, listeners.length);
		assertSame(CacheList.class, listeners[0].getClass());
		assertSame(CacheLoad.class, region.getAttributes().getCacheLoader().getClass());
		assertSame(CacheWrite.class, region.getAttributes().getCacheWriter().getClass());
	}

	// @Test
	// TODO: disabled since the interest registration requires a proper pool to
	// be created, which requires another JVM to run with the server/locator
	@SuppressWarnings("rawtypes")
	public void testRegionInterest() throws Exception {
		ClientRegionFactoryBean regionFB = (ClientRegionFactoryBean) ctx.getBean("&basic-client");
		System.out.println("**** interests are " + Arrays.toString(regionFB.getInterests()));
		BeanDefinition bd = ((BeanDefinitionRegistry) ctx.getAutowireCapableBeanFactory())
				.getBeanDefinition("basic-client");
		System.out.println(bd.getPropertyValues().getPropertyValue("interests").getValue());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testRegionAttributes() throws Exception {
		Region region = ctx.getBean("attr-region", Region.class);
		assertEquals("attr-region", region.getName());
		RegionAttributes attr = region.getAttributes();
		assertEquals(1024, attr.getInitialCapacity());

		PartitionAttributes pa = attr.getPartitionAttributes();
		assertEquals(512, pa.getLocalMaxMemory());
		assertEquals(1, pa.getRedundantCopies());
	}
}
