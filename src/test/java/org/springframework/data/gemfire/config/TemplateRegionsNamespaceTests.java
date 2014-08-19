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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanIsAbstractException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.data.gemfire.test.support.CollectionUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheLoaderException;
import com.gemstone.gemfire.cache.EntryOperation;
import com.gemstone.gemfire.cache.LoaderHelper;
import com.gemstone.gemfire.cache.PartitionResolver;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEvent;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.partition.PartitionListenerAdapter;
import com.gemstone.gemfire.cache.util.CacheListenerAdapter;
import com.gemstone.gemfire.cache.util.CacheWriterAdapter;

/**
 * The TemplateRegionsNamespaceTests class...
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class TemplateRegionsNamespaceTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Resource(name = "SimpleReplicateRegion")
	private Region<Integer, String> simpleReplicateRegion;

	@SuppressWarnings("deprecation")
	protected static <K, V> void assertBaseRegionAttributes(final Region<K, V> region) {
		assertNotNull("The Region must not be null!", region);

		RegionAttributes<K, V> regionAttributes = region.getAttributes();

		assertNotNull("The Region must have RegionAttributes defined!", regionAttributes);

		assertTrue(regionAttributes.getCloningEnabled());
		assertFalse(regionAttributes.getConcurrencyChecksEnabled());
		assertTrue(regionAttributes.isDiskSynchronous());
		assertTrue(regionAttributes.getIgnoreJTA());
		assertEquals(Long.class, regionAttributes.getKeyConstraint());
		assertEquals(0.90f, regionAttributes.getLoadFactor());
		assertFalse(regionAttributes.getDataPolicy().withPersistence());
		assertTrue(regionAttributes.getStatisticsEnabled());
		assertEquals(String.class, regionAttributes.getValueConstraint());
		assertTrue(regionAttributes.getIndexMaintenanceSynchronous());
		assertTrue(ObjectUtils.isEmpty(regionAttributes.getCacheListeners()));
		assertNotNull(regionAttributes.getCacheLoader());
		assertTrue(regionAttributes.getCacheLoader() instanceof TestCacheLoader);
		assertEquals("X", regionAttributes.getCacheLoader().toString());
		assertNotNull(regionAttributes.getCacheWriter());
		assertTrue(regionAttributes.getCacheWriter() instanceof TestCacheWriter);
		assertEquals("Y", regionAttributes.getCacheWriter().toString());
	}

	@Test
	public void testNoAbstractRegionTemplateBeans() {
		String[] beanNames = {
			"BaseRegion", "ExtendedRegionWithOverrides", "BaseReplicateRegion", "BasePartitionRegion", "BaseLocalRegion"
		};

		for (String beanName : beanNames) {
			assertTrue(applicationContext.containsBean(beanName));
			assertTrue(applicationContext.containsBeanDefinition(beanName));

			try {
				applicationContext.getBean(beanName);
				fail(String.format("The abstract bean definition '%1$s' should not exists as a bean in the Spring context!",
					beanName));
			}
			catch (BeansException ignore) {
				assertTrue(ignore instanceof BeanIsAbstractException);
				assertTrue(ignore.getMessage().contains(beanName));
			}
		}
	}

	@Test
	public void testSimpleReplicateRegion() {
		fail("Not Implemented!");
	}

	protected static interface Nameable {
		void setName(String name);
	}

	protected static abstract class AbstractNameable implements Nameable {

		private String name;

		public void setName(final String name) {
			this.name = name;
		}

		protected String getName() {
			return this.name;
		}

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestAsyncEventListener extends AbstractNameable implements AsyncEventListener {

		@Override public boolean processEvents(final List<AsyncEvent> asyncEvents) {
			return false;
		}

		@Override public void close() {

		}
	}

	public static final class TestCacheListener extends CacheListenerAdapter {

		private String name;

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestCacheLoader extends AbstractNameable implements CacheLoader {

		@Override public Object load(final LoaderHelper loaderHelper) throws CacheLoaderException {
			return null;
		}

		@Override public void close() {

		}
	}

	public static final class TestCacheWriter extends CacheWriterAdapter {

		private String name;

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestPartitionListener extends PartitionListenerAdapter {
		private String name;

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return name;
		}
	}

	public static final class TestPartitionResolver extends AbstractNameable implements PartitionResolver {

		@Override public Object getRoutingObject(final EntryOperation entryOperation) {
			return null;
		}

		@Override public String getName() {
			return super.getName();
		}

		@Override public void close() {
		}
	}

}
