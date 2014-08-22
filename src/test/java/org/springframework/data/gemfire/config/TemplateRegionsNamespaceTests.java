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
import static org.junit.Assert.assertNull;
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
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheLoaderException;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.EntryOperation;
import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAlgorithm;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.InterestPolicy;
import com.gemstone.gemfire.cache.LoaderHelper;
import com.gemstone.gemfire.cache.PartitionResolver;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.SubscriptionAttributes;
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

	@Resource(name = "InheritedReplicateRegion")
	private Region<Long, String> inheritedReplicateRegion;

	@Resource(name = "ComplexReplicateRegion")
	private Region complexReplicateRegion;

	protected static void assertRegionMetaData(final Region<?, ?> region, final String expectedRegionName) {
		assertRegionMetaData(region, expectedRegionName, Region.SEPARATOR + expectedRegionName);
	}

	protected static void assertRegionMetaData(final Region<?, ?> region, final String expectedRegionName, final String expectedRegionPath) {
		assertNotNull(String.format("The '%1$s' Region was not properly configured and initialized!",
			expectedRegionName), region);
		assertEquals(expectedRegionName, region.getName());
		assertEquals(expectedRegionPath, region.getFullPath());
		assertNotNull(String.format("The '%1$s' Region must have RegionAttributes defined!",
			expectedRegionName), region.getAttributes());
	}

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

	protected static <K, V> void assertBaseReplicateRegionAttributes(final Region<K, V> replicateRegion) {
		assertBaseRegionAttributes(replicateRegion);
		assertEquals(2, replicateRegion.getAttributes().getConcurrencyLevel());
		assertTrue(replicateRegion.getAttributes().getEnableAsyncConflation());
		assertTrue(replicateRegion.getAttributes().getEnableSubscriptionConflation());

		EvictionAttributes evictionAttributes = replicateRegion.getAttributes().getEvictionAttributes();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evictionAttributes.getAction());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());
		assertEquals(1000, evictionAttributes.getMaximum());

		SubscriptionAttributes subscriptionAttributes = replicateRegion.getAttributes().getSubscriptionAttributes();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.CACHE_CONTENT, subscriptionAttributes.getInterestPolicy());
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
		assertRegionMetaData(simpleReplicateRegion, "SimpleReplicateRegion");
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, simpleReplicateRegion.getAttributes().getDataPolicy());
		assertEquals(8, simpleReplicateRegion.getAttributes().getConcurrencyLevel());
		assertEquals(Integer.class, simpleReplicateRegion.getAttributes().getKeyConstraint());
		assertTrue(simpleReplicateRegion.getAttributes().isLockGrantor());
		assertEquals(Scope.GLOBAL, simpleReplicateRegion.getAttributes().getScope());
		assertNull(simpleReplicateRegion.getAttributes().getValueConstraint());
		assertNotNull(simpleReplicateRegion.getAttributes().getCacheListeners());
		assertEquals(1, simpleReplicateRegion.getAttributes().getCacheListeners().length);
		assertTrue(simpleReplicateRegion.getAttributes().getCacheListeners()[0] instanceof TestCacheListener);
		assertEquals("Simple", simpleReplicateRegion.getAttributes().getCacheListeners()[0].toString());
		assertNull(simpleReplicateRegion.getAttributes().getCacheLoader());
		assertNull(simpleReplicateRegion.getAttributes().getCacheWriter());
	}

	@Test
	public void testInheritedReplicateRegion() {
		assertRegionMetaData(inheritedReplicateRegion, "InheritedReplicateRegion");
		assertEquals(Scope.DISTRIBUTED_ACK, inheritedReplicateRegion.getAttributes().getScope());
		assertBaseReplicateRegionAttributes(inheritedReplicateRegion);
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testComplexReplicateRegion() {
		assertRegionMetaData(complexReplicateRegion, "ComplexReplicateRegion");
		assertEquals(DataPolicy.PERSISTENT_REPLICATE, complexReplicateRegion.getAttributes().getDataPolicy());
		assertEquals(Scope.DISTRIBUTED_ACK, complexReplicateRegion.getAttributes().getScope());
		assertFalse(complexReplicateRegion.getAttributes().getCloningEnabled());
		assertEquals(2, complexReplicateRegion.getAttributes().getConcurrencyLevel());
		assertTrue(complexReplicateRegion.getAttributes().isDiskSynchronous());
		assertFalse(complexReplicateRegion.getAttributes().getEnableAsyncConflation());
		assertTrue(complexReplicateRegion.getAttributes().getEnableSubscriptionConflation());
		assertFalse(complexReplicateRegion.getAttributes().getIgnoreJTA());
		assertEquals(1000, complexReplicateRegion.getAttributes().getInitialCapacity());
		assertEquals(Integer.class, complexReplicateRegion.getAttributes().getKeyConstraint());
		assertEquals(0.90f, complexReplicateRegion.getAttributes().getLoadFactor());
		assertTrue(complexReplicateRegion.getAttributes().getStatisticsEnabled());
		assertTrue(complexReplicateRegion.getAttributes().getIndexMaintenanceSynchronous());
		assertEquals(String.class, complexReplicateRegion.getAttributes().getValueConstraint());
		assertNotNull(complexReplicateRegion.getAttributes().getCacheListeners());
		assertEquals(1, complexReplicateRegion.getAttributes().getCacheListeners().length);
		assertTrue(complexReplicateRegion.getAttributes().getCacheListeners()[0] instanceof TestCacheListener);
		assertEquals("ComplexListener", complexReplicateRegion.getAttributes().getCacheListeners()[0].toString());
		assertNotNull(complexReplicateRegion.getAttributes().getCacheLoader());
		assertEquals("ComplexLoader", complexReplicateRegion.getAttributes().getCacheLoader().toString());
		assertNotNull(complexReplicateRegion.getAttributes().getCacheWriter());
		assertEquals("Y", complexReplicateRegion.getAttributes().getCacheWriter().toString());

		EvictionAttributes evictionAttributes = complexReplicateRegion.getAttributes().getEvictionAttributes();

		assertNotNull(evictionAttributes);
		assertEquals(EvictionAction.OVERFLOW_TO_DISK, evictionAttributes.getAction());
		assertEquals(EvictionAlgorithm.LRU_ENTRY, evictionAttributes.getAlgorithm());
		assertEquals(1024, evictionAttributes.getMaximum());

		SubscriptionAttributes subscriptionAttributes = complexReplicateRegion.getAttributes().getSubscriptionAttributes();

		assertNotNull(subscriptionAttributes);
		assertEquals(InterestPolicy.ALL, subscriptionAttributes.getInterestPolicy());
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
