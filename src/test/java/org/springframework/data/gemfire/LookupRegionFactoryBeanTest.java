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
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.geode.cache.AttributesMutator;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheListener;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheWriter;
import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.EvictionAttributesMutator;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewaySender;

import org.junit.Test;

/**
 * The LookupRegionFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the LookupRegionFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.LookupRegionFactoryBean
 * @see org.apache.geode.cache.AttributesMutator
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.EvictionAttributesMutator
 * @see org.apache.geode.cache.Region
 * @since 1.7.0
 */
public class LookupRegionFactoryBeanTest {

	private AsyncEventQueue mockAsyncEventQueue(final String id) {
		AsyncEventQueue mockQueue = mock(AsyncEventQueue.class, String.format("MockAsyncEventQueue.%1$s", id));
		when(mockQueue.getId()).thenReturn(id);
		return mockQueue;
	}

	private GatewaySender mockGatewaySender(final String id) {
		GatewaySender mockGatewaySender = mock(GatewaySender.class, String.format("MockGatewaySender.%1$s", id));
		when(mockGatewaySender.getId()).thenReturn(id);
		return mockGatewaySender;
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testAfterPropertiesSet() throws Exception {

		Cache mockCache = mock(Cache.class, "testAfterPropertiesSet.MockCache");

		Region<Object, Object> mockRegion = mock(Region.class, "testAfterPropertiesSet.MockRegion");

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class,
			"testAfterPropertiesSet.MockRegionAttributes");

		EvictionAttributesMutator mockEvictionAttributesMutator = mock(EvictionAttributesMutator.class,
			"testAfterPropertiesSet.EvictionAttributesMutator");

		AttributesMutator<Object, Object> mockAttributesMutator = mock(AttributesMutator.class,
			"testAfterPropertiesSet.MockAttributesMutator");

		when(mockCache.getRegion(eq("Example"))).thenReturn(mockRegion);
		when(mockRegion.getFullPath()).thenReturn("/Example");
		when(mockRegion.getName()).thenReturn("Example");
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getStatisticsEnabled()).thenReturn(true);
		when(mockRegion.getAttributesMutator()).thenReturn(mockAttributesMutator);
		when(mockAttributesMutator.getEvictionAttributesMutator()).thenReturn(mockEvictionAttributesMutator);

		AsyncEventQueue mockAsyncEventQueueOne = mockAsyncEventQueue("AEQ1");
		AsyncEventQueue mockAsyncEventQueueTwo = mockAsyncEventQueue("AEQ2");

		CacheListener mockCacheListenerZero = mock(CacheListener.class, "testAfterPropertiesSet.MockCacheListener.0");
		CacheListener mockCacheListenerOne = mock(CacheListener.class, "testAfterPropertiesSet.MockCacheListener.1");
		CacheListener mockCacheListenerTwo = mock(CacheListener.class, "testAfterPropertiesSet.MockCacheListener.2");

		CacheLoader mockCacheLoader = mock(CacheLoader.class, "testAfterPropertiesSet.MockCacheLoader");

		CacheWriter mockCacheWriter = mock(CacheWriter.class, "testAfterPropertiesSet.MockCacheWriter");

		CustomExpiry mockCustomExpiryTti = mock(CustomExpiry.class, "testAfterPropertiesSet.MockCustomExpiry.TTI");
		CustomExpiry mockCustomExpiryTtl = mock(CustomExpiry.class, "testAfterPropertiesSet.MockCustomExpiry.TTL");

		ExpirationAttributes mockExpirationAttributesEntryTti = mock(ExpirationAttributes.class,
			"testAfterPropertiesSet.MockExpirationAttributes.Entry.TTI");
		ExpirationAttributes mockExpirationAttributesEntryTtl = mock(ExpirationAttributes.class,
			"testAfterPropertiesSet.MockExpirationAttributes.Entry.TTL");
		ExpirationAttributes mockExpirationAttributesRegionTti = mock(ExpirationAttributes.class,
			"testAfterPropertiesSet.MockExpirationAttributes.Region.TTI");
		ExpirationAttributes mockExpirationAttributesRegionTtl = mock(ExpirationAttributes.class,
			"testAfterPropertiesSet.MockExpirationAttributes.Region.TTL");

		GatewaySender mockGatewaySender = mockGatewaySender("GWS1");

		LookupRegionFactoryBean factoryBean = new LookupRegionFactoryBean();

		factoryBean.setAsyncEventQueues(new AsyncEventQueue[] { mockAsyncEventQueueOne, mockAsyncEventQueueTwo });
		factoryBean.setBeanName("Example");
		factoryBean.setCache(mockCache);
		factoryBean.setCacheLoader(mockCacheLoader);
		factoryBean.setCacheWriter(mockCacheWriter);
		factoryBean.setCloningEnabled(true);
		factoryBean.setCustomEntryIdleTimeout(mockCustomExpiryTti);
		factoryBean.setCustomEntryTimeToLive(mockCustomExpiryTtl);
		factoryBean.setEntryIdleTimeout(mockExpirationAttributesEntryTti);
		factoryBean.setEntryTimeToLive(mockExpirationAttributesEntryTtl);
		factoryBean.setGatewaySenders(new GatewaySender[] { mockGatewaySender });
		factoryBean.setEvictionMaximum(1000);
		factoryBean.setRegionIdleTimeout(mockExpirationAttributesRegionTti);
		factoryBean.setRegionTimeToLive(mockExpirationAttributesRegionTtl);
		factoryBean.setStatisticsEnabled(true);

		factoryBean.setCacheListeners(new CacheListener[] {
			mockCacheListenerZero, mockCacheListenerOne, mockCacheListenerTwo
		});

		factoryBean.afterPropertiesSet();

		verify(mockAttributesMutator, times(1)).addAsyncEventQueueId(eq("AEQ1"));
		verify(mockAttributesMutator, times(1)).addAsyncEventQueueId(eq("AEQ2"));
		verify(mockAttributesMutator, times(1)).addCacheListener(same(mockCacheListenerZero));
		verify(mockAttributesMutator, times(1)).addCacheListener(same(mockCacheListenerOne));
		verify(mockAttributesMutator, times(1)).addCacheListener(same(mockCacheListenerTwo));
		verify(mockAttributesMutator, times(1)).setCacheLoader(same(mockCacheLoader));
		verify(mockAttributesMutator, times(1)).setCacheWriter(same(mockCacheWriter));
		verify(mockAttributesMutator, times(1)).setCloningEnabled(eq(true));
		verify(mockAttributesMutator, times(1)).setCustomEntryIdleTimeout(same(mockCustomExpiryTti));
		verify(mockAttributesMutator, times(1)).setCustomEntryTimeToLive(same(mockCustomExpiryTtl));
		verify(mockAttributesMutator, times(1)).setEntryIdleTimeout(same(mockExpirationAttributesEntryTti));
		verify(mockAttributesMutator, times(1)).setEntryTimeToLive(same(mockExpirationAttributesEntryTtl));
		verify(mockAttributesMutator, times(1)).addGatewaySenderId(eq("GWS1"));
		verify(mockEvictionAttributesMutator, times(1)).setMaximum(eq(1000));
		verify(mockAttributesMutator, times(1)).setRegionIdleTimeout(same(mockExpirationAttributesRegionTti));
		verify(mockAttributesMutator, times(1)).setRegionTimeToLive(same(mockExpirationAttributesRegionTtl));
	}

	@Test(expected = IllegalStateException.class)
	@SuppressWarnings("unchecked")
	public void testAfterPropertiesSetWhenRegionStatisticsDisabledAndExpirationSpecified() throws Exception {

		Cache mockCache = mock(Cache.class);

		Region<Object, Object> mockRegion = mock(Region.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		AttributesMutator mockAttributesMutator = mock(AttributesMutator.class);

		EvictionAttributesMutator mockEvictionAttributesMutator = mock(EvictionAttributesMutator.class);

		ExpirationAttributes mockExpirationAttributesEntryTtl = mock(ExpirationAttributes.class);

		when(mockCache.getRegion(eq("Example"))).thenReturn(mockRegion);
		when(mockRegion.getFullPath()).thenReturn("/Example");
		when(mockRegion.getName()).thenReturn("Example");
		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegion.getAttributesMutator()).thenReturn(mockAttributesMutator);
		when(mockAttributesMutator.getEvictionAttributesMutator()).thenReturn(mockEvictionAttributesMutator);
		when(mockRegionAttributes.getStatisticsEnabled()).thenReturn(false);

		LookupRegionFactoryBean factoryBean = new LookupRegionFactoryBean();

		factoryBean.setBeanName("Example");
		factoryBean.setCache(mockCache);
		factoryBean.setEntryTimeToLive(mockExpirationAttributesEntryTtl);
		//factoryBean.setStatisticsEnabled(true);

		assertTrue(factoryBean.isStatisticsEnabled());

		try {
			factoryBean.afterPropertiesSet();
		}
		catch (IllegalStateException expected) {
			assertEquals("Statistics for Region [/Example] must be enabled to change Entry & Region TTL/TTI Expiration settings",
				expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockAttributesMutator, never()).setEntryTimeToLive(any(ExpirationAttributes.class));
		}
	}

	@Test
	public void testIsLookupEnabledAlways() {

		LookupRegionFactoryBean factoryBean = new LookupRegionFactoryBean();

		assertTrue(factoryBean.isLookupEnabled());

		factoryBean.setLookupEnabled(false);

		assertTrue(factoryBean.isLookupEnabled());
	}
}
