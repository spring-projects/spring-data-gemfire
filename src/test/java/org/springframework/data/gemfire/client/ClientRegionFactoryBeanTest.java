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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.io.InputStream;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.compression.Compressor;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.core.io.Resource;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * Unit tests for {@link ClientRegionFactoryBean}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 */
public class ClientRegionFactoryBeanTest {

	private ClientRegionFactoryBean<Object, Object> factoryBean;

	@Before
	public void setup() {
		factoryBean = spy(new ClientRegionFactoryBean<>());
	}

	@After
	public void tearDown() throws Exception {
		factoryBean.destroy();
		factoryBean = null;
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void createRegionUsingDefaultShortcut() throws Exception {

		String testRegionName = "TestRegion";

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientRegionFactory mockClientRegionFactory = mock(ClientRegionFactory.class);

		Region mockRegion = mock(Region.class);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class);

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.LOCAL)))
			.thenReturn(mockClientRegionFactory);
		when(mockClientRegionFactory.create(eq(testRegionName))).thenReturn(mockRegion);
		when(mockRegionAttributes.getCloningEnabled()).thenReturn(false);
		when(mockRegionAttributes.getCompressor()).thenReturn(mock(Compressor.class));
		when(mockRegionAttributes.getConcurrencyChecksEnabled()).thenReturn(true);
		when(mockRegionAttributes.getConcurrencyLevel()).thenReturn(8);
		when(mockRegionAttributes.getCustomEntryIdleTimeout()).thenReturn(null);
		when(mockRegionAttributes.getCustomEntryTimeToLive()).thenReturn(null);
		when(mockRegionAttributes.getDiskStoreName()).thenReturn("TestDiskStoreOne");
		when(mockRegionAttributes.isDiskSynchronous()).thenReturn(false);
		when(mockRegionAttributes.getEntryIdleTimeout()).thenReturn(mock(ExpirationAttributes.class));
		when(mockRegionAttributes.getEntryTimeToLive()).thenReturn(mock(ExpirationAttributes.class));
		when(mockRegionAttributes.getEvictionAttributes()).thenReturn(mock(EvictionAttributes.class));
		when(mockRegionAttributes.getInitialCapacity()).thenReturn(101);
		when(mockRegionAttributes.getKeyConstraint()).thenReturn(Long.class);
		when(mockRegionAttributes.getLoadFactor()).thenReturn(0.75f);
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPoolOne");
		when(mockRegionAttributes.getRegionIdleTimeout()).thenReturn(mock(ExpirationAttributes.class));
		when(mockRegionAttributes.getRegionTimeToLive()).thenReturn(mock(ExpirationAttributes.class));
		when(mockRegionAttributes.getStatisticsEnabled()).thenReturn(true);
		when(mockRegionAttributes.getValueConstraint()).thenReturn(Number.class);

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		Pool mockPool = mock(Pool.class);

		Resource mockSnapshot = mock(Resource.class, "Snapshot");

		when(mockBeanFactory.containsBean(eq("TestPoolOne"))).thenReturn(false);
		when(mockBeanFactory.containsBean(eq("TestPoolTwo"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq("TestPoolTwo"), eq(Pool.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("TestPoolTwo"))).thenReturn(mockPool);
		when(mockPool.getName()).thenReturn("TestPoolTwo");
		when(mockSnapshot.getInputStream()).thenReturn(mock(InputStream.class));

		EvictionAttributes evictionAttributes = EvictionAttributes.createLRUEntryAttributes();

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setDiskStoreName("TestDiskStoreTwo");
		factoryBean.setEvictionAttributes(evictionAttributes);
		factoryBean.setPersistent(false);
		factoryBean.setPoolName("TestPoolTwo");
		factoryBean.setSnapshot(mockSnapshot);
		factoryBean.setShortcut(null);

		Region actualRegion = factoryBean.createRegion(mockClientCache, testRegionName);

		assertSame(mockRegion, actualRegion);

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.LOCAL));
		verify(mockClientRegionFactory, times(1)).setCloningEnabled(eq(false));
		verify(mockClientRegionFactory, times(1)).setCompressor(any(Compressor.class));
		verify(mockClientRegionFactory, times(1)).setConcurrencyChecksEnabled(eq(true));
		verify(mockClientRegionFactory, times(1)).setConcurrencyLevel(eq(8));
		verify(mockClientRegionFactory, times(1)).setCustomEntryIdleTimeout(null);
		verify(mockClientRegionFactory, times(1)).setCustomEntryTimeToLive(null);
		verify(mockClientRegionFactory, times(1)).setDiskStoreName(eq("TestDiskStoreOne"));
		verify(mockClientRegionFactory, times(1)).setDiskSynchronous(eq(false));
		verify(mockClientRegionFactory, times(1)).setEntryIdleTimeout(any(ExpirationAttributes.class));
		verify(mockClientRegionFactory, times(1)).setEntryTimeToLive(any(ExpirationAttributes.class));
		verify(mockClientRegionFactory, times(1)).setEvictionAttributes(eq(evictionAttributes));
		verify(mockClientRegionFactory, times(1)).setInitialCapacity(eq(101));
		verify(mockClientRegionFactory, times(1)).setKeyConstraint(eq(Long.class));
		verify(mockClientRegionFactory, times(1)).setLoadFactor(eq(0.75f));
		verify(mockClientRegionFactory, never()).setPoolName(eq("TestPoolOne"));
		verify(mockClientRegionFactory, times(1)).setRegionIdleTimeout(any(ExpirationAttributes.class));
		verify(mockClientRegionFactory, times(1)).setRegionTimeToLive(any(ExpirationAttributes.class));
		verify(mockClientRegionFactory, times(1)).setStatisticsEnabled(eq(true));
		verify(mockClientRegionFactory, times(1)).setValueConstraint(eq(Number.class));
		verify(mockClientRegionFactory, times(1)).setDiskStoreName(eq("TestDiskStoreTwo"));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("TestPoolTwo"));
		verify(mockClientRegionFactory, times(1)).create(eq(testRegionName));
		verify(mockRegion, never()).loadSnapshot(any(InputStream.class));
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void createRegionUsingDefaultPersistentShortcut() throws Exception {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		Region<Object, Object> mockRegion = mock(Region.class);

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.LOCAL_PERSISTENT)))
			.thenReturn(mockClientRegionFactory);
		when(mockClientRegionFactory.create(eq("TestRegion"))).thenReturn(mockRegion);

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(false);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPersistent(true);
		factoryBean.setPoolName("TestPool");

		Region<Object, Object> actualRegion = factoryBean.createRegion(mockClientCache, "TestRegion");

		assertSame(mockRegion, actualRegion);

		verify(mockClientCache, times(1)).createClientRegionFactory(eq(ClientRegionShortcut.LOCAL_PERSISTENT));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("TestPool"));
		verify(mockClientRegionFactory, times(1)).create(eq("TestRegion"));
		verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(mockBeanFactory, never()).getBean(eq("TestPool"));
		verify(mockRegion, never()).loadSnapshot(any(InputStream.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createRegionWithSpecifiedShortcut() throws Exception {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		Region<Object, Object> mockRegion = mock(Region.class);

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.CACHING_PROXY)))
			.thenReturn(mockClientRegionFactory);
		when(mockClientRegionFactory.create(eq("TestRegion"))).thenReturn(mockRegion);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setShortcut(ClientRegionShortcut.CACHING_PROXY);

		Region<Object, Object> actualRegion = factoryBean.createRegion(mockClientCache, "TestRegion");

		assertSame(mockRegion, actualRegion);

		verifyZeroInteractions(mockBeanFactory);
		verify(mockClientCache, times(1))
			.createClientRegionFactory(eq(ClientRegionShortcut.CACHING_PROXY));
		verify(mockClientRegionFactory, times(1)).create(eq("TestRegion"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void createRegionWithSubRegionCreation() throws Exception {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		Region<Object, Object> mockRegion = mock(Region.class, "RootRegion");
		Region<Object, Object> mockSubRegion = mock(Region.class, "SubRegion");

		when(mockClientCache.createClientRegionFactory(eq(ClientRegionShortcut.PROXY))).thenReturn(mockClientRegionFactory);
		when(mockClientRegionFactory.createSubregion(eq(mockRegion), eq("TestSubRegion"))).thenReturn(mockSubRegion);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setParent(mockRegion);
		factoryBean.setShortcut(ClientRegionShortcut.PROXY);

		Region<Object, Object> actualRegion = factoryBean.createRegion(mockClientCache, "TestSubRegion");

		assertSame(mockSubRegion, actualRegion);

		verifyZeroInteractions(mockBeanFactory);
		verify(mockClientCache, times(1))
			.createClientRegionFactory(eq(ClientRegionShortcut.PROXY));
		verify(mockClientRegionFactory, times(1))
			.createSubregion(eq(mockRegion), eq("TestSubRegion"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void configurePoolFromRegionAttributesAndEagerlyInitializesPool() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("TestPool"));
		verify(mockRegionAttributes, times(1)).getPoolName();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void configurePoolFromRegionAttributesThrowsExceptionWhileEagerlyInitializingPool() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("TestPool"), eq(Pool.class)))
			.thenThrow(new BeanCreationException("TEST"));
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("TestPool"));
		verify(mockRegionAttributes, times(1)).getPoolName();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void configurePoolFromRegionAttributesDoesNotEagerlyInitializePoolWhenNotPoolTypeMatch() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq("TestPool"), eq(Pool.class))).thenReturn(false);
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("TestPool"));
		verify(mockRegionAttributes, times(1)).getPoolName();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromRegionAttributesWhenPoolIsUnresolvable() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
		verify(mockRegionAttributes, times(1)).getPoolName();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromRegionAttributesWhenPoolIsDefaultPool() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegionAttributes.getPoolName()).thenReturn(ClientRegionFactoryBean.DEFAULT_POOL_NAME);

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.configure(mockClientRegionFactory);

		verify(factoryBean, never()).isPoolResolvable(anyString());
		verify(mockBeanFactory, never()).containsBean(anyString());
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
		verify(mockRegionAttributes, times(1)).getPoolName();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromRegionAttributesWhenPoolIsEmpty() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegionAttributes.getPoolName()).thenReturn("  ");

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.configure(mockClientRegionFactory);

		verify(factoryBean, never()).isNotDefaultPool(anyString());
		verify(factoryBean, never()).isPoolResolvable(anyString());
		verify(mockBeanFactory, never()).containsBean(anyString());
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
		verify(mockRegionAttributes, times(1)).getPoolName();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromRegionAttributesWhenPoolIsNull() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockRegionAttributes.getPoolName()).thenReturn(null);

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.configure(mockClientRegionFactory);

		verify(factoryBean, never()).isNotDefaultPool(anyString());
		verify(factoryBean, never()).isPoolResolvable(anyString());
		verify(mockBeanFactory, never()).containsBean(anyString());
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
		verify(mockRegionAttributes, times(1)).getPoolName();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void configurePoolFromClientRegionFactoryBeanAndEagerlyInitializesPool() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		when(mockBeanFactory.containsBean(eq("MockPool"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq("MockPool"), eq(Pool.class))).thenReturn(true);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName("MockPool");
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPool"));
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("MockPool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq("MockPool"), eq(Pool.class));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("MockPool"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void configurePoolFromClientRegionFactoryBeanThrowsExceptionWhileEagerlyInitializingPool() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		when(mockBeanFactory.containsBean(eq("MockPool"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(eq("MockPool"), eq(Pool.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("MockPool"), eq(Pool.class))).thenThrow(new BeanCreationException("TEST"));

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName("MockPool");
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPool"));
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("MockPool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq("MockPool"), eq(Pool.class));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("MockPool"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void configurePoolFromClientRegionFactoryBeanDoesNotEagerlyInitializePoolWhenNotPoolTypeMatch() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		when(mockBeanFactory.containsBean(eq("MockPool"))).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(anyString(), eq(Pool.class))).thenReturn(false);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName("MockPool");
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPool"));
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("MockPool"), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, times(1)).setPoolName(eq("MockPool"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void configuresPoolFromClientRegionFactoryBeanEvenWhenRegionAttributesPoolNameIsSet() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		RegionAttributes<Object, Object> mockRegionAttributes = mock(RegionAttributes.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(true);
		when(mockBeanFactory.isTypeMatch(anyString(), eq(Pool.class))).thenReturn(true);
		when(mockRegionAttributes.getPoolName()).thenReturn("TestPool");

		factoryBean.setAttributes(mockRegionAttributes);
		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName("MockPool");
		factoryBean.configure(mockClientRegionFactory);

		InOrder inOrderVerifier = inOrder(mockBeanFactory, mockClientRegionFactory);

		inOrderVerifier.verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		inOrderVerifier.verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestPool"), eq(Pool.class));
		inOrderVerifier.verify(mockBeanFactory, times(1)).getBean(eq("TestPool"), eq(Pool.class));
		inOrderVerifier.verify(mockClientRegionFactory, times(1)).setPoolName(eq("TestPool"));
		inOrderVerifier.verify(mockBeanFactory, times(1)).containsBean(eq("MockPool"));
		inOrderVerifier.verify(mockBeanFactory, times(1)).isTypeMatch(eq("MockPool"), eq(Pool.class));
		inOrderVerifier.verify(mockBeanFactory, times(1)).getBean(eq("MockPool"), eq(Pool.class));
		inOrderVerifier.verify(mockClientRegionFactory, times(1)).setPoolName(eq("MockPool"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromClientRegionFactoryBeanWhenPoolIsUnresolvable() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName("MockPool");
		factoryBean.configure(mockClientRegionFactory);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPool"));
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromClientRegionFactoryBeanWhenPoolIsDefaultPool() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName(ClientRegionFactoryBean.DEFAULT_POOL_NAME);
		factoryBean.configure(mockClientRegionFactory);

		verify(factoryBean, never()).isPoolResolvable(anyString());
		verify(mockBeanFactory, never()).containsBean(anyString());
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromClientRegionFactoryBeanWhenPoolIsEmpty() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName("");
		factoryBean.configure(mockClientRegionFactory);

		verify(factoryBean, never()).isNotDefaultPool(anyString());
		verify(factoryBean, never()).isPoolResolvable(anyString());
		verify(mockBeanFactory, never()).containsBean(anyString());
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doesNotConfigurePoolFromClientRegionFactoryBeanWhenPoolIsNull() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		ClientRegionFactory<Object, Object> mockClientRegionFactory = mock(ClientRegionFactory.class);

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setPoolName(null);
		factoryBean.configure(mockClientRegionFactory);

		verify(factoryBean, never()).isNotDefaultPool(anyString());
		verify(factoryBean, never()).isPoolResolvable(anyString());
		verify(mockBeanFactory, never()).containsBean(anyString());
		verify(mockBeanFactory, never()).isTypeMatch(anyString(), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(Pool.class));
		verify(mockClientRegionFactory, never()).setPoolName(anyString());
	}

	@Test
	@SuppressWarnings("deprecation")
	public void setDataPolicyName() throws Exception {

		factoryBean.setDataPolicyName("NORMAL");

		assertEquals(DataPolicy.NORMAL, TestUtils.readField("dataPolicy", factoryBean));
	}

	@Test(expected = IllegalArgumentException.class)
	@SuppressWarnings("deprecation")
	public void setDataPolicyNameWithInvalidName() throws Exception {

		try {
			factoryBean.setDataPolicyName("INVALID");
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Data Policy [INVALID] is not valid");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			assertNull(TestUtils.readField("dataPolicy", factoryBean));
		}
	}

	@Test
	public void isPersistentIsCorrect() {

		assertFalse(factoryBean.isPersistent());

		factoryBean.setPersistent(false);

		assertFalse(factoryBean.isPersistent());

		factoryBean.setPersistent(true);

		assertTrue(factoryBean.isPersistent());
	}

	@Test
	public void isPersistentUnspecifiedIsCorrect() {

		assertTrue(factoryBean.isPersistentUnspecified());

		factoryBean.setPersistent(true);

		assertTrue(factoryBean.isPersistent());
		assertFalse(factoryBean.isPersistentUnspecified());

		factoryBean.setPersistent(false);

		assertTrue(factoryBean.isNotPersistent());
		assertFalse(factoryBean.isPersistentUnspecified());
	}

	@Test
	public void isNotPersistentIsCorrect() {

		assertFalse(factoryBean.isNotPersistent());

		factoryBean.setPersistent(true);

		assertFalse(factoryBean.isNotPersistent());

		factoryBean.setPersistent(false);

		assertTrue(factoryBean.isNotPersistent());
	}

	@Test
	public void testCloseDestroySettings() {

		ClientRegionFactoryBean<Object, Object> factory = new ClientRegionFactoryBean<>();

		assertNotNull(factory);
		assertFalse(factory.isClose());
		assertFalse(factory.isDestroy());

		factory.setClose(false);

		assertFalse(factory.isClose());
		assertFalse(factory.isDestroy()); // when destroy is false it remains false even when setClose(false) is called

		factory.setClose(true);

		assertTrue(factory.isClose()); // calling setClose(true) should set close to true
		assertFalse(factory.isDestroy());

		factory.setDestroy(false);

		assertTrue(factory.isClose()); // calling setDestroy(false) should have no affect on close
		assertFalse(factory.isDestroy());

		factory.setDestroy(true);

		assertFalse(factory.isClose()); // setting destroy to true should set close to false
		assertTrue(factory.isDestroy()); // calling setDestroy(true) should set destroy to true

		factory.setClose(false);

		assertFalse(factory.isClose());
		assertTrue(factory.isDestroy()); // calling setClose(false) should have no affect on destroy

		factory.setDestroy(false);

		assertFalse(factory.isClose()); // setting destroy back to false should have no affect on close
		assertFalse(factory.isDestroy());

		factory.setDestroy(true);

		assertFalse(factory.isClose());
		assertTrue(factory.isDestroy());

		factory.setClose(true);

		assertTrue(factory.isClose());
		assertFalse(factory.isDestroy()); // setting close to true should set destroy to false
	}

	@Test
	public void testResolveClientRegionShortcut() throws Exception {

		assertNull(TestUtils.readField("dataPolicy", factoryBean));
		assertNull(TestUtils.readField("persistent", factoryBean));
		assertNull(TestUtils.readField("shortcut", factoryBean));
		assertEquals(ClientRegionShortcut.LOCAL, factoryBean.resolveClientRegionShortcut());
	}

	@Test
	public void testResolveClientRegionShortcutWhenNotPersistent() throws Exception {

		factoryBean.setPersistent(false);

		assertNull(TestUtils.readField("dataPolicy", factoryBean));
		assertTrue(factoryBean.isNotPersistent());
		assertNull(TestUtils.readField("shortcut", factoryBean));
		assertEquals(ClientRegionShortcut.LOCAL, factoryBean.resolveClientRegionShortcut());
	}

	@Test
	public void testResolveClientRegionShortcutWhenPersistent() throws Exception {

		factoryBean.setPersistent(true);

		assertNull(TestUtils.readField("dataPolicy", factoryBean));
		assertTrue(factoryBean.isPersistent());
		assertNull(TestUtils.readField("shortcut", factoryBean));
		assertEquals(ClientRegionShortcut.LOCAL_PERSISTENT, factoryBean.resolveClientRegionShortcut());
	}

	@Test
	public void testResolveClientRegionShortcutUsingShortcut() throws Exception {

		factoryBean.setShortcut(ClientRegionShortcut.CACHING_PROXY_OVERFLOW);

		assertNull(TestUtils.readField("dataPolicy", factoryBean));
		assertNull(TestUtils.readField("persistent", factoryBean));
		assertEquals(ClientRegionShortcut.CACHING_PROXY_OVERFLOW, factoryBean.resolveClientRegionShortcut());
	}

	@Test
	public void testResolveClientRegionShortcutUsingShortcutWhenNotPersistent() throws Exception {

		factoryBean.setPersistent(false);
		factoryBean.setShortcut(ClientRegionShortcut.CACHING_PROXY_HEAP_LRU);

		assertNull(TestUtils.readField("dataPolicy", factoryBean));
		assertTrue(factoryBean.isNotPersistent());
		assertEquals(ClientRegionShortcut.CACHING_PROXY_HEAP_LRU, factoryBean.resolveClientRegionShortcut());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveClientRegionShortcutUsingShortcutWhenPersistent() throws Exception {

		try {
			factoryBean.setPersistent(true);
			factoryBean.setShortcut(ClientRegionShortcut.CACHING_PROXY);

			assertNull(TestUtils.readField("dataPolicy", factoryBean));
			assertTrue(factoryBean.isPersistent());

			factoryBean.resolveClientRegionShortcut();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Client Region Shortcut [CACHING_PROXY] is not valid when persistent is true",
				expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testResolveClientRegionShortcutUsingPersistentShortcut() throws Exception {

		factoryBean.setShortcut(ClientRegionShortcut.LOCAL_PERSISTENT);

		assertNull(TestUtils.readField("dataPolicy", factoryBean));
		assertNull(TestUtils.readField("persistent", factoryBean));
		assertEquals(ClientRegionShortcut.LOCAL_PERSISTENT, factoryBean.resolveClientRegionShortcut());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveClientRegionShortcutUsingPersistentShortcutWhenNotPersistent() throws Exception {

		try {
			factoryBean.setPersistent(false);
			factoryBean.setShortcut(ClientRegionShortcut.LOCAL_PERSISTENT);

			assertNull(TestUtils.readField("dataPolicy", factoryBean));
			assertTrue(factoryBean.isNotPersistent());

			factoryBean.resolveClientRegionShortcut();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Client Region Shortcut [LOCAL_PERSISTENT] is not valid when persistent is false",
				expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testResolveClientRegionShortcutUsingPersistentShortcutWhenPersistent() throws Exception {

		factoryBean.setPersistent(true);
		factoryBean.setShortcut(ClientRegionShortcut.LOCAL_PERSISTENT_OVERFLOW);

		assertNull(TestUtils.readField("dataPolicy", factoryBean));
		assertTrue(factoryBean.isPersistent());
		assertEquals(ClientRegionShortcut.LOCAL_PERSISTENT_OVERFLOW, factoryBean.resolveClientRegionShortcut());
	}

	@Test
	public void testResolveClientRegionShortcutUsingEmptyDataPolicy() throws Exception {

		factoryBean.setDataPolicy(DataPolicy.EMPTY);

		assertNull(TestUtils.readField("persistent", factoryBean));
		assertNull(TestUtils.readField("shortcut", factoryBean));
		assertEquals(ClientRegionShortcut.PROXY, factoryBean.resolveClientRegionShortcut());
	}

	@Test
	public void testResolveClientRegionShortcutUsingNormalDataPolicyWhenNotPersistent() throws Exception {

		factoryBean.setDataPolicy(DataPolicy.NORMAL);
		factoryBean.setPersistent(false);

		assertTrue(factoryBean.isNotPersistent());
		assertNull(TestUtils.readField("shortcut", factoryBean));
		assertEquals(ClientRegionShortcut.CACHING_PROXY, factoryBean.resolveClientRegionShortcut());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveClientRegionShortcutUsingNormalDataPolicyWhenPersistent() throws Exception {

		try {
			factoryBean.setDataPolicy(DataPolicy.NORMAL);
			factoryBean.setPersistent(true);

			assertTrue(factoryBean.isPersistent());
			assertNull(TestUtils.readField("shortcut", factoryBean));

			factoryBean.resolveClientRegionShortcut();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy [NORMAL] is not valid when persistent is true", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testResolveClientRegionShortcutUsingPersistentReplicateDataPolicy() throws Exception {

		factoryBean.setDataPolicy(DataPolicy.PERSISTENT_REPLICATE);

		assertNull(TestUtils.readField("persistent", factoryBean));
		assertNull(TestUtils.readField("shortcut", factoryBean));
		assertEquals(ClientRegionShortcut.LOCAL_PERSISTENT, factoryBean.resolveClientRegionShortcut());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testResolveClientRegionShortcutUsingPersistentReplicateDataPolicyWhenNotPersistent() throws Exception {

		try {
			factoryBean.setDataPolicy(DataPolicy.PERSISTENT_REPLICATE);
			factoryBean.setPersistent(false);

			assertTrue(factoryBean.isNotPersistent());
			assertNull(TestUtils.readField("shortcut", factoryBean));

			factoryBean.resolveClientRegionShortcut();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Data Policy [PERSISTENT_REPLICATE] is not valid when persistent is false", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testResolveClientRegionShortcutUsingPersistentReplicateDataPolicyWhenPersistent() throws Exception {

		factoryBean.setDataPolicy(DataPolicy.PERSISTENT_REPLICATE);
		factoryBean.setPersistent(true);

		assertNull(TestUtils.readField("shortcut", factoryBean));
		assertTrue(factoryBean.isPersistent());
		assertEquals(ClientRegionShortcut.LOCAL_PERSISTENT, factoryBean.resolveClientRegionShortcut());
	}

	@SuppressWarnings("all")
	private <K> Interest<K> newInterest(K key) {
		return new Interest<>(key);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void destroyCallsRegionClose() throws Exception {

		Region mockRegion = mock(Region.class, "MockRegion");

		RegionService mockRegionService = mock(RegionService.class, "MockRegionService");

		when(mockRegion.getRegionService()).thenReturn(mockRegionService);
		when(mockRegionService.isClosed()).thenReturn(false);

		doReturn(mockRegion).when(factoryBean).getObject();

		factoryBean.setClose(true);
		factoryBean.setInterests(ArrayUtils.asArray(newInterest("test")));

		assertThat(factoryBean.isClose()).isTrue();
		assertThat(factoryBean.isDestroy()).isFalse();
		assertThat(factoryBean.getInterests()).isNotNull();
		assertThat(factoryBean.getInterests()).hasSize(1);

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
		verify(mockRegion, times(1)).getRegionService();
		verify(mockRegionService, times(1)).isClosed();
		verify(mockRegion, times(1)).close();
		verify(mockRegion, never()).destroyRegion();
		verify(mockRegion, never()).unregisterInterest(any());
		verify(mockRegion, never()).unregisterInterestRegex(anyString());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void destroyCallsRegionDestroy() throws Exception {

		Region mockRegion = mock(Region.class, "MockRegion");

		RegionService mockRegionService = mock(RegionService.class, "MockRegionService");

		when(mockRegion.getRegionService()).thenReturn(mockRegionService);
		when(mockRegionService.isClosed()).thenReturn(false);

		doReturn(mockRegion).when(factoryBean).getObject();

		factoryBean.setClose(false);
		factoryBean.setDestroy(true);
		factoryBean.setInterests(ArrayUtils.asArray(newInterest("test")));

		assertThat(factoryBean.isClose()).isFalse();
		assertThat(factoryBean.isDestroy()).isTrue();
		assertThat(factoryBean.getInterests()).isNotNull();
		assertThat(factoryBean.getInterests()).hasSize(1);

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
		verify(mockRegion, never()).getRegionService();
		verify(mockRegionService, never()).isClosed();
		verify(mockRegion, never()).close();
		verify(mockRegion, times(1)).destroyRegion();
		verify(mockRegion, never()).unregisterInterest(any());
		verify(mockRegion, never()).unregisterInterestRegex(anyString());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void destroyDoesNothingWhenClientRegionFactoryBeanCloseIsTrueButRegionServiceIsClosed() throws Exception {

		Region mockRegion = mock(Region.class, "MockRegion");

		RegionService mockRegionService = mock(RegionService.class, "MockRegionService");

		when(mockRegion.getRegionService()).thenReturn(mockRegionService);
		when(mockRegionService.isClosed()).thenReturn(true);

		doReturn(mockRegion).when(factoryBean).getObject();

		factoryBean.setClose(true);
		factoryBean.setInterests(ArrayUtils.asArray(newInterest("test")));

		assertThat(factoryBean.isClose()).isTrue();
		assertThat(factoryBean.isDestroy()).isFalse();
		assertThat(factoryBean.getInterests()).isNotNull();
		assertThat(factoryBean.getInterests()).hasSize(1);

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
		verify(mockRegion, times(1)).getRegionService();
		verify(mockRegionService, times(1)).isClosed();
		verify(mockRegion, never()).close();
		verify(mockRegion, never()).destroyRegion();
		verify(mockRegion, never()).unregisterInterest(any());
		verify(mockRegion, never()).unregisterInterestRegex(anyString());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void destroyDoesNothingWhenClientRegionFactoryBeanCloseAndDestroyAreFalse() throws Exception {

		Region mockRegion = mock(Region.class, "MockRegion");

		doReturn(mockRegion).when(factoryBean).getObject();

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
		verify(mockRegion, never()).getRegionService();
		verify(mockRegion, never()).close();
		verify(mockRegion, never()).destroyRegion();
		verify(mockRegion, never()).unregisterInterest(any());
		verify(mockRegion, never()).unregisterInterestRegex(anyString());
	}

	@Test
	public void destroyDoesNothingWhenRegionIsNull() throws Exception {

		doReturn(null).when(factoryBean).getObject();

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
	}
}
