/*
 * Copyright 2016-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.DiskStore;
import com.gemstone.gemfire.cache.FixedPartitionAttributes;
import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.PartitionResolver;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;

import org.junit.After;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.gemfire.config.annotation.test.entities.ClientRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.CollocatedPartitionRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.GenericRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.NonEntity;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.mapping.ClientRegion;
import org.springframework.data.gemfire.mapping.LocalRegion;
import org.springframework.data.gemfire.mapping.PartitionRegion;
import org.springframework.data.gemfire.mapping.ReplicateRegion;
import org.springframework.data.gemfire.support.ClientRegionShortcutWrapper;
import org.springframework.data.gemfire.util.CollectionUtils;

/**
 * Unit tests for the {@link EnableEntityDefinedRegions} annotation and {@link EntityDefinedRegionsConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.EntityDefinedRegionsConfiguration
 * @since 1.9.0
 */
public class EnableEntityDefinedRegionsConfigurationUnitTests {

	private static final AtomicInteger MOCK_ID = new AtomicInteger(0);

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		if (applicationContext != null) {
			applicationContext.close();
		}
	}

	/* (non-Javadoc) */
	protected void assertRegion(Region<?, ?> region, String name) {
		assertRegion(region, name, null, null);
	}

	/* (non-Javadoc) */
	protected <K, V> void assertRegion(Region<?, ?> region, String name,
			Class<K> keyConstraint, Class<V> valueConstraint) {

		assertRegion(region, name, String.format("%1$s%2$s", Region.SEPARATOR, name), keyConstraint, valueConstraint);
	}

	/* (non-Javadoc) */
	protected void assertRegion(Region<?, ?> region, String name, String fullPath) {
		assertRegion(region, name, fullPath, null, null);
	}

	/* (non-Javadoc) */
	protected <K, V> void assertRegion(Region<?, ?> region, String name, String fullPath,
			Class<K> keyConstraint, Class<V> valueConstraint) {

		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(name);
		assertThat(region.getFullPath()).isEqualTo(fullPath);
		assertThat(region.getAttributes()).isNotNull();
		assertThat(region.getAttributes().getKeyConstraint()).isEqualTo(keyConstraint);
		assertThat(region.getAttributes().getValueConstraint()).isEqualTo(valueConstraint);
	}

	/* (non-Javadoc) */
	protected void assertRegionAttributes(RegionAttributes<?, ?> regionAttributes, DataPolicy dataPolicy,
			String diskStoreName, Boolean diskSynchronous, Boolean ignoreJta, String poolName, Scope scope) {

		assertThat(regionAttributes).isNotNull();
		assertThat(regionAttributes.getDataPolicy()).isEqualTo(dataPolicy);
		assertThat(regionAttributes.getDiskStoreName()).isEqualTo(diskStoreName);
		assertThat(regionAttributes.isDiskSynchronous()).isEqualTo(diskSynchronous);
		assertThat(regionAttributes.getIgnoreJTA()).isEqualTo(ignoreJta);
		assertThat(regionAttributes.getPoolName()).isEqualToIgnoringCase(poolName);
		assertThat(regionAttributes.getScope()).isEqualTo(scope);
	}

	/* (non-Javadoc) */
	protected void assertPartitionAttributes(PartitionAttributes<?, ?> partitionAttributes,
			String collocatedWith, PartitionResolver partitionResolver, Integer redundantCopies) {

		assertThat(partitionAttributes).isNotNull();
		assertThat(partitionAttributes.getColocatedWith()).isEqualTo(collocatedWith);
		assertThat(partitionAttributes.getPartitionResolver()).isEqualTo(partitionResolver);
		assertThat(partitionAttributes.getRedundantCopies()).isEqualTo(redundantCopies);
	}

	/* (non-Javadoc) */
	protected void assertFixedPartitionAttributes(FixedPartitionAttributes fixedPartitionAttributes,
			String partitionName, boolean primary, int numBuckets) {

		assertThat(fixedPartitionAttributes).isNotNull();
		assertThat(fixedPartitionAttributes.getPartitionName()).isEqualTo(partitionName);
		assertThat(fixedPartitionAttributes.isPrimary()).isEqualTo(primary);
		assertThat(fixedPartitionAttributes.getNumBuckets()).isEqualTo(numBuckets);
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected FixedPartitionAttributes findFixedPartitionAttributes(PartitionAttributes partitionAttributes,
			String partitionName) {

		assertThat(partitionAttributes).isNotNull();

		List<FixedPartitionAttributes> fixedPartitionAttributes =
			CollectionUtils.nullSafeList(partitionAttributes.getFixedPartitionAttributes());

		for (FixedPartitionAttributes attributes : fixedPartitionAttributes) {
			if (attributes.getPartitionName().equals(partitionName)) {
				return attributes;
			}
		}

		return null;
	}

	/* (non-Javadoc) */
	protected ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	@Test
	@SuppressWarnings("unchecked")
	public void entityClientRegionsDefined() {
		applicationContext = newApplicationContext(ClientPersistentEntitiesConfiguration.class);

		Region<String, ClientRegionEntity> sessions = applicationContext.getBean("Sessions", Region.class);

		assertRegion(sessions, "Sessions", String.class, ClientRegionEntity.class);
		assertRegionAttributes(sessions.getAttributes(), DataPolicy.NORMAL, null, true, false,
			GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME, null);

		Region<Long, GenericRegionEntity> genericRegionEntity =
			applicationContext.getBean("GenericRegionEntity", Region.class);

		assertRegion(genericRegionEntity, "GenericRegionEntity", Long.class, GenericRegionEntity.class);
		assertRegionAttributes(genericRegionEntity.getAttributes(), DataPolicy.EMPTY, null, true, false,
			GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME, null);

		assertThat(applicationContext.containsBean("CollocatedPartitionRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("ContactEvents")).isFalse();
		assertThat(applicationContext.containsBean("NonEntity")).isFalse();
		assertThat(applicationContext.containsBean("Accounts")).isFalse();
		assertThat(applicationContext.containsBean("Customers")).isFalse();
		assertThat(applicationContext.containsBean("LocalRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("PartitionRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("ReplicateRegionEntity")).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void entityPeerPartitionRegionsDefined() {
		applicationContext = newApplicationContext(PeerPartitionRegionPersistentEntitiesConfiguration.class);

		Region<Object, Object> customers = applicationContext.getBean("Customers", Region.class);

		assertRegion(customers, "Customers");
		assertRegionAttributes(customers.getAttributes(), DataPolicy.PERSISTENT_PARTITION, null, true, false, null,
			Scope.DISTRIBUTED_NO_ACK);
		assertPartitionAttributes(customers.getAttributes().getPartitionAttributes(), null, null, 1);
		assertFixedPartitionAttributes(findFixedPartitionAttributes(
			customers.getAttributes().getPartitionAttributes(), "one"), "one", true, 16);
		assertFixedPartitionAttributes(findFixedPartitionAttributes(
			customers.getAttributes().getPartitionAttributes(), "two"), "two", false, 21);

		Region<Object, Object> contactEvents = applicationContext.getBean("ContactEvents", Region.class);

		assertRegion(contactEvents, "ContactEvents");
		assertRegionAttributes(contactEvents.getAttributes(), DataPolicy.PERSISTENT_PARTITION, "mockDiskStore",
			false, true, null, Scope.DISTRIBUTED_NO_ACK);
		assertPartitionAttributes(contactEvents.getAttributes().getPartitionAttributes(), "Customers",
			applicationContext.getBean("mockPartitionResolver", PartitionResolver.class), 2);

		assertThat(applicationContext.getBean("mockDiskStore")).isInstanceOf(DiskStore.class);
		assertThat(applicationContext.containsBean("ClientRegion")).isFalse();
		assertThat(applicationContext.containsBean("NonEntity")).isFalse();
		assertThat(applicationContext.containsBean("Accounts")).isFalse();
		assertThat(applicationContext.containsBean("LocalRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("PartitionRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("ReplicateRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("Sessions")).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void entityServerRegionsDefined() {
		applicationContext = newApplicationContext(AllServerPersistentEntitiesConfiguration.class);

		Region<Object, Object> accounts = applicationContext.getBean("Accounts", Region.class);

		assertRegion(accounts, "Accounts");
		assertRegionAttributes(accounts.getAttributes(), DataPolicy.REPLICATE, null, true, false, null,
			Scope.DISTRIBUTED_ACK);

		Region<Object, Object> customers = applicationContext.getBean("Customers", Region.class);

		assertRegion(customers, "Customers");
		assertRegionAttributes(customers.getAttributes(), DataPolicy.PERSISTENT_PARTITION, null, true, false, null,
			Scope.DISTRIBUTED_NO_ACK);
		assertPartitionAttributes(customers.getAttributes().getPartitionAttributes(), null, null, 1);

		Region<Object, Object> localRegionEntity = applicationContext.getBean("LocalRegionEntity", Region.class);

		assertRegion(localRegionEntity, "LocalRegionEntity");
		assertRegionAttributes(localRegionEntity.getAttributes(), DataPolicy.NORMAL,
			null, true, false, null, Scope.LOCAL);

		Region<Object, Object> genericRegionEntity = applicationContext.getBean("GenericRegionEntity", Region.class);

		assertRegion(genericRegionEntity, "GenericRegionEntity");
		assertRegionAttributes(genericRegionEntity.getAttributes(), DataPolicy.NORMAL, null, true, false, null,
			Scope.DISTRIBUTED_NO_ACK);

		assertThat(applicationContext.containsBean("CollocatedPartitionRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("ContactEvents")).isFalse();
		assertThat(applicationContext.containsBean("NonEntity")).isFalse();
		assertThat(applicationContext.containsBean("PartitionRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("ReplicateRegionEntity")).isFalse();
		assertThat(applicationContext.containsBean("Sessions")).isFalse();
	}

	/* (non-Javadoc) */
	protected static String mockName(String baseMockName) {
		return String.format("%s%d", baseMockName, MOCK_ID.incrementAndGet());
	}

	/* (non-Javadoc) */
	protected static <K, V> Cache mockCache() {
		Cache mockCache = mock(Cache.class);

		Answer<RegionFactory<K, V>> createRegionFactory = new Answer<RegionFactory<K, V>>() {
			@Override @SuppressWarnings("unchecked")
			public RegionFactory<K, V> answer(InvocationOnMock invocation) throws Throwable {
				RegionAttributes<K, V> defaultRegionAttributes =
					mockRegionAttributes(null, null, true, false, null, null, null, Scope.DISTRIBUTED_NO_ACK, null);

				RegionAttributes<K, V> regionAttributes = (invocation.getArguments().length == 1
					? invocation.getArgumentAt(0, RegionAttributes.class) : defaultRegionAttributes);

				return mockRegionFactory(regionAttributes);
			}
		};

		when(mockCache.createRegionFactory()).thenAnswer(createRegionFactory);
		when(mockCache.createRegionFactory(any(RegionAttributes.class))).thenAnswer(createRegionFactory);

		return mockCache;
	}

	protected static <K, V> ClientCache mockClientCache() {
		ClientCache mockClientCache = mock(ClientCache.class, mockName("ClientCache"));

		Answer<ClientRegionFactory<K, V>> createClientRegionFactory = new Answer<ClientRegionFactory<K, V>>() {
			@Override @SuppressWarnings("unchecked")
			public ClientRegionFactory<K, V> answer(InvocationOnMock invocation) throws Throwable {
				return mockClientRegionFactory(invocation.getArgumentAt(0, ClientRegionShortcut.class));
			}
		};

		when(mockClientCache.createClientRegionFactory(any(ClientRegionShortcut.class)))
			.thenAnswer(createClientRegionFactory);

		return mockClientCache;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected static <K, V> ClientRegionFactory<K, V> mockClientRegionFactory(ClientRegionShortcut shortcut) {
		ClientRegionFactory<K, V> mockClientRegionFactory =
			mock(ClientRegionFactory.class, mockName("MockClientRegionFactory"));

		AtomicReference<String> diskStoreName = new AtomicReference<String>();
		AtomicReference<Boolean> diskSynchronous = new AtomicReference<Boolean>(true);
		AtomicReference<Class> keyConstraint = new AtomicReference<Class>(null);
		AtomicReference<String> poolName = new AtomicReference<String>();
		AtomicReference<Class> valueConstraint = new AtomicReference<Class>(null);

		when(mockClientRegionFactory.setDiskStoreName(anyString())).thenAnswer(
			newSetter(String.class, diskStoreName, mockClientRegionFactory));

		when(mockClientRegionFactory.setDiskSynchronous(anyBoolean())).thenAnswer(
			newSetter(Boolean.TYPE, diskSynchronous, mockClientRegionFactory));

		when(mockClientRegionFactory.setKeyConstraint(any(Class.class))).thenAnswer(
			newSetter(Class.class, keyConstraint, mockClientRegionFactory));

		when(mockClientRegionFactory.setPoolName(anyString())).thenAnswer(
			newSetter(String.class, poolName, mockClientRegionFactory));

		when(mockClientRegionFactory.setValueConstraint(any(Class.class))).thenAnswer(
			newSetter(Class.class, valueConstraint, mockClientRegionFactory));

		final RegionAttributes<K, V> mockRegionAttributes =
			mock(RegionAttributes.class, mockName("MockClientRegionAttributes"));

		when(mockRegionAttributes.getDataPolicy()).thenReturn(
			ClientRegionShortcutWrapper.valueOf(shortcut).getDataPolicy());
		when(mockRegionAttributes.getDiskStoreName()).thenAnswer(newGetter(diskStoreName));
		when(mockRegionAttributes.isDiskSynchronous()).thenAnswer(newGetter(diskSynchronous));
		when(mockRegionAttributes.getKeyConstraint()).thenAnswer(newGetter(keyConstraint));
		when(mockRegionAttributes.getPoolName()).thenAnswer(newGetter(poolName));
		when(mockRegionAttributes.getValueConstraint()).thenAnswer(newGetter(valueConstraint));

		when(mockClientRegionFactory.create(anyString())).thenAnswer(new Answer<Region<K, V>>() {
			@Override
			public Region<K, V> answer(InvocationOnMock invocation) throws Throwable {
				return mockRegion(invocation.getArgumentAt(0, String.class), mockRegionAttributes);
			}
		});

		return mockClientRegionFactory;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected static <K, V> RegionAttributes<K, V> mockRegionAttributes(DataPolicy dataPolicy,
			String diskStoreName, boolean diskSynchronous, boolean ignoreJta, Class<K> keyConstraint,
			PartitionAttributes<K, V> partitionAttributes, String poolName, Scope scope, Class<V> valueConstraint) {

		RegionAttributes<K, V> mockRegionAttributes = mock(RegionAttributes.class, mockName("MockRegionAttributes"));

		when(mockRegionAttributes.getDataPolicy()).thenReturn(dataPolicy);
		when(mockRegionAttributes.getDiskStoreName()).thenReturn(diskStoreName);
		when(mockRegionAttributes.isDiskSynchronous()).thenReturn(diskSynchronous);
		when(mockRegionAttributes.getIgnoreJTA()).thenReturn(ignoreJta);
		when(mockRegionAttributes.getKeyConstraint()).thenReturn(keyConstraint);
		when(mockRegionAttributes.getPartitionAttributes()).thenReturn(partitionAttributes);
		when(mockRegionAttributes.getPoolName()).thenReturn(poolName);
		when(mockRegionAttributes.getScope()).thenReturn(scope);
		when(mockRegionAttributes.getValueConstraint()).thenReturn(valueConstraint);

		return mockRegionAttributes;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected static <K, V> RegionFactory<K, V> mockRegionFactory(RegionAttributes<K, V> regionAttributes) {
		RegionFactory<K, V> mockRegionFactory = mock(RegionFactory.class, mockName("MockRegionFactory"));

		AtomicReference<DataPolicy> dataPolicy = new AtomicReference<DataPolicy>(regionAttributes.getDataPolicy());
		AtomicReference<String> diskStoreName = new AtomicReference<String>(regionAttributes.getDiskStoreName());
		AtomicReference<Boolean> diskSynchronous = new AtomicReference<Boolean>(regionAttributes.isDiskSynchronous());
		AtomicReference<Boolean> ignoreJta = new AtomicReference<Boolean>(regionAttributes.getIgnoreJTA());
		AtomicReference<Class> keyConstraint = new AtomicReference<Class>(null);
		AtomicReference<PartitionAttributes> partitionAttributes = new AtomicReference<PartitionAttributes>(
			regionAttributes.getPartitionAttributes());
		AtomicReference<Scope> scope = new AtomicReference<Scope>(regionAttributes.getScope());
		AtomicReference<Class> valueConstraint = new AtomicReference<Class>(null);

		when(mockRegionFactory.setDataPolicy(any(DataPolicy.class))).thenAnswer(
			newSetter(DataPolicy.class, dataPolicy, mockRegionFactory));

		when(mockRegionFactory.setDiskStoreName(anyString())).thenAnswer(
			newSetter(String.class, diskStoreName, mockRegionFactory));

		when(mockRegionFactory.setDiskSynchronous(anyBoolean())).thenAnswer(
			newSetter(Boolean.TYPE, diskSynchronous, mockRegionFactory));

		when(mockRegionFactory.setIgnoreJTA(anyBoolean())).thenAnswer(
			newSetter(Boolean.TYPE, ignoreJta, mockRegionFactory));

		when(mockRegionFactory.setKeyConstraint(any(Class.class))).thenAnswer(
			newSetter(Class.class, keyConstraint, mockRegionFactory));

		when(mockRegionFactory.setPartitionAttributes(any(PartitionAttributes.class))).thenAnswer(
			newSetter(PartitionAttributes.class, partitionAttributes, mockRegionFactory));

		when(mockRegionFactory.setScope(any(Scope.class))).thenAnswer(
			newSetter(Scope.class, scope, mockRegionFactory));

		when(mockRegionFactory.setValueConstraint(any(Class.class))).thenAnswer(
			newSetter(Class.class, valueConstraint, mockRegionFactory));

		final RegionAttributes<K, V> mockRegionAttributes =
			mock(RegionAttributes.class, mockName("MockRegionAttributes"));

		when(mockRegionAttributes.getDataPolicy()).thenAnswer(newGetter(dataPolicy));
		when(mockRegionAttributes.getDiskStoreName()).thenAnswer(newGetter(diskStoreName));
		when(mockRegionAttributes.isDiskSynchronous()).thenAnswer(newGetter(diskSynchronous));
		when(mockRegionAttributes.getIgnoreJTA()).thenAnswer(newGetter(ignoreJta));
		when(mockRegionAttributes.getKeyConstraint()).thenAnswer(newGetter(keyConstraint));
		when(mockRegionAttributes.getPartitionAttributes()).thenAnswer(newGetter(partitionAttributes));
		when(mockRegionAttributes.getScope()).thenAnswer(newGetter(scope));
		when(mockRegionAttributes.getValueConstraint()).thenAnswer(newGetter(valueConstraint));

		when(mockRegionFactory.create(anyString())).thenAnswer(new Answer<Region<K, V>>() {
			@Override
			public Region<K, V> answer(InvocationOnMock invocation) throws Throwable {
				return mockRegion(invocation.getArgumentAt(0, String.class), mockRegionAttributes);
			}
		});

		return mockRegionFactory;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected static <K, V> Region<K, V> mockRegion(String name, RegionAttributes<K, V> regionAttributes) {
		Region<K, V> mockRegion = mock(Region.class, mockName(name));

		when(mockRegion.getName()).thenReturn(name);
		when(mockRegion.getFullPath()).thenReturn(String.format("%1$s%2$s", Region.SEPARATOR, name));
		when(mockRegion.getAttributes()).thenReturn(regionAttributes);

		return mockRegion;
	}

	/* (non-Javadoc) */
	protected static <R> Answer<R> newGetter(final AtomicReference<R> returnValue) {
		return new Answer<R>() {
			@Override
			public R answer(InvocationOnMock invocation) throws Throwable {
				return returnValue.get();
			}
		};
	}

	/* (non-Javadoc) */
	protected static <T, R> Answer<R> newSetter(final Class<T> parameterType, final AtomicReference<T> argument,
			final R returnValue) {

		return new Answer<R>() {
			@Override
			public R answer(InvocationOnMock invocation) throws Throwable {
				argument.set(invocation.getArgumentAt(0, parameterType));
				return returnValue;
			}
		};
	}

	@Configuration
	@SuppressWarnings("unused")
	static abstract class ClientCacheConfiguration {

		@Bean
		ClientCache gemfireCache() {
			return mockClientCache();
		}
	}

	@Configuration
	@SuppressWarnings("unused")
	static abstract class ServerCacheConfiguration {

		@Bean
		Cache gemfireCache() {
			return mockCache();
		}
	}

	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = { @ComponentScan.Filter(type = FilterType.ANNOTATION, classes = ClientRegion.class),
			@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = CollocatedPartitionRegionEntity.class) })
	@SuppressWarnings("all")
	static class AllServerPersistentEntitiesConfiguration extends ServerCacheConfiguration {
	}

	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class, strict = true,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ANNOTATION,
			classes = { LocalRegion.class, PartitionRegion.class, ReplicateRegion.class }))
	@SuppressWarnings("all")
	static class ClientPersistentEntitiesConfiguration extends ClientCacheConfiguration {
	}

	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ANNOTATION,
			classes = { ClientRegion.class, LocalRegion.class, ReplicateRegion.class }))
	@SuppressWarnings("all")
	static class PeerPartitionRegionPersistentEntitiesConfiguration extends ServerCacheConfiguration {

		@Bean @Lazy
		DiskStore mockDiskStore() {
			return mock(DiskStore.class, mockName("MockDiskStore"));
		}

		@Bean @Lazy
		PartitionResolver mockPartitionResolver() {
			return mock(PartitionResolver.class, mockName("MockPartitionResolver"));
		}
	}
}
