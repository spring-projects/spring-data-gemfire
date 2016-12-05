/*
 * Copyright 2016 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.config.annotation.EnableEviction.EvictionPolicy;

import java.util.concurrent.atomic.AtomicReference;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.util.ObjectSizer;

import org.junit.After;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.EvictionActionType;
import org.springframework.data.gemfire.EvictionAttributesFactoryBean;
import org.springframework.data.gemfire.EvictionPolicyType;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * Unit tests for the {@link EnableEviction} annotation and {@link EvictionConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.annotation.EnableEviction
 * @see org.springframework.data.gemfire.config.annotation.EvictionConfiguration
 * @see org.springframework.data.gemfire.EvictionAttributesFactoryBean
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.EvictionAttributes
 * @since 1.9.0
 */
public class EnableEvictionConfigurationUnitTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		if (applicationContext != null) {
			applicationContext.close();
		}
	}

	protected void assertEvictionAttributes(Region region, EvictionAttributes expectedEvictionAttributes) {
		assertThat(region).isNotNull();
		assertThat(region.getAttributes()).isNotNull();
		assertEvictionAttributes(region.getAttributes().getEvictionAttributes(), expectedEvictionAttributes);
	}

	protected void assertEvictionAttributes(EvictionAttributes actualEvictionAttributes,
		EvictionAttributes expectedEvictionAttributes) {

		assertThat(actualEvictionAttributes).isNotNull();
		assertThat(actualEvictionAttributes.getAction()).isEqualTo(expectedEvictionAttributes.getAction());
		assertThat(actualEvictionAttributes.getAlgorithm()).isEqualTo(expectedEvictionAttributes.getAlgorithm());
		assertThat(actualEvictionAttributes.getObjectSizer()).isEqualTo(expectedEvictionAttributes.getObjectSizer());

		if (!EvictionPolicyType.HEAP_PERCENTAGE.equals(
				EvictionPolicyType.valueOf(actualEvictionAttributes.getAlgorithm()))) {
			assertThat(actualEvictionAttributes.getMaximum()).isEqualTo(expectedEvictionAttributes.getMaximum());
		}
	}

	@SuppressWarnings("unchecked")
	protected <K, V> Region<K, V> getRegion(String beanName) {
		return applicationContext.getBean(beanName, Region.class);
	}

	protected AnnotationConfigApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	protected EvictionAttributes newEvictionAttributes(Integer maximum, EvictionPolicyType type, EvictionActionType action,
			ObjectSizer... objectSizer) {

		EvictionAttributesFactoryBean evictionAttributesFactory = new EvictionAttributesFactoryBean();

		evictionAttributesFactory.setAction(action.getEvictionAction());
		evictionAttributesFactory.setObjectSizer(ArrayUtils.getFirst(objectSizer));
		evictionAttributesFactory.setThreshold(maximum);
		evictionAttributesFactory.setType(type);
		evictionAttributesFactory.afterPropertiesSet();

		return evictionAttributesFactory.getObject();
	}

	@Test
	public void usesDefaultEvictionPolicyConfiguration() {
		applicationContext = newApplicationContext(DefaultEvictionPolicyConfiguration.class);

		EvictionAttributes defaultEvictionAttributes = EvictionAttributes.createLRUEntryAttributes();

		assertEvictionAttributes(applicationContext.getBean("PartitionRegion", Region.class), defaultEvictionAttributes);
		assertEvictionAttributes(applicationContext.getBean("ReplicateRegion", Region.class), defaultEvictionAttributes);
	}

	@Test
	public void usesCustomEvictionPolicyConfiguration() {
		applicationContext = newApplicationContext(CustomEvictionPolicyConfiguration.class);

		ObjectSizer mockObjectSizer = applicationContext.getBean("mockObjectSizer", ObjectSizer.class);

		EvictionAttributes customEvictionAttributes = newEvictionAttributes(65536, EvictionPolicyType.MEMORY_SIZE,
			EvictionActionType.OVERFLOW_TO_DISK, mockObjectSizer);

		assertEvictionAttributes(applicationContext.getBean("PartitionRegion", Region.class), customEvictionAttributes);
		assertEvictionAttributes(applicationContext.getBean("ReplicateRegion", Region.class), customEvictionAttributes);
	}

	@Test
	public void usesRegionSpecificEvictionPolicyConfiguration() {
		applicationContext = newApplicationContext(RegionSpecificEvictionPolicyConfiguration.class);

		ObjectSizer mockObjectSizer = applicationContext.getBean("mockObjectSizer", ObjectSizer.class);

		EvictionAttributes partitionRegionEvictionAttributes = newEvictionAttributes(null,
			EvictionPolicyType.HEAP_PERCENTAGE, EvictionActionType.OVERFLOW_TO_DISK, mockObjectSizer);

		EvictionAttributes replicateRegionEvictionAttributes = newEvictionAttributes(10000,
			EvictionPolicyType.ENTRY_COUNT, EvictionActionType.LOCAL_DESTROY);

		assertEvictionAttributes(applicationContext.getBean("PartitionRegion", Region.class),
			partitionRegionEvictionAttributes);

		assertEvictionAttributes(applicationContext.getBean("ReplicateRegion", Region.class),
			replicateRegionEvictionAttributes);
	}

	@Test
	public void usesLastMatchingEvictionPolicyConfiguration() {
		applicationContext = newApplicationContext(LastMatchingWinsEvictionPolicyConfiguration.class);

		EvictionAttributes lastMatchingEvictionAttributes = newEvictionAttributes(99, EvictionPolicyType.ENTRY_COUNT,
			EvictionActionType.OVERFLOW_TO_DISK);

		assertEvictionAttributes(applicationContext.getBean("PartitionRegion", Region.class),
			lastMatchingEvictionAttributes);

		assertEvictionAttributes(applicationContext.getBean("ReplicateRegion", Region.class),
			lastMatchingEvictionAttributes);
	}

	@Configuration
	@SuppressWarnings("unused")
	static class CacheRegionConfiguration {

		@Bean("mockCache")
		@SuppressWarnings("unchecked")
		Cache mockCache() {
			Cache mockCache = mock(Cache.class);

			RegionFactory mockRegionFactory = mock(RegionFactory.class);

			final AtomicReference<EvictionAttributes> evictionAttributes =
				new AtomicReference<EvictionAttributes>(null);

			when(mockCache.createRegionFactory()).thenReturn(mockRegionFactory);

			when(mockRegionFactory.setEvictionAttributes(any(EvictionAttributes.class))).thenAnswer(
				new Answer<RegionFactory>() {
					@Override
					public RegionFactory answer(InvocationOnMock invocation) throws Throwable {
						evictionAttributes.set(invocation.getArgumentAt(0, EvictionAttributes.class));
						return (RegionFactory) invocation.getMock();
					}
				}
			);

			when(mockRegionFactory.create(anyString())).thenAnswer(new Answer<Region>() {
				@Override
				public Region answer(InvocationOnMock invocation) throws Throwable {
					String regionName = invocation.getArgumentAt(0, String.class);

					Region mockRegion = mock(Region.class, regionName);

					RegionAttributes mockRegionAttributes =
						mock(RegionAttributes.class, regionName.concat("Attributes"));

					when(mockRegion.getName()).thenReturn(regionName);
					when(mockRegion.getFullPath()).thenReturn(String.format("%1$s%2$s", Region.SEPARATOR, regionName));
					when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
					when(mockRegionAttributes.getEvictionAttributes()).thenReturn(evictionAttributes.get());

					return mockRegion;
				}
			});

			return mockCache;
		}

		@Bean("PartitionRegion")
		PartitionedRegionFactoryBean<Object, Object> mockPartitionRegion(Cache gemfireCache) {
			PartitionedRegionFactoryBean<Object, Object> partitionRegion =
				new PartitionedRegionFactoryBean<Object, Object>();

			partitionRegion.setCache(gemfireCache);
			partitionRegion.setClose(false);
			partitionRegion.setPersistent(false);

			return partitionRegion;
		}

		@Bean("ReplicateRegion")
		ReplicatedRegionFactoryBean<Object, Object> mockReplicateRegion(Cache gemfireCache) {
			ReplicatedRegionFactoryBean<Object, Object> replicateRegion =
				new ReplicatedRegionFactoryBean<Object, Object>();

			replicateRegion.setCache(gemfireCache);
			replicateRegion.setClose(false);
			replicateRegion.setPersistent(false);

			return replicateRegion;
		}

		@Bean
		ObjectSizer mockObjectSizer() {
			return mock(ObjectSizer.class);
		}
	}

	@EnableEviction
	static class DefaultEvictionPolicyConfiguration extends CacheRegionConfiguration {
	}

	@EnableEviction(policies = @EvictionPolicy(maximum = 65536, type = EvictionPolicyType.MEMORY_SIZE,
		action = EvictionActionType.OVERFLOW_TO_DISK, objectSizerName = "mockObjectSizer"))
	static class CustomEvictionPolicyConfiguration extends CacheRegionConfiguration {
	}

	@EnableEviction(policies = {
		@EvictionPolicy(maximum = 85, type = EvictionPolicyType.HEAP_PERCENTAGE, action = EvictionActionType.OVERFLOW_TO_DISK,
			objectSizerName = "mockObjectSizer", regionNames = "PartitionRegion"),
		@EvictionPolicy(maximum = 10000, type = EvictionPolicyType.ENTRY_COUNT, action = EvictionActionType.LOCAL_DESTROY,
			regionNames = "ReplicateRegion")
	})
	static class RegionSpecificEvictionPolicyConfiguration extends CacheRegionConfiguration {
	}

	@EnableEviction(policies = {
		@EvictionPolicy(maximum = 1, type = EvictionPolicyType.ENTRY_COUNT, action = EvictionActionType.LOCAL_DESTROY,
			objectSizerName = "mockObjectSizer", regionNames = "ReplicateRegion"),
		@EvictionPolicy(maximum = 99, type = EvictionPolicyType.ENTRY_COUNT, action = EvictionActionType.OVERFLOW_TO_DISK)
	})
	static class LastMatchingWinsEvictionPolicyConfiguration extends CacheRegionConfiguration {
	}
}
