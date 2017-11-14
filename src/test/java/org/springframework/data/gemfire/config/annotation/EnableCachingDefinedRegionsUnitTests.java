/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newRuntimeException;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;

import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.test.support.MapBuilder;
import org.springframework.stereotype.Service;

/**
 * Unit tests for {@link EnableCachingDefinedRegions} and {@link CachingDefinedRegionsConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Region
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.cache.annotation.Cacheable
 * @see org.springframework.cache.annotation.CacheEvict
 * @see org.springframework.cache.annotation.CachePut
 * @see org.springframework.cache.annotation.Caching
 * @see org.springframework.stereotype.Service
 * @since 2.0.0
 */
public class EnableCachingDefinedRegionsUnitTests {

	// Subject Under Test (SUT)
	private CachingDefinedRegionsConfiguration configuration;

	@Before
	public void setup() {
		this.configuration = new CachingDefinedRegionsConfiguration();
	}

	@SuppressWarnings("unchecked")
	private BeanDefinition mockBeanDefinition(Class<?> beanClass) {

		try {

			AbstractBeanDefinition mockBeanDefinition = mock(AbstractBeanDefinition.class, beanClass.getSimpleName());

			when(mockBeanDefinition.getBeanClassName()).thenReturn(beanClass.getName());
			when(mockBeanDefinition.resolveBeanClass(any(ClassLoader.class))).thenReturn((Class) beanClass);
			when(mockBeanDefinition.getRole()).thenReturn(BeanDefinition.ROLE_APPLICATION);

			return mockBeanDefinition;
		}
		catch (ClassNotFoundException cause) {
			throw newRuntimeException(cause, "Mock for class [%s] failed", beanClass.getName());
		}
	}

	private BeanDefinitionRegistry mockBeanDefinitionRegistry(Map<String, BeanDefinition> registeredBeanDefinitions) {

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mock(BeanDefinitionRegistry.class);

		when(mockBeanDefinitionRegistry.getBeanDefinitionNames())
			.thenReturn(registeredBeanDefinitions.keySet().toArray(new String[registeredBeanDefinitions.size()]));

		when(mockBeanDefinitionRegistry.getBeanDefinition(anyString()))
			.thenAnswer(invocation -> registeredBeanDefinitions.get(invocation.<String>getArgument(0)));

		when(mockBeanDefinitionRegistry.containsBeanDefinition(anyString()))
			.thenAnswer(invocation -> registeredBeanDefinitions.containsKey(invocation.<String>getArgument(0)));

		doAnswer(invocation -> registeredBeanDefinitions.put(invocation.getArgument(0), invocation.getArgument(1)))
			.when(mockBeanDefinitionRegistry).registerBeanDefinition(anyString(), any(BeanDefinition.class));

		return mockBeanDefinitionRegistry;
	}

	@Test
	public void setGetAndResolveClientRegionShortcut() {

		assertThat(this.configuration.getClientRegionShortcut().orElse(null))
			.isEqualTo(ClientRegionShortcut.PROXY);
		assertThat(this.configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.PROXY);

		this.configuration.setClientRegionShortcut(ClientRegionShortcut.LOCAL);

		assertThat(this.configuration.getClientRegionShortcut().orElse(null))
			.isEqualTo(ClientRegionShortcut.LOCAL);
		assertThat(this.configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.LOCAL);

		this.configuration.setClientRegionShortcut(null);

		assertThat(this.configuration.getClientRegionShortcut().orElse(null)).isNull();
		assertThat(this.configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.PROXY);

		this.configuration.setClientRegionShortcut(ClientRegionShortcut.CACHING_PROXY);

		assertThat(this.configuration.getClientRegionShortcut().orElse(null))
			.isEqualTo(ClientRegionShortcut.CACHING_PROXY);
		assertThat(this.configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.CACHING_PROXY);
	}

	@Test
	public void setGetAndResolvePoolName() {

		assertThat(this.configuration.getPoolName().orElse(null))
			.isEqualTo(ClientRegionFactoryBean.DEFAULT_POOL_NAME);
		assertThat(this.configuration.resolvePoolName()).isEqualTo(ClientRegionFactoryBean.DEFAULT_POOL_NAME);

		this.configuration.setPoolName(null);

		assertThat(this.configuration.getPoolName().orElse(null)).isNull();
		assertThat(this.configuration.resolvePoolName()).isEqualTo(ClientRegionFactoryBean.DEFAULT_POOL_NAME);

		this.configuration.setPoolName("TestPool");

		assertThat(this.configuration.getPoolName().orElse(null)).isEqualTo("TestPool");
		assertThat(this.configuration.resolvePoolName()).isEqualTo("TestPool");
	}

	@Test
	public void setGetAndResolveServerRegionShortcut() {

		assertThat(this.configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.PARTITION);
		assertThat(this.configuration.resolveServerRegionShortcut()).isEqualTo(RegionShortcut.PARTITION);

		this.configuration.setServerRegionShortcut(RegionShortcut.LOCAL);

		assertThat(this.configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.LOCAL);
		assertThat(this.configuration.resolveServerRegionShortcut()).isEqualTo(RegionShortcut.LOCAL);

		this.configuration.setServerRegionShortcut(null);

		assertThat(this.configuration.getServerRegionShortcut().orElse(null)).isNull();
		assertThat(this.configuration.resolveServerRegionShortcut()).isEqualTo(RegionShortcut.PARTITION);

		this.configuration.setServerRegionShortcut(RegionShortcut.REPLICATE);

		assertThat(this.configuration.getServerRegionShortcut().orElse(null)).isEqualTo(RegionShortcut.REPLICATE);
		assertThat(this.configuration.resolveServerRegionShortcut()).isEqualTo(RegionShortcut.REPLICATE);
	}

	@Test
	public void setImportMetadataConfiguresClientRegionShortcutPoolNameAndServerRegionShortcut() {

		AnnotationMetadata mockAnnotationMetadata = mock(AnnotationMetadata.class);

		Map<String, Object> annotationAttributes = MapBuilder.<String, Object>newMapBuilder()
			.put("clientRegionShortcut", ClientRegionShortcut.LOCAL_PERSISTENT)
			.put("poolName", "SwimmingPool")
			.put("serverRegionShortcut", RegionShortcut.PARTITION_PERSISTENT)
			.build();

		when(mockAnnotationMetadata.hasAnnotation(eq(EnableCachingDefinedRegions.class.getName()))).thenReturn(true);
		when(mockAnnotationMetadata.getAnnotationAttributes(eq(EnableCachingDefinedRegions.class.getName())))
			.thenReturn(annotationAttributes);

		this.configuration.setImportMetadata(mockAnnotationMetadata);

		assertThat(this.configuration.resolveClientRegionShortcut()).isEqualTo(ClientRegionShortcut.LOCAL_PERSISTENT);
		assertThat(this.configuration.resolvePoolName()).isEqualTo("SwimmingPool");
		assertThat(this.configuration.resolveServerRegionShortcut()).isEqualTo(RegionShortcut.PARTITION_PERSISTENT);

		verify(mockAnnotationMetadata, times(1))
			.hasAnnotation(eq(EnableCachingDefinedRegions.class.getName()));

		verify(mockAnnotationMetadata, times(1))
			.getAnnotationAttributes(eq(EnableCachingDefinedRegions.class.getName()));
	}

	@Test
	public void cacheableServiceOneRegistersRegionsOneAndTwo() {

		Map<String, BeanDefinition> registeredBeanDefinitions = MapBuilder.<String, BeanDefinition>newMapBuilder()
			.put("cacheableServiceOne", mockBeanDefinition(CacheableServiceOne.class))
			.build();

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mockBeanDefinitionRegistry(registeredBeanDefinitions);

		this.configuration.registerBeanDefinitions(mockBeanDefinitionRegistry);

		verify(mockBeanDefinitionRegistry, times(1)).getBeanDefinitionNames();

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceOne"));

		verify(mockBeanDefinitionRegistry, times(2))
			.registerBeanDefinition(anyString(), any(BeanDefinition.class));

		Arrays.asList("RegionOne", "RegionTwo").forEach(beanName -> {
			verify(mockBeanDefinitionRegistry, times(1)).containsBeanDefinition(eq(beanName));
			verify(mockBeanDefinitionRegistry, times(1))
				.registerBeanDefinition(eq(beanName), any(BeanDefinition.class));
		});
	}

	@Test
	public void cacheableServiceTwoRegistersRegionsTwoThreeFourFiveAndSix() {

		Map<String, BeanDefinition> registeredBeanDefinitions = MapBuilder.<String, BeanDefinition>newMapBuilder()
			.put("cacheableServiceTwo", mockBeanDefinition(CacheableServiceTwo.class))
			.build();

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mockBeanDefinitionRegistry(registeredBeanDefinitions);

		this.configuration.registerBeanDefinitions(mockBeanDefinitionRegistry);

		Set<String> registeredRegionBeanNames =
			asSet("RegionTwo", "RegionThree", "RegionFour", "RegionFive", "RegionSix");

		verify(mockBeanDefinitionRegistry, times(1)).getBeanDefinitionNames();

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceTwo"));

		verify(mockBeanDefinitionRegistry, times(registeredRegionBeanNames.size()))
			.registerBeanDefinition(anyString(), any(BeanDefinition.class));

		registeredRegionBeanNames.forEach(beanName -> {
			verify(mockBeanDefinitionRegistry, times(1)).containsBeanDefinition(eq(beanName));
			verify(mockBeanDefinitionRegistry, times(1))
				.registerBeanDefinition(eq(beanName), any(BeanDefinition.class));
		});

	}

	@Test
	public void cacheableServiceThreeRegistersSeventeenRegionBeans() {

		Map<String, BeanDefinition> registeredBeanDefinitions = MapBuilder.<String, BeanDefinition>newMapBuilder()
			.put("cacheableServiceThree", mockBeanDefinition(CacheableServiceThree.class))
			.build();

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mockBeanDefinitionRegistry(registeredBeanDefinitions);

		this.configuration.registerBeanDefinitions(mockBeanDefinitionRegistry);

		Set<String> registeredRegionBeanNames =
			asSet("RegionSix", "RegionSeven", "RegionEight", "RegionNine", "RegionTen", "RegionEleven",
				"RegionTwelve", "RegionThirteen", "RegionFourteen", "RegionFifteen", "RegionSixteen", "RegionSeventeen",
				"RegionEighteen", "RegionNineteen", "RegionTwenty", "RegionTwentyOne", "RegionTwentyFive");

		verify(mockBeanDefinitionRegistry, times(1)).getBeanDefinitionNames();

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceThree"));

		verify(mockBeanDefinitionRegistry, times(registeredRegionBeanNames.size()))
			.registerBeanDefinition(anyString(), any(BeanDefinition.class));

		registeredRegionBeanNames.forEach(beanName -> {
			verify(mockBeanDefinitionRegistry, times(1)).containsBeanDefinition(eq(beanName));
			verify(mockBeanDefinitionRegistry, times(1))
				.registerBeanDefinition(eq(beanName), any(BeanDefinition.class));
		});
	}

	@Test
	public void cacheableServiceFourRegistersNineteenRegionBeans() {

		Map<String, BeanDefinition> registeredBeanDefinitions = MapBuilder.<String, BeanDefinition>newMapBuilder()
			.put("cacheableServiceFour", mockBeanDefinition(CacheableServiceFour.class))
			.build();

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mockBeanDefinitionRegistry(registeredBeanDefinitions);

		this.configuration.registerBeanDefinitions(mockBeanDefinitionRegistry);

		Set<String> registeredRegionBeanNames =
			asSet("RegionSix", "RegionSeven", "RegionEight", "RegionNine", "RegionTen", "RegionEleven",
				"RegionTwelve", "RegionThirteen", "RegionFourteen", "RegionFifteen", "RegionSixteen", "RegionSeventeen",
				"RegionEighteen", "RegionNineteen", "RegionTwenty", "RegionTwentyOne", "RegionTwentyTwo",
				"RegionTwentyThree", "RegionTwentyFour");

		verify(mockBeanDefinitionRegistry, times(1)).getBeanDefinitionNames();

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceFour"));

		verify(mockBeanDefinitionRegistry, never())
			.registerBeanDefinition(eq("RegionTwentyFive"), any(BeanDefinition.class));

		verify(mockBeanDefinitionRegistry, times(registeredRegionBeanNames.size()))
			.registerBeanDefinition(anyString(), any(BeanDefinition.class));

		registeredRegionBeanNames.forEach(beanName ->
			verify(mockBeanDefinitionRegistry, times(1))
				.registerBeanDefinition(eq(beanName), any(BeanDefinition.class)));
	}

	@Test
	public void cacheableServiceOneTwoThreeRegistersTwentyTwoRegionBeans() {

		Map<String, BeanDefinition> registeredBeanDefinitions = MapBuilder.<String, BeanDefinition>newMapBuilder()
			.put("cacheableServiceOne", mockBeanDefinition(CacheableServiceOne.class))
			.put("cacheableServiceTwo", mockBeanDefinition(CacheableServiceTwo.class))
			.put("cacheableServiceThree", mockBeanDefinition(CacheableServiceThree.class))
			.build();

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mockBeanDefinitionRegistry(registeredBeanDefinitions);

		this.configuration.registerBeanDefinitions(mockBeanDefinitionRegistry);

		Set<String> registeredRegionBeanNames =
			asSet("RegionOne", "RegionTwo", "RegionThree", "RegionFour", "RegionFive", "RegionSix",
				"RegionSeven", "RegionEight", "RegionNine", "RegionTen", "RegionEleven", "RegionTwelve",
				"RegionThirteen", "RegionFourteen", "RegionFifteen", "RegionSixteen", "RegionSeventeen",
				"RegionEighteen", "RegionNineteen", "RegionTwenty", "RegionTwentyOne", "RegionTwentyFive");

		verify(mockBeanDefinitionRegistry, times(1)).getBeanDefinitionNames();

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceOne"));

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceTwo"));

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceThree"));

		verify(mockBeanDefinitionRegistry, times(registeredRegionBeanNames.size()))
			.registerBeanDefinition(anyString(), any(BeanDefinition.class));

		registeredRegionBeanNames.forEach(beanName ->
			verify(mockBeanDefinitionRegistry, times(1))
				.registerBeanDefinition(eq(beanName), any(BeanDefinition.class)));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectCacheNamesForCacheableAndCachePutOnCacheableServiceThreeRegistersRegionFifteenSixteenSeventeen() {

		assertThat(this.configuration.collectCacheNames(CacheableServiceThree.class, Cacheable.class, CachePut.class))
			.containsExactly("RegionFifteen", "RegionSixteen", "RegionSeventeen");
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectCacheNamesForCacheableOnCacheableServiceThreeRegistersRegionFifteen() {

		assertThat(this.configuration.collectCacheNames(CacheableServiceThree.class, Cacheable.class))
			.containsExactly("RegionFifteen");
	}

	@Test
	@SuppressWarnings("unchecked")
	public void collectCacheNamesForCachePutOnCacheableServiceThreeRegistersRegionSixteenSeventeen() {

		assertThat(this.configuration.collectCacheNames(CacheableServiceThree.class, CachePut.class))
			.containsExactly("RegionSixteen", "RegionSeventeen");
	}

	@Service
	@Cacheable(cacheNames = { "RegionOne", "RegionTwo" })
	@SuppressWarnings("unused")
	static class CacheableServiceOne {

		public void cacheableMethodOne() {}

	}

	@Service
	@Cacheable(cacheNames = { "RegionTwo" })
	@CachePut("RegionSix")
	@SuppressWarnings("unused")
	static class CacheableServiceTwo {

		@Cacheable("RegionThree")
		public void cacheableMethodOne() {}

		@CacheEvict(cacheNames = { "RegionThree", "RegionFour" })
		public void cacheableMethodTwo() {}

		@CachePut(cacheNames = "RegionFive")
		public void cacheableMethodThree() {}

	}

	@Service
	@Caching(
		cacheable = { @Cacheable(cacheNames = { "RegionSix", "RegionSeven" }), @Cacheable("RegionEight") },
		evict = { @CacheEvict(cacheNames = { "RegionNine", "RegionTen"}), @CacheEvict("RegionEleven") },
		put = { @CachePut(cacheNames = { "RegionTwelve", "RegionThirteen" }), @CachePut("RegionFourteen") }
	)
	@Cacheable("RegionFifteen")
	@CachePut(cacheNames = { "RegionSixteen", "RegionSeventeen" })
	@SuppressWarnings("unused")
	static class CacheableServiceThree {

		@Caching(
			cacheable = { @Cacheable(cacheNames = { "RegionSix", "RegionTwelve" }), @Cacheable("RegionEighteen") },
			put = { @CachePut({ "RegionTen", "RegionNineteen" }), @CachePut("RegionTwenty") }
		)
		public void cacheableMethodOne() {}

		@CachePut("RegionTwentyOne")
		public void cacheableMethodTwo() {}

		@Cacheable("RegionTwentyFive")
		public void cacheableMethodFour() {}

	}

	@SuppressWarnings("unused")
	static class CacheableServiceFour extends CacheableServiceThree {

		@Cacheable({ "RegionEleven", "RegionTwentyTwo", "RegionTwentyThree", "RegionTwentyFour" })
		public void cacheableMethodThree() {}

		@Override
		@CachePut({ "RegionTwentyOne", "RegionTwentyTwo" })
		public void cacheableMethodFour() {}

		public void cacheableMethodFive() {}

	}
}
