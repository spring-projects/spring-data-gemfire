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
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newRuntimeException;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;

import org.junit.Test;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.core.SpringVersion;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.type.MethodMetadata;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
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
	private CachingDefinedRegionsConfiguration configuration = new CachingDefinedRegionsConfiguration();

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
	public void cacheableServiceOneRegistersRegionsOneAndTwo() {

		Map<String, BeanDefinition> registeredBeanDefinitions = MapBuilder.<String, BeanDefinition>newMapBuilder()
			.put("cacheableServiceOne", mockBeanDefinition(CacheableServiceOne.class))
			.build();

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mockBeanDefinitionRegistry(registeredBeanDefinitions);

		this.configuration.registerBeanDefinitions(mockBeanDefinitionRegistry);

		verify(mockBeanDefinitionRegistry, times(1)).getBeanDefinitionNames();

		verify(mockBeanDefinitionRegistry, times(1))
			.getBeanDefinition(eq("cacheableServiceOne"));

		verify(mockBeanDefinitionRegistry, times(1))
			.containsBeanDefinition(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));

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

		verify(mockBeanDefinitionRegistry, times(1))
			.containsBeanDefinition(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));

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

		verify(mockBeanDefinitionRegistry, times(1))
			.containsBeanDefinition(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));

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

		verify(mockBeanDefinitionRegistry, times(1))
			.containsBeanDefinition(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));

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

		verify(mockBeanDefinitionRegistry, times(3))
			.containsBeanDefinition(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME));

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

	@Test
	@SuppressWarnings("unchecked")
	public void resolveBeanClassFromBeanClassName() throws ClassNotFoundException {

		AbstractBeanDefinition mockBeanDefinition = mock(AbstractBeanDefinition.class);

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mock(BeanDefinitionRegistry.class);

		when(mockBeanDefinition.resolveBeanClass(any(ClassLoader.class))).thenReturn((Class) CacheableServiceOne.class);

		assertThat(this.configuration.resolveBeanClass(mockBeanDefinition, mockBeanDefinitionRegistry).orElse(null))
			.isEqualTo(CacheableServiceOne.class);

		verify(mockBeanDefinition, times(1))
			.resolveBeanClass(eq(Thread.currentThread().getContextClassLoader()));

		verifyNoMoreInteractions(mockBeanDefinition);

		verifyZeroInteractions(mockBeanDefinitionRegistry);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void resolveBeanClassFromFactoryMethodReturnType() throws ClassNotFoundException {

		AnnotatedBeanDefinition mockBeanDefinition = mock(AnnotatedBeanDefinition.class);

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mock(BeanDefinitionRegistry.class);

		MethodMetadata mockMethodMetadata = mock(MethodMetadata.class);

		when(mockBeanDefinition.getBeanClassName()).thenReturn("  ");
		when(mockBeanDefinition.getFactoryMethodName()).thenReturn("testFactoryMethod");
		when(mockBeanDefinition.getFactoryMethodMetadata()).thenReturn(mockMethodMetadata);
		when(mockMethodMetadata.getReturnTypeName()).thenReturn(CacheableServiceTwo.class.getName());

		assertThat(this.configuration.resolveBeanClass(mockBeanDefinition, mockBeanDefinitionRegistry).orElse(null))
			.isEqualTo(CacheableServiceTwo.class);

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodMetadata();
		verify(mockMethodMetadata, times(1)).getReturnTypeName();

		verifyNoMoreInteractions(mockBeanDefinition);
		verifyNoMoreInteractions(mockMethodMetadata);

		verifyZeroInteractions(mockBeanDefinitionRegistry);
	}

	@Test
	public void isNotInfrastructureBeanIsTrue() {

		BeanDefinition mockBeanDefinition = mockBeanDefinition(CacheableServiceOne.class);

		assertThat(this.configuration.isNotInfrastructureBean(mockBeanDefinition)).isTrue();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getRole();
	}

	@Test
	public void isNotInfrastructureBeanWithInfrastructureClassIsFalse() {

		BeanDefinition mockBeanDefinition = mockBeanDefinition(SpringVersion.class);

		assertThat(this.configuration.isNotInfrastructureBean(mockBeanDefinition)).isFalse();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getRole();
	}

	@Test
	public void isNotInfrastructureBeanWithInfrastructureRoleIsFalse() {

		BeanDefinition mockBeanDefinition = mockBeanDefinition(CacheableServiceOne.class);

		when(mockBeanDefinition.getRole()).thenReturn(BeanDefinition.ROLE_INFRASTRUCTURE);

		assertThat(this.configuration.isNotInfrastructureBean(mockBeanDefinition)).isFalse();

		verify(mockBeanDefinition, never()).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getRole();
	}

	@Test
	public void isNotInfrastructureClassWithBeanDefinitionIsTrue() {

		BeanDefinition mockBeanDefinition = mockBeanDefinition(CacheableServiceOne.class);

		assertThat(this.configuration.isNotInfrastructureClass(mockBeanDefinition)).isTrue();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
	}

	@Test
	public void isNotInfrastructureClassWithBeanDefinitionIsFalse() {

		BeanDefinition mockBeanDefinition = mockBeanDefinition(SpringVersion.class);

		assertThat(this.configuration.isNotInfrastructureClass(mockBeanDefinition)).isFalse();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
	}

	@Test
	public void isNotInfrastructureClassIsTrue() {
		assertThat(this.configuration.isNotInfrastructureClass(CacheableServiceOne.class.getName())).isTrue();
		assertThat(this.configuration.isNotInfrastructureClass("org.example.app.MyClass")).isTrue();
	}

	@Test
	public void isNotInfrastructureClassIsFalse() {
		assertThat(this.configuration.isNotInfrastructureClass("org.springframework.SomeType")).isFalse();
		assertThat(this.configuration.isNotInfrastructureClass("org.springframework.core.type.SomeType"))
			.isFalse();
	}

	@Test
	public void isNotInfrastructureRoleIsTrue() {

		BeanDefinition mockBeanDefinition = mock(BeanDefinition.class);

		when(mockBeanDefinition.getRole()).thenReturn(BeanDefinition.ROLE_APPLICATION).thenReturn(Integer.MAX_VALUE);

		assertThat(this.configuration.isNotInfrastructureRole(mockBeanDefinition)).isTrue();
		assertThat(this.configuration.isNotInfrastructureRole(mockBeanDefinition)).isTrue();

		verify(mockBeanDefinition, times(2)).getRole();
	}

	@Test
	public void isNotInfrastructureRoleIsFalse() {

		BeanDefinition mockBeanDefinition = mock(BeanDefinition.class);

		when(mockBeanDefinition.getRole()).thenReturn(BeanDefinition.ROLE_INFRASTRUCTURE)
			.thenReturn(BeanDefinition.ROLE_SUPPORT);

		assertThat(this.configuration.isNotInfrastructureRole(mockBeanDefinition)).isFalse();
		assertThat(this.configuration.isNotInfrastructureRole(mockBeanDefinition)).isFalse();

		verify(mockBeanDefinition, times(2)).getRole();
	}

	@Test
	public void isUserLevelMethodWithNullMethodReturnsFalse() {
		assertThat(this.configuration.isUserLevelMethod(null)).isFalse();
	}

	@Test
	public void isUserLevelMethodWithObjectMethodReturnsFalse() throws NoSuchMethodException {
		assertThat(this.configuration.isUserLevelMethod(Object.class.getMethod("equals", Object.class)))
			.isFalse();
	}

	@Test
	public void isUserLevelMethodWithUserMethodReturnsTrue() throws NoSuchMethodException {
		assertThat(this.configuration.isUserLevelMethod(CacheableServiceOne.class.getMethod("cacheableMethodOne")))
			.isTrue();
	}

	@Test
	public void resolveAnnotationFromClass() {

		Annotation cacheable = this.configuration.resolveAnnotation(CacheableServiceFour.class, Cacheable.class);

		assertThat(cacheable).isNotNull();
		assertThat(AnnotationUtils.getAnnotationAttributes(cacheable).get("cacheNames"))
			.isEqualTo(asArray("RegionFifteen"));
	}

	@Test
	public void resolveAnnotationFromMethod() throws NoSuchMethodException {

		Method cacheableMethodFour = CacheableServiceFour.class.getMethod("cacheableMethodFour");

		Annotation cachePut = this.configuration.resolveAnnotation(cacheableMethodFour, CachePut.class);

		assertThat(cachePut).isNotNull();
		assertThat(AnnotationUtils.getAnnotationAttributes(cachePut).get("cacheNames"))
			.isEqualTo(asArray("RegionTwentyOne", "RegionTwentyTwo"));
	}

	@Test
	public void resolveAnnotationIsUnresolvable() throws NoSuchMethodException {

		Method cacheableMethodFive = CacheableServiceFour.class.getMethod("cacheableMethodFive");

		assertThat(this.configuration.resolveAnnotation(cacheableMethodFive, Cacheable.class)).isNull();
	}

	@Test
	public void resolveBeanClassIsUnresolvable() throws ClassNotFoundException {

		AbstractBeanDefinition mockBeanDefinition = mock(AbstractBeanDefinition.class);

		when(mockBeanDefinition.getBeanClassName()).thenReturn("non.existing.bean.Class");
		when(mockBeanDefinition.resolveBeanClass(any(ClassLoader.class))).thenThrow(new ClassNotFoundException("TEST"));

		assertThat(this.configuration.resolveBeanClass(mockBeanDefinition, null).orElse(null)).isNull();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1))
			.resolveBeanClass(eq(Thread.currentThread().getContextClassLoader()));

		verifyNoMoreInteractions(mockBeanDefinition);
	}

	@Test
	public void resolveBeanClassLoaderReturnsRegistryClassLoader() {

		ClassLoader mockClassLoader = mock(ClassLoader.class);

		DefaultListableBeanFactory mockBeanDefinitionRegistry = mock(DefaultListableBeanFactory.class);

		when(mockBeanDefinitionRegistry.getBeanClassLoader()).thenReturn(mockClassLoader);

		assertThat(this.configuration.resolveBeanClassLoader(mockBeanDefinitionRegistry)).isEqualTo(mockClassLoader);

		verify(mockBeanDefinitionRegistry, times(1)).getBeanClassLoader();
	}

	@Test
	public void resolveBeanClassLoaderReturnsThreadContextClassLoader() {

		BeanDefinitionRegistry mockBeanDefinitionRegistry = mock(BeanDefinitionRegistry.class);

		assertThat(this.configuration.resolveBeanClassLoader(mockBeanDefinitionRegistry))
			.isEqualTo(Thread.currentThread().getContextClassLoader());

		verifyZeroInteractions(mockBeanDefinitionRegistry);
	}

	@Test
	public void resolveBeanClassNameReturnsBeanDefinitionBeanClassName() {

		BeanDefinition mockBeanDefinition = mockBeanDefinition(CacheableServiceOne.class);

		assertThat(this.configuration.resolveBeanClassName(mockBeanDefinition).orElse(null))
			.isEqualTo(CacheableServiceOne.class.getName());

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verifyNoMoreInteractions(mockBeanDefinition);
	}

	@Test
	public void resolveBeanClassNameReturnsFactoryMethodReturnType() {

		AnnotatedBeanDefinition mockBeanDefinition = mock(AnnotatedBeanDefinition.class);

		MethodMetadata mockMethodMetadata = mock(MethodMetadata.class);

		when(mockBeanDefinition.getBeanClassName()).thenReturn(null);
		when(mockBeanDefinition.getFactoryMethodName()).thenReturn("testFactoryMethod");
		when(mockBeanDefinition.getFactoryMethodMetadata()).thenReturn(mockMethodMetadata);
		when(mockMethodMetadata.getReturnTypeName()).thenReturn(CacheableServiceTwo.class.getName());

		assertThat(this.configuration.resolveBeanClassName(mockBeanDefinition).orElse(null))
			.isEqualTo(CacheableServiceTwo.class.getName());

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodMetadata();
	}

	@Test
	public void resolveBeanClassNameWithNoFactoryMethodMetadataReturnsEmpty() {

		AnnotatedBeanDefinition mockBeanDefinition = mock(AnnotatedBeanDefinition.class);

		when(mockBeanDefinition.getBeanClassName()).thenReturn(null);
		when(mockBeanDefinition.getFactoryMethodName()).thenReturn("testFactoryMethod");
		when(mockBeanDefinition.getFactoryMethodMetadata()).thenReturn(null);

		assertThat(this.configuration.resolveBeanClassName(mockBeanDefinition).orElse(null)).isNull();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodMetadata();
		verifyNoMoreInteractions(mockBeanDefinition);
	}

	@Test
	public void resolveBeanClassNameWithNonAnnotatedBeanDefinitionReturnsEmpty() {

		BeanDefinition mockBeanDefinition = mock(BeanDefinition.class);

		when(mockBeanDefinition.getBeanClassName()).thenReturn(null);
		when(mockBeanDefinition.getFactoryMethodName()).thenReturn("testFactoryMethod");

		assertThat(this.configuration.resolveBeanClassName(mockBeanDefinition).orElse(null)).isNull();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodName();
		verifyNoMoreInteractions(mockBeanDefinition);
	}

	@Test
	public void resolveBeanClassNameWithNoFactoryMethodNameReturnsEmpty() {

		BeanDefinition mockBeanDefinition = mock(BeanDefinition.class);

		when(mockBeanDefinition.getBeanClassName()).thenReturn(null);
		when(mockBeanDefinition.getFactoryMethodName()).thenReturn("  ");

		assertThat(this.configuration.resolveBeanClassName(mockBeanDefinition).orElse(null)).isNull();

		verify(mockBeanDefinition, times(1)).getBeanClassName();
		verify(mockBeanDefinition, times(1)).getFactoryMethodName();
		verifyNoMoreInteractions(mockBeanDefinition);
	}

	@Test
	public void safeResolveTypeReturnsType() {
		assertThat(this.configuration.safeResolveType(() -> Object.class)).isEqualTo(Object.class);
	}

	@Test
	public void safeResolveTypeThrowingClassNotFoundExceptionReturnsNull() {
		assertThat(this.configuration.safeResolveType(() -> { throw new ClassNotFoundException("TEST"); })).isNull();
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
