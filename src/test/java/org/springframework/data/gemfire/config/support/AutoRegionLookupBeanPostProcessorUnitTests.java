/*
 * Copyright 2012 the original author or authors.
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

package org.springframework.data.gemfire.config.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.util.CollectionUtils;

/**
 * Unit tests for {@link AutoRegionLookupBeanPostProcessor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.support.AutoRegionLookupBeanPostProcessor
 * @since 1.9.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AutoRegionLookupBeanPostProcessorUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private AutoRegionLookupBeanPostProcessor autoRegionLookupBeanPostProcessor;

	@Mock
	private ConfigurableListableBeanFactory mockBeanFactory;

	@Before
	public void setup() {
		autoRegionLookupBeanPostProcessor = new AutoRegionLookupBeanPostProcessor();
	}

	protected Region<?, ?> mockRegion(String regionFullPath) {
		Region<?, ?> mockRegion = mock(Region.class);
		when(mockRegion.getFullPath()).thenReturn(regionFullPath);
		when(mockRegion.getName()).thenReturn(toRegionName(regionFullPath));
		return mockRegion;
	}

	protected String toRegionName(String regionFullPath) {
		int index = regionFullPath.lastIndexOf(Region.SEPARATOR);
		return (index > -1 ? regionFullPath.substring(index + 1) : regionFullPath);
	}

	@Test
	public void setAndGetBeanFactory() {
		autoRegionLookupBeanPostProcessor.setBeanFactory(mockBeanFactory);

		assertThat(autoRegionLookupBeanPostProcessor.getBeanFactory()).isSameAs(mockBeanFactory);
	}

	@Test
	public void setBeanFactoryToIncompatibleBeanFactoryType() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(String.format("BeanFactory [%1$s] must be an instance of %2$s",
			mockBeanFactory.getClass().getName(), ConfigurableListableBeanFactory.class.getSimpleName()));

		autoRegionLookupBeanPostProcessor.setBeanFactory(mockBeanFactory);
	}

	@Test
	public void setBeanFactoryToNull() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(String.format("BeanFactory [null] must be an instance of %s",
			ConfigurableListableBeanFactory.class.getSimpleName()));

		autoRegionLookupBeanPostProcessor.setBeanFactory(null);
	}

	@Test
	public void getBeanFactoryUninitialized() {
		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("BeanFactory was not properly initialized");

		autoRegionLookupBeanPostProcessor.getBeanFactory();
	}

	@Test
	public void postProcessBeforeInitializationReturnsBean() {
		Object bean = new Object();

		assertThat(autoRegionLookupBeanPostProcessor.postProcessBeforeInitialization(bean, "test")).isSameAs(bean);
	}

	@Test
	public void postProcessAfterInitializationWithNonGemFireCacheBean() {
		Object bean = new Object();

		AutoRegionLookupBeanPostProcessor autoRegionLookupBeanPostProcessorSpy =
			spy(this.autoRegionLookupBeanPostProcessor);

		assertThat(autoRegionLookupBeanPostProcessorSpy.postProcessAfterInitialization(bean, "test")).isSameAs(bean);

		verify(autoRegionLookupBeanPostProcessorSpy, never()).registerCacheRegionsAsBeans(any(GemFireCache.class));
	}

	@Test
	public void registerCacheRegionsAsBeansIsSuccessful() {
		Set<Region<?, ?>> expected = CollectionUtils.asSet(mockRegion("one"), mockRegion("two"), mockRegion("three"));
		final Set<Region<?, ?>> actual = new HashSet<Region<?, ?>>(expected.size());

		AutoRegionLookupBeanPostProcessor autoRegionLookupBeanPostProcessor = new AutoRegionLookupBeanPostProcessor() {
			@Override void registerCacheRegionAsBean(Region<?, ?> region) {
				actual.add(region);
			}
		};

		GemFireCache mockGemFireCache = mock(GemFireCache.class);

		when(mockGemFireCache.rootRegions()).thenReturn(expected);

		autoRegionLookupBeanPostProcessor.registerCacheRegionsAsBeans(mockGemFireCache);

		assertThat(actual).isEqualTo(expected);

		verify(mockGemFireCache, times(1)).rootRegions();

		for (Region<?, ?> region : expected) {
			verifyZeroInteractions(region);
		}
	}

	@Test
	public void registerCacheRegionAsBeanIsSuccessful() {
		Region<?, ?> mockRegion = mockRegion("Example");

		when(mockRegion.subregions(anyBoolean())).thenReturn(Collections.<Region<?, ?>>emptySet());
		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);

		autoRegionLookupBeanPostProcessor.setBeanFactory(mockBeanFactory);
		autoRegionLookupBeanPostProcessor.registerCacheRegionAsBean(mockRegion);

		verify(mockBeanFactory, times(1)).containsBean(eq("Example"));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("Example"), eq(mockRegion));
		verify(mockRegion, times(1)).getFullPath();
		verify(mockRegion, times(1)).getName();
		verify(mockRegion, times(1)).subregions(eq(false));
	}

	@Test
	public void registerCacheRegionAsBeanRegistersSubRegionIgnoresRootRegion() {
		Region<?, ?> mockRootRegion = mockRegion("Root");
		Region<?, ?> mockSubRegion = mockRegion("/Root/Sub");

		when(mockRootRegion.subregions(anyBoolean())).thenReturn(CollectionUtils.<Region<?, ?>>asSet(mockSubRegion));
		when(mockSubRegion.subregions(anyBoolean())).thenReturn(Collections.<Region<?, ?>>emptySet());
		when(mockBeanFactory.containsBean(eq("Root"))).thenReturn(true);
		when(mockBeanFactory.containsBean(eq("/Root/Sub"))).thenReturn(false);

		autoRegionLookupBeanPostProcessor.setBeanFactory(mockBeanFactory);
		autoRegionLookupBeanPostProcessor.registerCacheRegionAsBean(mockRootRegion);

		verify(mockBeanFactory, times(1)).containsBean(eq("Root"));
		verify(mockBeanFactory, times(1)).containsBean(eq("/Root/Sub"));
		verify(mockBeanFactory, never()).registerSingleton(eq("Root"), eq(mockRootRegion));
		verify(mockBeanFactory, times(1)).registerSingleton(eq("/Root/Sub"), eq(mockSubRegion));
		verify(mockRootRegion, times(1)).getFullPath();
		verify(mockRootRegion, times(1)).getName();
		verify(mockRootRegion, times(1)).subregions(eq(false));
		verify(mockSubRegion, times(1)).getFullPath();
		verify(mockSubRegion, never()).getName();
		verify(mockSubRegion, times(1)).subregions(eq(false));
	}

	@Test
	public void registerNullCacheRegionAsBeanDoesNothing() {
		autoRegionLookupBeanPostProcessor.setBeanFactory(mockBeanFactory);
		autoRegionLookupBeanPostProcessor.registerCacheRegionAsBean(null);

		verifyZeroInteractions(mockBeanFactory);
	}

	@Test
	public void getBeanNameReturnsRegionFullPath() {
		Region mockRegion = mockRegion("/Parent/Child");

		assertThat(autoRegionLookupBeanPostProcessor.getBeanName(mockRegion)).isEqualTo("/Parent/Child");

		verify(mockRegion, times(1)).getFullPath();
		verify(mockRegion, never()).getName();
	}

	@Test
	public void getBeanNameReturnsRegionName() {
		Region mockRegion = mockRegion("/Example");

		assertThat(autoRegionLookupBeanPostProcessor.getBeanName(mockRegion)).isEqualTo("Example");

		verify(mockRegion, times(1)).getFullPath();
		verify(mockRegion, times(1)).getName();
	}

	@Test
	public void nullSafeSubRegionsWhenSubRegionsIsNotNull() {
		Set<Region<?, ?>> mockSubRegions = CollectionUtils.asSet(mockRegion("one"), mockRegion("two"));
		Region mockRegion = mockRegion("parent");

		when(mockRegion.subregions(anyBoolean())).thenReturn(mockSubRegions);

		assertThat(autoRegionLookupBeanPostProcessor.nullSafeSubregions(mockRegion)).isEqualTo(mockSubRegions);

		verify(mockRegion, times(1)).subregions(eq(false));
	}

	@Test
	public void nullSafeSubRegionsWhenSubRegionsIsNull() {
		Region mockRegion = mockRegion("parent");

		when(mockRegion.subregions(anyBoolean())).thenReturn(null);

		assertThat(autoRegionLookupBeanPostProcessor.nullSafeSubregions(mockRegion)).isEqualTo(Collections.emptySet());

		verify(mockRegion, times(1)).subregions(eq(false));
	}
}
