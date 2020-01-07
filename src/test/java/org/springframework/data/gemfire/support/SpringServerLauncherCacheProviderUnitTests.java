/*
 * Copyright 2016-2020 the original author or authors.
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
package org.springframework.data.gemfire.support;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.distributed.ServerLauncher;

import org.junit.After;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.gemfire.GemfireUtils;

/**
 * Unit tests for {@link SpringServerLauncherCacheProvider}.
 *
 * @author Dan Smith
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.distributed.ServerLauncher
 * @see org.apache.geode.distributed.ServerLauncherCacheProvider
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.support.SpringServerLauncherCacheProvider
 */
public class SpringServerLauncherCacheProviderUnitTests {

	String gemfireName() {
		return (GemfireUtils.GEMFIRE_PREFIX + GemfireUtils.NAME_PROPERTY_NAME);
	}

	Properties singletonProperties(String propertyName, String propertyValue) {
		Properties properties = new Properties();
		properties.setProperty(propertyName, propertyValue);
		return properties;
	}

	@After
	public void tearDown() {
		System.clearProperty(gemfireName());
		SpringContextBootstrappingInitializer.applicationContext = null;
	}

	@Test
	public void createsCacheWhenSpringXmlLocationIsSpecified() {
		Cache mockCache = mock(Cache.class);
		ConfigurableApplicationContext mockApplicationContext = mock(ConfigurableApplicationContext.class);
		ServerLauncher mockServerLauncher = mock(ServerLauncher.class);

		SpringContextBootstrappingInitializer.applicationContext = mockApplicationContext;

		when(mockServerLauncher.isSpringXmlLocationSpecified()).thenReturn(true);
		when(mockServerLauncher.getSpringXmlLocation()).thenReturn("test-context.xml");
		when(mockServerLauncher.getMemberName()).thenReturn("TEST");
		when(mockApplicationContext.getBean(eq(Cache.class))).thenReturn(mockCache);

		final SpringContextBootstrappingInitializer initializer = mock(SpringContextBootstrappingInitializer.class);

		SpringServerLauncherCacheProvider provider = new SpringServerLauncherCacheProvider() {
			@Override
			public SpringContextBootstrappingInitializer newSpringContextBootstrappingInitializer() {
				return initializer;
			}
		};

		Properties expectedParameters = singletonProperties(
			SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER, "test-context.xml");

		assertThat(provider.createCache(null, mockServerLauncher), is(equalTo(mockCache)));

		verify(mockServerLauncher, times(1)).isSpringXmlLocationSpecified();
		verify(mockServerLauncher, times(1)).getSpringXmlLocation();
		verify(mockServerLauncher, times(1)).getMemberName();
		verify(mockApplicationContext, times(1)).getBean(eq(Cache.class));
		verify(initializer).init(eq(expectedParameters));
	}

	@Test
	public void doesNothingWhenSpringXmlLocationNotSpecified() {
		ServerLauncher launcher = mock(ServerLauncher.class);

		when(launcher.isSpringXmlLocationSpecified()).thenReturn(false);

		assertThat(new SpringServerLauncherCacheProvider().createCache(null, launcher), is(nullValue()));

		verify(launcher, times(1)).isSpringXmlLocationSpecified();
	}
}
