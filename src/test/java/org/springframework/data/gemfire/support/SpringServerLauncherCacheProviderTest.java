/*
 * Copyright 2016 the original author or authors.
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
package org.springframework.data.gemfire.support;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.Collections;

import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.distributed.ServerLauncher;
import com.gemstone.gemfire.distributed.internal.DistributionConfig;
import com.gemstone.gemfire.internal.util.CollectionUtils;

/**
 * The SpringServerLauncherCacheProviderTest class is a test suite of test cases testing the contract and functionality
 * of the SpringServerLauncherCacheProvider class. This test class focuses on testing isolated units of functionality in
 * the ServerLauncherCacheProvider class directly, mocking any dependencies as appropriate, in order for the class to
 * uphold it's contract.
 *
 * @author Dan Smith
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.support.SpringServerLauncherCacheProvider
 */
public class SpringServerLauncherCacheProviderTest {
	
	@After
	public void tearDown() {
		System.clearProperty(DistributionConfig.GEMFIRE_PREFIX + DistributionConfig.NAME_NAME);
		SpringContextBootstrappingInitializer.applicationContext = null;
	}

	@Test
	public void doesNothingWhenSpringXmlLocationNotSpecified() {
		SpringServerLauncherCacheProvider provider = new SpringServerLauncherCacheProvider();
		ServerLauncher launcher = mock(ServerLauncher.class);
		when(launcher.isSpringXmlLocationSpecified()).thenReturn(false);
		assertEquals(null, provider.createCache(null, launcher));
		verify(launcher).isSpringXmlLocationSpecified();
	}
	
	@Test
	public void createCacheWithSpecifiedConfig() {
		String xmlLocation = "xml/location";
		
		ServerLauncher launcher = mock(ServerLauncher.class);
		when(launcher.isSpringXmlLocationSpecified()).thenReturn(true);
		when(launcher.getSpringXmlLocation()).thenReturn(xmlLocation);
		when(launcher.getMemberName()).thenReturn("membername");
		
		final SpringContextBootstrappingInitializer initializer = mock(SpringContextBootstrappingInitializer.class);
		ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
		SpringContextBootstrappingInitializer.applicationContext = context;
		Cache cache = mock(Cache.class);
		when(context.getBean(eq(Cache.class))).thenReturn(cache );
		
		SpringServerLauncherCacheProvider provider = new SpringServerLauncherCacheProvider() {
			@Override
			public SpringContextBootstrappingInitializer createSpringContextBootstrappingInitializer() {
				return initializer;
			}
		};
		
		assertEquals(cache, provider.createCache(null, launcher));
		verify(launcher).isSpringXmlLocationSpecified();
		
		verify(initializer).init(CollectionUtils.createProperties(
				Collections.singletonMap(SpringContextBootstrappingInitializer.CONTEXT_CONFIG_LOCATIONS_PARAMETER,
						xmlLocation)));
	}

}
