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
import org.springframework.data.gemfire.support.SpringContextBootstrappingInitializerIntegrationTest.UserDataStoreCacheLoader;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.distributed.AbstractLauncher.Status;
import com.gemstone.gemfire.distributed.ServerLauncher;
import com.gemstone.gemfire.distributed.ServerLauncher.ServerState;
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
public class SpringServerLauncherCacheProviderIntegrationTest {
	
	@After
	public void tearDown() {
		System.clearProperty(DistributionConfig.GEMFIRE_PREFIX + DistributionConfig.NAME_NAME);
		SpringContextBootstrappingInitializer.getApplicationContext().close();
		tearDownCache();
	}

	private void tearDownCache() {
		try {
			Cache cache = CacheFactory.getAnyInstance();

			if (cache != null) {
				cache.close();
			}
		}
		catch (CacheClosedException ignore) {
			// CacheClosedExceptions happen when the Cache reference returned by GemFireCacheImpl.getInstance()
			// inside the CacheFactory.getAnyInstance() is null, or the Cache is already closed with calling
			// Cache.close();
		}
	}

	@Test
	public void createCacheWithSpecifiedConfig() {
		String xmlLocation = getClass().getSimpleName() + "-context.xml";
		
		ServerLauncher launcher = mock(ServerLauncher.class);
	
		ServerLauncher.Builder builder = new ServerLauncher.Builder();
		builder.setSpringXmlLocation(xmlLocation);
		builder.setMemberName("membername");
		launcher = builder.build();
		ServerState state = launcher.start();
		assertEquals(Status.ONLINE, state.getStatus());
		ConfigurableApplicationContext ctx = SpringContextBootstrappingInitializer.getApplicationContext();
		Cache cache = ctx.getBean(Cache.class);
		assertNotNull(cache);
		assertEquals(55, cache.getResourceManager().getCriticalHeapPercentage(), 0.1);
		assertEquals(Status.STOPPED, launcher.stop().getStatus());
	}

}
