/*
 * Copyright 2010-2013 the original author or authors.
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.BeanFactory;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;

/**
 * @author David Turanski
 * @author John Blum
 */
public class ClientRegionFactoryBeanTest {

	@Test
	public void testLookupFallbackFailingToUseProvidedShortcut() throws Exception {
		ClientRegionFactoryBean<Object, Object> fb = new ClientRegionFactoryBean<Object, Object>();

		fb.setShortcut(ClientRegionShortcut.CACHING_PROXY);

		Pool pool = Mockito.mock(Pool.class);
		
		BeanFactory beanFactory = Mockito.mock(BeanFactory.class);
		
		Mockito.when(beanFactory.getBean(Pool.class)).thenReturn(pool);
		
		fb.setBeanFactory(beanFactory);

		String regionName = "regionName";
		ClientCache cache = Mockito.mock(ClientCache.class);

		@SuppressWarnings("unchecked")
		ClientRegionFactory<Object, Object> factory = Mockito.mock(ClientRegionFactory.class);
		Mockito.when(cache.createClientRegionFactory(ClientRegionShortcut.CACHING_PROXY)).thenReturn(factory);

		@SuppressWarnings("unchecked")
		Region<Object, Object> region = Mockito.mock(Region.class);
		Mockito.when(factory.create(regionName)).thenReturn(region);

		Region<Object, Object> result = fb.lookupFallback(cache, regionName);

		assertSame(region, result);
	}

	@Test
	public void testCloseDestroySettings() {
		final ClientRegionFactoryBean<Object, Object> factory = new ClientRegionFactoryBean<Object, Object>();

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

}
