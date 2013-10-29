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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.TestUtils;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;

public class ClientRegionFactoryBeanTest {

	private ClientRegionFactoryBean<Object, Object> factoryBean;

	@Before
	public void setup() {
		factoryBean = new ClientRegionFactoryBean<Object, Object>();
	}

	@After
	public void tearDown() throws Exception {
		factoryBean.destroy();
		factoryBean = null;
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testLookupFallbackFailingToUseProvidedShortcut() throws Exception {
		factoryBean.setShortcut(ClientRegionShortcut.CACHING_PROXY);

		BeanFactory beanFactory = mock(BeanFactory.class);
		Pool pool = mock(Pool.class);

		when(beanFactory.getBean(Pool.class)).thenReturn(pool);

		factoryBean.setBeanFactory(beanFactory);

		ClientCache cache = mock(ClientCache.class);
		ClientRegionFactory<Object, Object> clientRegionFactory = mock(ClientRegionFactory.class);
		Region<Object, Object> expectedRegion = mock(Region.class);

		when(cache.createClientRegionFactory(ClientRegionShortcut.CACHING_PROXY)).thenReturn(clientRegionFactory);
		when(clientRegionFactory.create("testRegion")).thenReturn(expectedRegion);

		Region<Object, Object> actualRegion = factoryBean.lookupFallback(cache, "testRegion");

		assertSame(expectedRegion, actualRegion);
	}

	@Test
	public void testSetDataPolicyName() throws Exception {
		factoryBean.setDataPolicyName("NORMAL");
		assertEquals(DataPolicy.NORMAL, TestUtils.readField("dataPolicy", factoryBean));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetDataPolicyNameWithInvalidName() throws Exception {
		try {
			factoryBean.setDataPolicyName("INVALID");
		}
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'INVALID' is invalid.", e.getMessage());
			throw e;
		}
		finally {
			assertNull(TestUtils.readField("dataPolicy", factoryBean));
		}
	}

	@Test
	public void testIsPersistent() {
		assertFalse(factoryBean.isPersistent());
		factoryBean.setPersistent(false);
		assertFalse(factoryBean.isPersistent());
		factoryBean.setPersistent(true);
		assertTrue(factoryBean.isPersistent());
	}

	@Test
	public void testIsNotPersistent() {
		assertFalse(factoryBean.isNotPersistent());
		factoryBean.setPersistent(true);
		assertFalse(factoryBean.isNotPersistent());
		factoryBean.setPersistent(false);
		assertTrue(factoryBean.isNotPersistent());
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
		catch (IllegalArgumentException e) {
			assertEquals("Client Region Shortcut 'CACHING_PROXY' is invalid when persistent is true.", e.getMessage());
			throw e;
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
		catch (IllegalArgumentException e) {
			assertEquals("Client Region Shortcut 'LOCAL_PERSISTENT' is invalid when persistent is false.", e.getMessage());
			throw e;
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
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'NORMAL' is invalid when persistent is true.", e.getMessage());
			throw e;
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
		catch (IllegalArgumentException e) {
			assertEquals("Data Policy 'PERSISTENT_REPLICATE' is invalid when persistent is false.", e.getMessage());
			throw e;
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

}
