/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import javax.annotation.Resource;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEvent;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.util.CacheListenerAdapter;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The SubRegionSubElementNamespaceTest class...
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.Region
 * @link https://jira.springsource.org/browse/SGF-219
 * @link https://jira.springsource.org/browse/SGF-220
 * @link https://jira.springsource.org/browse/SGF-221
 * @since 1.3.3
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "subregionsubelement-ns.xml",
	initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class SubRegionSubElementNamespaceTest {

	@Resource(name = "/Customers/Accounts")
	private Region customersAccountsRegion;

	@Resource(name = "/Orders/Items")
	private Region orderItemsRegion;

	@Resource(name = "/Parent/Child")
	private Region parentChildRegion;

	@Test
	public void testCustomersAccountsSubRegionCacheListener() {
		assertNotNull(customersAccountsRegion);
		assertNotNull(customersAccountsRegion.getAttributes());
		assertNotNull(customersAccountsRegion.getAttributes().getCacheListeners());

		boolean found = false;

		for (CacheListener listener : customersAccountsRegion.getAttributes().getCacheListeners()) {
			found |= (listener instanceof TestNoOpCacheListener);
		}

		assertTrue(String.format("Expected a GemFire CacheListener of type (%1$s) to be registered on Region (%2$s)!",
			TestNoOpCacheListener.class.getName(), customersAccountsRegion.getName()), found);
	}

	@Test
	public void testOrderItemsSubRegionGatewaySender() {
		assertNotNull(orderItemsRegion);
		assertNotNull(orderItemsRegion.getAttributes());
		assertNotNull(orderItemsRegion.getAttributes().getGatewaySenderIds());
		assertTrue(orderItemsRegion.getAttributes().getGatewaySenderIds().contains("testSender"));
	}

	@Test
	public void testParentChildSubRegionAsyncEventQueue() {
		assertNotNull(parentChildRegion);
		assertNotNull(parentChildRegion.getAttributes());
		assertNotNull(parentChildRegion.getAttributes().getAsyncEventQueueIds());
		assertTrue(parentChildRegion.getAttributes().getAsyncEventQueueIds().contains("testQueue"));
	}

	public static final class TestNoOpCacheListener extends CacheListenerAdapter {
	}

	public static final class TestNoOpAsyncEventListener implements AsyncEventListener {

		@Override
		public boolean processEvents(final List<AsyncEvent> events) {
			throw new UnsupportedOperationException("Not Implemented!");
		}

		@Override
		public void close() {
		}
	}

}
