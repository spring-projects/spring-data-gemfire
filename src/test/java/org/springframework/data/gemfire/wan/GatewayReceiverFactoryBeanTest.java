/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.wan;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.wan.GatewayReceiverFactory;
import org.apache.geode.cache.wan.GatewayTransportFilter;
import org.junit.Test;

/**
 * The GatewayReceiverFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the GatewayReceiverFactoryBean class.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean
 * @see org.apache.geode.cache.wan.GatewayReceiverFactory
 * @since 1.5.0
 */
public class GatewayReceiverFactoryBeanTest {

	@Test
	public void testDoInit() throws Exception {
		Cache mockCache = mock(Cache.class, "testDoInit.Cache");
		GatewayReceiverFactory mockGatewayReceiverFactory = mock(GatewayReceiverFactory.class, "testDoInit.GatewayReceiverFactory");
		GatewayTransportFilter mockGatewayTransportFilter = mock(GatewayTransportFilter.class, "testDoInit.GatewayTransportFilter");

		when(mockCache.createGatewayReceiverFactory()).thenReturn(mockGatewayReceiverFactory);

		GatewayReceiverFactoryBean factoryBean = new GatewayReceiverFactoryBean(mockCache);

		factoryBean.setBindAddress("10.224.112.77");
		factoryBean.setHostnameForSenders("skullbox");
		factoryBean.setStartPort(2048);
		factoryBean.setEndPort(4096);
		factoryBean.setManualStart(true);
		factoryBean.setMaximumTimeBetweenPings(5000);
		factoryBean.setName("testDoInit");
		factoryBean.setSocketBufferSize(16384);
		factoryBean.setTransportFilters(Arrays.asList(mockGatewayTransportFilter));
		factoryBean.afterPropertiesSet();

		verify(mockGatewayReceiverFactory).setBindAddress(eq("10.224.112.77"));
		verify(mockGatewayReceiverFactory).setHostnameForSenders(eq("skullbox"));
		verify(mockGatewayReceiverFactory).setStartPort(eq(2048));
		verify(mockGatewayReceiverFactory).setEndPort(eq(4096));
		verify(mockGatewayReceiverFactory).setManualStart(eq(true));
		verify(mockGatewayReceiverFactory).setMaximumTimeBetweenPings(eq(5000));
		verify(mockGatewayReceiverFactory).setSocketBufferSize(eq(16384));
		verify(mockGatewayReceiverFactory).addGatewayTransportFilter(same(mockGatewayTransportFilter));
		verify(mockGatewayReceiverFactory).create();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDoInitWithIllegalStartEndPorts() throws Exception {
		try {
			Cache mockCache = mock(Cache.class, "testDoInitWithIllegalStartEndPorts.Cache");
			GatewayReceiverFactory mockGatewayReceiverFactory = mock(GatewayReceiverFactory.class,
				"testDoInitWithIllegalStartEndPorts.GatewayReceiverFactory");

			when(mockCache.createGatewayReceiverFactory()).thenReturn(mockGatewayReceiverFactory);

			GatewayReceiverFactoryBean factoryBean = new GatewayReceiverFactoryBean(mockCache);

			factoryBean.setName("testDoInitWithIllegalStartEndPorts");
			factoryBean.setStartPort(10240);
			factoryBean.setEndPort(8192);
			factoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("'startPort' must be less than or equal to 8192.", expected.getMessage());
			throw expected;
		}
	}

}
