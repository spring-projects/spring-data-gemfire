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
package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.util.GatewayEvent;
import com.gemstone.gemfire.cache.util.GatewayEventListener;
import com.gemstone.gemfire.cache.util.GatewayHub;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.data.gemfire.wan.GatewayHubFactoryBean;
import org.springframework.data.gemfire.wan.GatewayProxy;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations= "/org/springframework/data/gemfire/config/xml/gateway-v6-ns.xml",
	initializers=GemfireTestApplicationContextInitializer.class)
public class GemfireV6GatewayNamespaceTest {

	@Autowired ApplicationContext ctx;

	@Test
	public void testGatewayHubFactoryBean() throws Exception {
		GatewayHubFactoryBean gwhfb = ctx.getBean("&gateway-hub", GatewayHubFactoryBean.class);
		List<GatewayProxy> gateways = TestUtils.readField("gateways", gwhfb);
		assertNotNull(gateways);
		assertEquals(2, gateways.size());
		GatewayProxy gwp = gateways.get(0);
		assertEquals("gateway", gwp.getId());
		assertTrue(gwp.getListeners().get(0) instanceof GatewayListener);

		gwp = gateways.get(1);
		assertEquals("gateway2", gwp.getId());
		List<GatewayProxy.GatewayEndpoint> endpoints = gwp.getEndpoints();
		assertEquals(2, endpoints.size());
		GatewayProxy.GatewayEndpoint endpoint;

		endpoint = endpoints.get(0);
		assertEquals("endpoint1", endpoint.getId());
		assertEquals("host1", endpoint.getHost());
		assertEquals(1234, endpoint.getPort());

		endpoint = endpoints.get(1);
		assertEquals("endpoint2", endpoint.getId());
		assertEquals("host2", endpoint.getHost());
		assertEquals(2345, endpoint.getPort());
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testGatewaysInGemfire() {
		Cache cache = ctx.getBean("gemfireCache", Cache.class);
		GatewayHub gwh = cache.getGatewayHub("gateway-hub");
		assertNotNull(gwh);

		Region region = ctx.getBean("region-with-gateway", Region.class);
		assertTrue(region.getAttributes().getEnableGateway());
		assertEquals("gateway-hub", region.getAttributes().getGatewayHubId());
	}

	public static class GatewayListener implements GatewayEventListener {

		@Override
		public void close() {
			// TODO Auto-generated method stub

		}

		@Override
		public boolean processEvents(List<GatewayEvent> event) {
			// TODO Auto-generated method stub
			return false;
		}

	}
}
