/*
 * Copyright 2010 the original author or authors.
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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.client.PoolManager;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("pool-ns.xml")
public class PoolNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testBasicClient() throws Exception {
		assertTrue(context.containsBean("gemfire-pool"));
		assertEquals(context.getBean("gemfire-pool"), PoolManager.find("gemfire-pool"));
	}

	@Test
	public void testComplexPool() throws Exception {
		assertTrue(context.containsBean("complex"));
		PoolFactoryBean pfb = (PoolFactoryBean) context.getBean("&complex");
		assertEquals(30, TestUtils.readField("retry-attempts", pfb));
		assertEquals(6000, TestUtils.readField("free-connection-timeout", pfb));
		assertEquals(5000, TestUtils.readField("ping-interval", pfb));
	}
}
