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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.Interest;
import org.springframework.data.gemfire.client.RegexInterest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.CacheListener;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.InterestResultPolicy;
import com.gemstone.gemfire.cache.Scope;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("client-ns.xml")
public class ClientRegionNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testBasicClient() throws Exception {
		assertTrue(context.containsBean("simple"));
	}

	@Test
	public void testPublishingClient() throws Exception {
		assertTrue(context.containsBean("empty"));
		ClientRegionFactoryBean fb = context.getBean("&empty", ClientRegionFactoryBean.class);
		assertEquals(DataPolicy.EMPTY, TestUtils.readField("dataPolicy", fb));
		assertEquals(Scope.LOCAL, TestUtils.readField("scope", fb));
	}


	public void testComplexClient() throws Exception {
		assertTrue(context.containsBean("complex"));
		ClientRegionFactoryBean fb = context.getBean("&complex", ClientRegionFactoryBean.class);
		CacheListener[] listeners = TestUtils.readField("cacheListeners", fb);
		assertFalse(ObjectUtils.isEmpty(listeners));
		assertEquals(2, listeners.length);
		assertSame(listeners[0], context.getBean("c-listener"));
		Interest[] ints = TestUtils.readField("interests", fb);
		assertEquals(2, ints.length);

		// key interest
		Interest keyInt = ints[0];
		assertTrue((Boolean) TestUtils.readField("durable", keyInt));
		assertEquals(InterestResultPolicy.KEYS, TestUtils.readField("policy", keyInt));
		assertEquals(Object.class, TestUtils.readField("key", keyInt).getClass());

		// regex interest
		RegexInterest regexInt = (RegexInterest) ints[1];
		assertFalse((Boolean) TestUtils.readField("durable", regexInt));
		assertEquals(InterestResultPolicy.KEYS_VALUES, TestUtils.readField("key", regexInt));
		assertEquals(".*", TestUtils.readField("key", regexInt));

	}
}