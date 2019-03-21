/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Lyndon Adams
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "client-cache-ready-for-events.xml",
	initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class ClientReadyForEventsTest {
	
	@Autowired
	private ApplicationContext context;
	
	@Test
	public void testReadyForEvents() throws Exception {
		ClientCacheFactoryBean clientCacheFactoryBean = context.getBean("&gemfireCache", ClientCacheFactoryBean.class);
		Boolean readyForEvents = TestUtils.readField("readyForEvents", clientCacheFactoryBean);
		assertTrue(readyForEvents);
	}

}
