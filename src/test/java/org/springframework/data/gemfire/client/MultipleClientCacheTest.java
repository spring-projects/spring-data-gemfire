/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.fork.SpringCacheServerProcess;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.Pool;

/**
 * @author David Turanski
 *
 */

public class MultipleClientCacheTest {
	@BeforeClass
	public static void startUp() throws Exception {
		ForkUtil.startCacheServer(SpringCacheServerProcess.class.getName() + " "
				+ "/org/springframework/data/gemfire/client/datasource-server.xml");
	}
	@Test
	public void testMultipleCaches() {
		String resourcePath = "/org/springframework/data/gemfire/client/client-cache-no-close.xml";
		
		ConfigurableApplicationContext ctx1 = new ClassPathXmlApplicationContext(resourcePath);
		ConfigurableApplicationContext ctx2 = new ClassPathXmlApplicationContext(resourcePath);
		Region region1 = ctx1.getBean(Region.class);
		Cache cache1 = ctx1.getBean(Cache.class);
		Pool pool = ctx1.getBean(Pool.class);
		Region region2 = ctx2.getBean(Region.class);
		Cache cache2 = ctx2.getBean(Cache.class);
		assertSame(region1,region2);
		assertSame(cache1,cache2);
		assertFalse(region1.isDestroyed());
		ctx1.close();
		assertFalse(cache1.isClosed());
		assertFalse("region was destroyed" ,region1.isDestroyed());
	}

	@AfterClass
	public static void cleanUp() throws InterruptedException {
		Thread.sleep(3000);
		ForkUtil.sendSignal();
	}
}
