/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.Region;

import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * @author David Turanski
 * @author John Blum
 */
public class MultipleCacheTest {

	@Test
	public void testMultipleCaches() {
		String configLocation = "/org/springframework/data/gemfire/config/xml/MultipleCacheTest-context.xml";

		ConfigurableApplicationContext context1 = new ClassPathXmlApplicationContext(configLocation);
		ConfigurableApplicationContext context2 = new ClassPathXmlApplicationContext(configLocation);

		Cache cache1 = context1.getBean(Cache.class);
		Cache cache2 = context2.getBean(Cache.class);

		assertNotNull(cache1);
		assertSame(cache1, cache2);

		Region region1 = context1.getBean(Region.class);
		Region region2 = context2.getBean(Region.class);

		assertNotNull(region1);
		assertSame(region1, region2);
		assertFalse(cache1.isClosed());
		assertFalse(region1.isDestroyed());

		context1.close();

		assertFalse(cache1.isClosed());
		assertFalse("region was destroyed", region1.isDestroyed());
	}

}
