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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "client-cache.xml", initializers = GemfireTestApplicationContextInitializer.class)
public class ClientCacheTest {

	@Resource(name = "ChallengeQuestions")
	@SuppressWarnings("all")
	private Region<?, ?> region;

	@Test
	public void clientCacheIsNotClosed() {
		ConfigurableApplicationContext context = new ClassPathXmlApplicationContext(
			"/org/springframework/data/gemfire/client/client-cache-no-close.xml");

		Cache cache = context.getBean(Cache.class);

		context.close();

		assertThat(cache.isClosed(), is(false));
	}

	@Test
	public void poolNameEqualsDefault() {
		assertThat(region.getAttributes().getPoolName(), is(nullValue()));
	}

}
