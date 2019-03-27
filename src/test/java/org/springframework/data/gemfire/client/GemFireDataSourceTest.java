/*
 * Copyright 2002-2019 the original author or authors.
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
package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.fork.SpringCacheServerProcess;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.Pool;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("/org/springframework/data/gemfire/client/datasource-client.xml")
public class GemFireDataSourceTest {
	@Autowired
	ApplicationContext ctx;

	@BeforeClass
	public static void startUp() throws Exception {
		ForkUtil.startCacheServer(SpringCacheServerProcess.class.getName() + " "
				+ "/org/springframework/data/gemfire/client/datasource-server.xml");
	}

	@SuppressWarnings("unused")
	@Test
	public void testServerDataSource() {
		Cache cache = ctx.getBean("gemfireCache", Cache.class);
		Pool pool = ctx.getBean("gemfirePool", Pool.class);
		assertEquals(true, pool.getSubscriptionEnabled());

		String regions[] = ctx.getBeanNamesForType(Region.class);
		List<String> regionList = Arrays.asList(regions);
		assertTrue(regionList.contains("r1"));
		assertTrue(regionList.contains("r2"));
		assertTrue(regionList.contains("simple"));

		Region<?, ?> simple = ctx.getBean("simple", Region.class);
		assertEquals(DataPolicy.EMPTY, simple.getAttributes().getDataPolicy());
	}

	@Test
	public void testRepositoryCreated() {
		PersonRepository repo = ctx.getBean(PersonRepository.class);
		Person dave = new Person(1L, "Dave", "Mathhews");
		repo.save(dave);
		Person saved = repo.findOne(1L);
		assertEquals("Dave", saved.getFirstname());
	}

	@AfterClass
	public static void cleanUp() {
		ForkUtil.sendSignal();
	}

}
