/*
 * Copyright 2002-2019 the original author or authors.
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
package org.springframework.data.gemfire.function.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.TimeUnit;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.execute.FunctionException;
import org.apache.geode.cache.execute.ResultCollector;
import org.apache.geode.distributed.DistributedMember;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.data.gemfire.function.execution.GemfireOnServerFunctionTemplate;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = { TestClientCacheConfig.class })
public class FunctionExecutionClientCacheTests {

	@Autowired
	ApplicationContext applicationContext;

	@Test
	public void contextCreated() throws Exception {

		ClientCache cache = this.applicationContext.getBean("gemfireCache", ClientCache.class);

		Pool pool = this.applicationContext.getBean("gemfirePool", Pool.class);

		assertEquals("gemfirePool", pool.getName());
		assertTrue(cache.getDefaultPool().getLocators().isEmpty());
		assertEquals(1, cache.getDefaultPool().getServers().size());
		assertTrue(pool.getLocators().isEmpty());
		assertEquals(1, pool.getServers().size());
		assertEquals(pool.getServers().get(0), cache.getDefaultPool().getServers().get(0));

		Region region = this.applicationContext.getBean("r1", Region.class);

		assertEquals("r1", region.getName());
		assertNotNull(region.getAttributes());
		assertNull(region.getAttributes().getPoolName());

		GemfireOnServerFunctionTemplate template = this.applicationContext.getBean(GemfireOnServerFunctionTemplate.class);

		assertTrue(template.getResultCollector() instanceof MyResultCollector);
	}
}

@Configuration
@ImportResource("/org/springframework/data/gemfire/function/config/FunctionExecutionCacheClientTests-context.xml")
@EnableGemfireFunctionExecutions(basePackages = "org.springframework.data.gemfire.function.config.three")
class TestClientCacheConfig {

	@Bean
	MyResultCollector myResultCollector() {
		return new MyResultCollector();
	}
}

@SuppressWarnings("rawtypes")
class MyResultCollector implements ResultCollector {

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.execute.ResultCollector#addResult(org.apache.geode.distributed.DistributedMember, java.lang.Object)
	 */
	@Override
	public void addResult(DistributedMember arg0, Object arg1) {
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.execute.ResultCollector#clearResults()
	 */
	@Override
	public void clearResults() {
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.execute.ResultCollector#endResults()
	 */
	@Override
	public void endResults() {
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.execute.ResultCollector#getResult()
	 */
	@Override
	public Object getResult() throws FunctionException {
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.geode.cache.execute.ResultCollector#getResult(long, java.util.concurrent.TimeUnit)
	 */
	@Override
	public Object getResult(long arg0, TimeUnit arg1) throws FunctionException, InterruptedException {
		return null;
	}
}
