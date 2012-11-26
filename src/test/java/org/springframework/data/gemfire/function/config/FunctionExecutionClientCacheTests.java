/*
 * Copyright 2002-2012 the original author or authors.
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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.ImportResource;
import org.springframework.data.gemfire.function.config.EnableGemfireFunctionExecutions;
import org.springframework.data.gemfire.function.execution.GemfireFunctionProxyFactoryBean;
import org.springframework.data.gemfire.function.execution.OnRegionFunctionProxyFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.Pool;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes={TestClientCacheConfig.class})
public class FunctionExecutionClientCacheTests {
	@Autowired
	ApplicationContext context;
	
	@Test
	public void testContextCreated() throws Exception {
		
		//String name ="testClientOnRegionFunction";
		String name ="testClientOnServerFunction";
	    
	//	GemfireFunctionProxyFactoryBean factoryBean = (GemfireFunctionProxyFactoryBean)context.getBean("&"+name);
	 
		ClientCache cache = context.getBean("gemfireCache",ClientCache.class);
		Pool pool = context.getBean("gemfirePool",Pool.class);
		assertEquals("gemfirePool", pool.getName());
		assertEquals(1, cache.getDefaultPool().getServers().size());
		assertEquals(pool.getServers().get(0), cache.getDefaultPool().getServers().get(0));
		
 	
		context.getBean("r1",Region.class);
		//ComponentScan s;
		//FilterType f;
		
	}
	
}


@ImportResource("/org/springframework/data/gemfire/function/config/FunctionExecutionCacheClientTests-context.xml")
@EnableGemfireFunctionExecutions (basePackages = "org.springframework.data.gemfire.function.config.three",
 excludeFilters = {
		/*@ComponentScan.Filter(type=FilterType.ANNOTATION, value=OnRegion.class),
		@ComponentScan.Filter(type=FilterType.ANNOTATION, value=OnServer.class)*/
		}
)
@Configuration
class TestClientCacheConfig {
	
}



