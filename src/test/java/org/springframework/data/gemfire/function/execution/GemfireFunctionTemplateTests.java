/*
 * Copyright 2002-2013 the original author or authors.
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
package org.springframework.data.gemfire.function.execution;

import static org.junit.Assert.assertEquals;

import java.util.Iterator;
import java.util.Properties;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.fork.FunctionCacheServerProcess;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;
import com.gemstone.gemfire.cache.client.PoolManager;

/**
 * @author David Turanski
 *
 */
public class GemfireFunctionTemplateTests {
	private static ClientCache cache = null;

	private static Pool pool = null;

	private static Region<String, Integer> clientRegion = null;

	@BeforeClass
	public static void startUp() throws Exception {
		ForkUtil.cacheServer(FunctionCacheServerProcess.class);

		Properties props = new Properties();
		props.put("mcast-port", "0");
		props.put("name", "function-client");
		props.put("log-level", "warning");

		ClientCacheFactory ccf = new ClientCacheFactory(props);
		ccf.setPoolSubscriptionEnabled(true);
		cache = ccf.create();

		PoolFactory pf = PoolManager.createFactory();
		pf.addServer("localhost", 40404);
		pf.setSubscriptionEnabled(true);
		pool = pf.create("client");

		ClientRegionFactory<String, Integer> crf = cache.createClientRegionFactory(ClientRegionShortcut.PROXY);
		crf.setPoolName("client");
		clientRegion = crf.create("test-function");
	}

	@AfterClass
	public static void cleanUp() {
		ForkUtil.sendSignal();
		if (clientRegion != null) {
			clientRegion.destroyRegion();
		}
		if (pool != null) {
			pool.destroy();
			pool = null;
		}

		if (cache != null) {
			cache.close();
		}
		cache = null;
	}
	 
	@Test 
	public void testFunctionTemplates() {
		 verifyfunctionTemplateExecution( new GemfireOnServerFunctionTemplate(cache));
		 verifyfunctionTemplateExecution( new GemfireOnServersFunctionTemplate(cache));
		 verifyfunctionTemplateExecution( new GemfireOnRegionFunctionTemplate(clientRegion)); 
		 verifyfunctionTemplateExecution( new GemfireOnServerFunctionTemplate(pool)); 
		 verifyfunctionTemplateExecution( new GemfireOnServersFunctionTemplate(pool)); 		 
	} 
	 
	private void verifyfunctionTemplateExecution(GemfireFunctionOperations functionTemplate) {
		Iterable<String> results = functionTemplate.execute("echoFunction","1","2","3");
		 
		Iterator<String> it = results.iterator();
		for (int i = 1; i<= 3; i++) {
			assertEquals(String.valueOf(i),it.next());
		}
	}

}
