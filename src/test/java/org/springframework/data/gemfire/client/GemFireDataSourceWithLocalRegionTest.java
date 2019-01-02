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
package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.fork.SpringCacheServerProcess;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("/org/springframework/data/gemfire/client/datasource-client-with-regions.xml")
public class GemFireDataSourceWithLocalRegionTest {
	@Autowired 
	ApplicationContext ctx;
	
	@BeforeClass
	public static void startUp() throws Exception {
		ForkUtil.startCacheServer(SpringCacheServerProcess.class.getName() + " " 
				+ "/org/springframework/data/gemfire/client/datasource-server.xml");
	}
	
	 
	@Test
	public void testRegionDefinitionNotOverridden() {
		 Region<?,?> simple = ctx.getBean("simple",Region.class);
		 assertEquals(DataPolicy.NORMAL,
				 simple.getAttributes().getDataPolicy());
	}
	
 
	 
	
	@AfterClass
	public static void cleanUp() {
		ForkUtil.sendSignal();
	}

}
