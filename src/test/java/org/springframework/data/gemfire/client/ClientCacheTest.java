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

import static org.junit.Assert.*;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.test.GemfireTestRunner;
import org.springframework.test.context.ContextConfiguration;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;

/**
 * 
 * @author David Turanski
 *
 */
@RunWith(GemfireTestRunner.class)
@ContextConfiguration("client-cache.xml")
public class ClientCacheTest {
	@Resource(name="challengeQuestionsRegion")
	Region<?,?> region;
	
	@Autowired
	ClientCache cache;
	
    @Test
    public void test() {
    	assertEquals("gemfirePool",region.getAttributes().getPoolName());
    }
}
