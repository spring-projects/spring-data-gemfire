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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.*;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.InterestPolicy;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.SubscriptionAttributes;

/**
 * Test to ensure subscription policy can be applied to server regions.
 * 
 * @author Lyndon Adams
 * @since 13 March 2013
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("/org/springframework/data/gemfire/config/subscription-ns.xml")
public class CacheSubscriptionTest{
	
	@Autowired ApplicationContext context;
	
	@SuppressWarnings("rawtypes")
	@Test
	public void testRRSubscriptionAllPolicy() throws Exception {
		assertTrue(context.containsBean("replicALL"));
		RegionFactoryBean fb = context.getBean("&replicALL", RegionFactoryBean.class);
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		
		SubscriptionAttributes sa = attrs.getSubscriptionAttributes();
		assertEquals(InterestPolicy.ALL, sa.getInterestPolicy() );
	}

	@SuppressWarnings("rawtypes")
	@Test
	public void testPRSubscriptionCacheContentPolicy() throws Exception {
		assertTrue(context.containsBean("partCACHE_CONTENT"));
		RegionFactoryBean fb = context.getBean("&partCACHE_CONTENT", RegionFactoryBean.class);
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		
		SubscriptionAttributes sa = attrs.getSubscriptionAttributes();
		assertEquals(InterestPolicy.CACHE_CONTENT, sa.getInterestPolicy() );
	}
	
	@SuppressWarnings("rawtypes")
	@Test
	public void testPRSubscriptionDefaultPolicy() throws Exception {
		assertTrue(context.containsBean("partDEFAULT"));
		RegionFactoryBean fb = context.getBean("&partDEFAULT", RegionFactoryBean.class);
		RegionAttributes attrs = TestUtils.readField("attributes", fb);
		
		SubscriptionAttributes sa = attrs.getSubscriptionAttributes();
		assertEquals(InterestPolicy.ALL, sa.getInterestPolicy() );
	}
	
}
