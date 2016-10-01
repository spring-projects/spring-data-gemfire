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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.GemfireTransactionManager;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations= "/org/springframework/data/gemfire/config/xml/tx-ns.xml",
	initializers=GemfireTestApplicationContextInitializer.class)
public class TxManagerNamespaceTest {

	@Autowired ApplicationContext ctx;

	@Test
	public void testBasicCache() throws Exception {
		assertTrue(ctx.containsBean("gemfireTransactionManager"));
		//Check old style alias also registered
		assertTrue(ctx.containsBean("gemfire-transaction-manager"));

		GemfireTransactionManager tx = ctx.getBean("gemfireTransactionManager", GemfireTransactionManager.class);
		assertFalse(tx.isCopyOnRead());
	}
}
