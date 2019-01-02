/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.AfterTransaction;
import org.springframework.test.context.transaction.BeforeTransaction;
import org.springframework.transaction.annotation.Transactional;

/**
 * Simple TX integration test.
 *
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "basic-tx-config.xml" })
@Transactional
public class TxIntegrationTest {

	@Resource(name = "rollback-region")
	private Map<String, String> rollbackRegion;
	@Resource(name = "commit-region")
	private Map<String, String> commitRegion;

	private boolean txCommit = false;

	@BeforeTransaction
	public void addItemsToTheCache() {
		rollbackRegion.put("Vlaicu", "Aurel");
		rollbackRegion.put("Coanda", "Henri");
		commitRegion.put("Coanda", "Henri");
		commitRegion.put("Vlaicu", "Aurel");
		txCommit = false;
	}

	@Test
	public void testTransactionRollback() throws Exception {
		rollbackRegion.remove("Coanda");
		rollbackRegion.put("Ciurcu", "Alexandru");
	}

	@Test
	@Rollback(value = false)
	public void testTransactionCommit() throws Exception {
		commitRegion.put("Poenaru", "Petrache");
		commitRegion.remove("Coanda");
		commitRegion.put("Vlaicu", "A");

		txCommit = true;
	}

	@AfterTransaction
	public void testTxOutcome() {
		if (txCommit) {
			assertFalse(commitRegion.containsKey("Coanda"));
			assertTrue(commitRegion.containsKey("Poenaru"));
			assertTrue(commitRegion.containsValue("A"));
		}
		assertTrue(rollbackRegion.containsKey("Coanda"));
		assertTrue(rollbackRegion.containsKey("Vlaicu"));
		assertFalse(rollbackRegion.containsKey("Ciurcu"));
	}
}
