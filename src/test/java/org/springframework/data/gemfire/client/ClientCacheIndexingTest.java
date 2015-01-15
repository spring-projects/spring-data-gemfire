/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.fork.SpringCacheServerProcess;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexType;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * The ClientCacheIndexingTest class is a test suite of test cases testing the creation and application of indexes
 * on client Regions of a GemFire ClientCache using the &lt;gfe:index/&gt; tag element in the Spring Data GemFire
 * XML namespace and configuration meta-data, which is backed by the IndexFactoryBean.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.query.Index
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.5.2
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClientCacheIndexingTest {

	private static final Object MUTEX = new Object();

	@Autowired
	private ClientCache clientCache;

	@Autowired
	private Index exampleIndex;

	@BeforeClass
	public static void setup() throws IOException {
		ForkUtil.startCacheServer(String.format("%1$s %2$s", SpringCacheServerProcess.class.getName(),
			"/org/springframework/data/gemfire/client/ClientCacheIndexingTest-server-context.xml"));
		waitForServerStart(TimeUnit.SECONDS.toMillis(20));
		System.out.println("GemFire Cache Server Process for ClientCache Indexing should be running...");
	}

	private static void waitForServerStart(final long milliseconds) {
		long timeout = (System.currentTimeMillis() + milliseconds);

		while (System.currentTimeMillis() < timeout
				&& ForkUtil.controlFileExists(SpringCacheServerProcess.class.getName())) {
			try {
				TimeUnit.MILLISECONDS.timedWait(MUTEX, 500);
			}
			catch (Exception ignore) {
			}
		}
	}

	@AfterClass
	public static void tearDown() {
		ForkUtil.sendSignal();
	}

	protected Index getIndex(final GemFireCache gemfireCache, final String indexName) {
		QueryService queryService = (gemfireCache instanceof ClientCache
			? ((ClientCache) gemfireCache).getLocalQueryService() : gemfireCache.getQueryService());

		for (Index index : queryService.getIndexes()) {
			if (index.getName().equals(indexName)) {
				return index;
			}
		}

		return null;
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testIndexByName() {
		assertNotNull("The GemFire ClientCache was not properly configured and initialized!", clientCache);

		Index actualIndex = getIndex(clientCache, "ExampleIndex");

		assertNotNull(actualIndex);
		assertEquals("ExampleIndex", actualIndex.getName());
		assertEquals(IndexType.HASH, actualIndex.getType());
		assertSame(exampleIndex, actualIndex);
	}

}
