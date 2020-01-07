/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexType;
import org.apache.geode.cache.query.QueryService;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.process.ProcessExecutor;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.FileSystemUtils;
import org.springframework.data.gemfire.test.support.ThreadUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.Assert;

/**
<<<<<<< HEAD
 * The ClientCacheIndexingTest class is a test suite of test cases testing the creation and application of indexes
 * on client Regions of a Pivotal GemFireClientCache using the &lt;gfe:index/&gt; tag element in the SDG
 * XML namespace and configuration meta-data, which is backed by the IndexFactoryBean.
=======
 * Integration tests for testing {@link ClientCache} {@link Index Indexes}.
>>>>>>> 626076b0... DATAGEODE-180 - Polish log statements and log levels.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.query.Index
 * @see org.apache.geode.cache.query.QueryService
 * @since 1.5.2
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClientCacheIndexingTest {

	private static ProcessWrapper serverProcess;

	@Autowired
	private ClientCache clientCache;

	@Autowired
	private Index exampleIndex;

	@BeforeClass
	public static void setup() throws IOException {

		String serverName = "GemFireIndexingCacheServer";

		File serverWorkingDirectory = new File(FileSystemUtils.WORKING_DIRECTORY, serverName.toLowerCase());

		Assert.isTrue(serverWorkingDirectory.isDirectory() || serverWorkingDirectory.mkdirs(),
			String.format("Server working directory [%s] does not exist and could not be created", serverWorkingDirectory));

		List<String> arguments = new ArrayList<>();

		arguments.add("-Dgemfire.name=" + serverName);
		arguments.add("/org/springframework/data/gemfire/client/ClientCacheIndexingTest-server-context.xml");

		serverProcess = ProcessExecutor.launch(serverWorkingDirectory, ServerProcess.class,
			arguments.toArray(new String[0]));

		waitForServerStart(TimeUnit.SECONDS.toMillis(20));
	}

	private static void waitForServerStart(final long milliseconds) {

		ThreadUtils.timedWait(milliseconds, TimeUnit.MILLISECONDS.toMillis(500), new ThreadUtils.WaitCondition() {

			private File serverPidControlFile =
				new File(serverProcess.getWorkingDirectory(), ServerProcess.getServerProcessControlFilename());

			@Override
			public boolean waiting() {
				return !serverPidControlFile.isFile();
			}
		});
	}

	@AfterClass
	public static void tearDown() {

		serverProcess.shutdown();

		if (Boolean.valueOf(System.getProperty("spring.gemfire.fork.clean", Boolean.TRUE.toString()))) {
			org.springframework.util.FileSystemUtils.deleteRecursively(serverProcess.getWorkingDirectory());
		}
	}

	protected Index getIndex(GemFireCache gemfireCache, String indexName) {

		QueryService queryService = gemfireCache instanceof ClientCache
			? ((ClientCache) gemfireCache).getLocalQueryService()
			: gemfireCache.getQueryService();

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
