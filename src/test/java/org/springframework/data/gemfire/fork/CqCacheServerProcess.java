/*
 * Copyright 2011-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.fork;

import java.io.IOException;
import java.util.Scanner;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionFactory;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.Scope;
import org.apache.geode.cache.server.CacheServer;

import org.springframework.data.gemfire.ForkUtil;

/**
 * @author Costin Leau
 * @author John Blum
 */
@SuppressWarnings("unchecked")
public class CqCacheServerProcess {

	private static final int DEFAULT_CACHE_SERVER_PORT = 40404;

	private static Region<String, Integer> testCqRegion;

	private static final String CACHE_SERVER_PORT_PROPERTY = "spring.data.gemfire.cache.server.port";
	private static final String GEMFIRE_LOG_LEVEL = "error";
	private static final String GEMFIRE_NAME = "CqServer";

	@SuppressWarnings("deprecation")
	public static void main(final String[] args) throws Exception {
		waitForShutdown(registerShutdownHook(startCacheServer(addRegion(
			newGemFireCache(GEMFIRE_NAME, GEMFIRE_LOG_LEVEL), "test-cq"))));
	}

	private static Cache newGemFireCache(String name, String logLevel) {

		return new CacheFactory()
			.set("name", name)
			.set("log-level", logLevel)
			.create();
	}

	private static Cache addRegion(Cache gemfireCache, String name) {
		RegionFactory<String, Integer> regionFactory = gemfireCache.createRegionFactory(RegionShortcut.REPLICATE);

		regionFactory.setScope(Scope.DISTRIBUTED_ACK);

		testCqRegion = regionFactory.create(name);

		return gemfireCache;
	}

	private static Cache startCacheServer(Cache gemfireCache) throws IOException {
		CacheServer cacheServer = gemfireCache.addCacheServer();
		cacheServer.setPort(getCacheServerPort(DEFAULT_CACHE_SERVER_PORT));
		cacheServer.start();
		return gemfireCache;
	}

	private static int getCacheServerPort(int defaultPort) {
		return Integer.getInteger(CACHE_SERVER_PORT_PROPERTY, defaultPort);
	}

	private static Cache registerShutdownHook(Cache gemfireCache) {
		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			if (gemfireCache != null) {
				try {
					gemfireCache.close();
				}
				catch (CacheClosedException ignore) {
				}
			}
		}));

		return gemfireCache;
	}

	@SuppressWarnings("deprecation")
	private static void waitForShutdown(Cache gemfireCache) throws IOException {
		ForkUtil.createControlFile(CqCacheServerProcess.class.getName());

		Scanner scanner = new Scanner(System.in);

		scanner.nextLine();

		testCqRegion.put("one", 1);
		testCqRegion.put("two", 2);
		testCqRegion.put("three", 3);

		scanner.nextLine();
	}
}
