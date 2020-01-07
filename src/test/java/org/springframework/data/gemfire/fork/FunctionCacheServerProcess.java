/*
 * Copyright 2012-2020 the original author or authors.
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
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.cache.execute.FunctionService;
import org.apache.geode.cache.server.CacheServer;

import org.springframework.data.gemfire.ForkUtil;

/**
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
public class FunctionCacheServerProcess {

	private static final int DEFAULT_CACHE_SERVER_PORT = 40404;

	private static Region<Object,Object> testFunctionRegion;

	private static final String CACHE_SERVER_PORT_PROPERTY = "spring.data.gemfire.cache.server.port";
	private static final String GEMFIRE_LOG_LEVEL = "error";
	private static final String GEMFIRE_NAME = "FunctionServer";

	public static void main(String[] args) throws Exception {

		waitForShutdown(registerShutdownHook(registerFunctions(startCacheServer(
			addRegion(newGemFireCache(GEMFIRE_NAME, GEMFIRE_LOG_LEVEL), "test-function")))));
	}

	private static Cache newGemFireCache(String name, String logLevel) {

		return new CacheFactory()
			.set("name", name)
			.set("mcast-port", "0")
			.set("log-level", logLevel)
			.set("groups", "g1,g2,g3")
			.create();
	}

	private static Cache addRegion(Cache gemfireCache, String name) {

		RegionFactory<Object,Object> regionFactory = gemfireCache.createRegionFactory(RegionShortcut.REPLICATE);

		regionFactory.setScope(Scope.DISTRIBUTED_ACK);

		testFunctionRegion = regionFactory.create(name);
		testFunctionRegion.put("one", 1);
		testFunctionRegion.put("two", 2);
		testFunctionRegion.put("three", 3);

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

	private static Cache registerFunctions(Cache gemfireCache) {

		FunctionService.registerFunction(new EchoFunction());
		FunctionService.registerFunction(new ServerFunction());

		return gemfireCache;
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

	@SuppressWarnings({ "deprecation", "unused" })
	private static void waitForShutdown(Cache gemfireCache) throws IOException {

		ForkUtil.createControlFile(FunctionCacheServerProcess.class.getName());

		Scanner scanner = new Scanner(System.in);

		scanner.nextLine();
	}

	@SuppressWarnings("serial")
	static class EchoFunction implements Function {

		@Override
		public String getId() {
			return "echoFunction";
		}

		@Override
		public void execute(FunctionContext functionContext) {

			Object[] arguments = (Object[]) functionContext.getArguments();

			for (int index = 0; index < arguments.length; index++) {
				if ((index + 1) == arguments.length){
					functionContext.getResultSender().lastResult(arguments[index]);
				}
				else {
					functionContext.getResultSender().sendResult(arguments[index]);
				}
			}
		}
	}

	@SuppressWarnings("serial")
	static class ServerFunction implements Function {

		@Override
		public String getId() {
			return "serverFunction";
		}

		@Override
		public void execute(FunctionContext functionContext) {

			Object[] arguments = (Object[]) functionContext.getArguments();

			testFunctionRegion.put(arguments[0], arguments[1]);

			functionContext.getResultSender().lastResult(null);
		}
	}
}
