/*
 * Copyright 2012-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.client;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.CacheLoaderException;
import org.apache.geode.cache.LoaderHelper;
import org.apache.geode.cache.Region;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.AbstractGemFireClientServerIntegrationTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;

/**
 * The ClientCachePoolTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("all")
public class ClientCachePoolTests extends AbstractGemFireClientServerIntegrationTest {

	private static ProcessWrapper gemfireServerProcess;

	@BeforeClass
	public static void setupGemFireServer() throws Exception {
		gemfireServerProcess = startGemFireServer(ClientCachePoolTests.class);
	}

	@AfterClass
	public static void tearDownGemFireServer() {
		stopGemFireServer(gemfireServerProcess);
	}

	@Resource(name = "Factorials")
	private Region<Long, Long> factorials;

	@Test
	public void computeFactorials() {
		assertThat(factorials.get(0l), is(equalTo(1l)));
		assertThat(factorials.get(1l), is(equalTo(1l)));
		assertThat(factorials.get(2l), is(equalTo(2l)));
		assertThat(factorials.get(3l), is(equalTo(6l)));
		assertThat(factorials.get(4l), is(equalTo(24l)));
		assertThat(factorials.get(5l), is(equalTo(120l)));
		assertThat(factorials.get(6l), is(equalTo(720l)));
		assertThat(factorials.get(7l), is(equalTo(5040l)));
		assertThat(factorials.get(8l), is(equalTo(40320l)));
		assertThat(factorials.get(9l), is(equalTo(362880l)));
	}

	public static class FactorialsClassLoader implements CacheLoader<Long, Long> {

		@Override
		public Long load(LoaderHelper<Long, Long> helper) throws CacheLoaderException {
			Long number = helper.getKey();

			Assert.notNull(number, "number must not be null");
			Assert.isTrue(number >= 0, String.format("number [%1$d] must be greater than equal to 0", number));

			if (number <= 2l) {
				return (number < 2l ? 1l : 2l);
			}

			long result = number;

			while (--number > 1) {
				result *= number;
			}

			return result;
		}

		@Override
		public void close() {
		}
	}
}
