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

package org.springframework.data.gemfire;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.util.Properties;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.gemfire.function.sample.HelloFunctionExecution;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionContext;

/**
 * The LazyWiringDeclarableSupportFunctionBasedIntegrationTest class is a test suite of test cases testing the contract
 * and functionality of a GemFire Function implementing LazyWiringDeclarableSupport, defined using native GemFire
 * configuration metadata (cache.xml).
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.7.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LazyWiringDeclarableSupportFunctionBasedIntegrationTest {

	@Autowired
	private Cache gemfireCache;

	@Autowired
	private HelloFunctionExecution helloFunctionExecution;

/*
	@BeforeClass
	public static void setupBeforeClass() {
		Cache gemfireCache = new CacheFactory()
			.set("name", LazyWiringDeclarableSupportFunctionBasedIntegrationTest.class.getSimpleName())
			.set("mcast-port", "0")
			.set("log-level", "config")
			.set("cache-xml-file", null)
			.create();

		assertThat(gemfireCache, is(notNullValue()));
		assertThat(SpringContextBootstrappingInitializer.getApplicationContext(), is(notNullValue()));
	}

	@AfterClass
	public static void tearDownAfterClass() {
		CacheFactory.getAnyInstance().close();
	}
*/

	@Test
	public void helloGreeting() {
		assertThat(helloFunctionExecution.hello(null), is(equalTo("Hello Everyone!")));
	}

	protected static abstract class FunctionAdaptor extends LazyWiringDeclarableSupport implements Function {

		private final String id;

		public FunctionAdaptor(final String id) {
			Assert.hasText(id, "The Function ID must be specified!");
			this.id = id;
		}

		@Override
		public String getId() {
			return id;
		}

		@Override
		public boolean hasResult() {
			return true;
		}

		@Override
		public boolean isHA() {
			return false;
		}

		@Override
		public boolean optimizeForWrite() {
			return false;
		}
	}

	public static class HelloGemFireFunction extends FunctionAdaptor {

		protected static final String ADDRESS_TO_PARAMETER = "hello.address.to";
		protected static final String DEFAULT_ADDRESS_TO = "World";
		protected static final String HELLO_GREETING = "Hello %1$s!";
		protected static final String ID = "hello";

		@Value("${hello.default.address.to}")
		private String defaultAddressTo;

		private String addressTo;

		public HelloGemFireFunction() {
			super(ID);
		}

		protected String getAddressTo() {
			return addressTo;
		}

		protected String getDefaultAddressTo() {
			return (StringUtils.hasText(defaultAddressTo) ? defaultAddressTo : DEFAULT_ADDRESS_TO);
		}

		@Override
		protected void doPostInit(final Properties parameters) {
			addressTo = parameters.getProperty(ADDRESS_TO_PARAMETER, getDefaultAddressTo());
		}

		@Override
		public void execute(final FunctionContext context) {
			context.getResultSender().lastResult(formatHelloGreeting(addressTo(context)));
		}

		// precedence is... 1. Caller 2. GemFire 3. Spring
		protected String addressTo(FunctionContext context) {
			Object arguments = context.getArguments();
			String addressTo = null;

			if (arguments instanceof Object[]) {
				Object[] args = (Object[]) arguments;
				addressTo = (args.length > 0 && args[0] != null ? String.valueOf(args[0]) : null);
			}
			else if (arguments != null) {
				addressTo = String.valueOf(arguments);
			}

			return (StringUtils.hasText(addressTo) ? addressTo : getAddressTo());
		}

		protected String formatHelloGreeting(String addressTo) {
			return String.format(HELLO_GREETING, addressTo);
		}
	}

}
