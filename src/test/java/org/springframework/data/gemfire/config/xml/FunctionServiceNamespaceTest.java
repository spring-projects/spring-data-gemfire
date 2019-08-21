/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.apache.geode.cache.execute.FunctionAdapter;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.cache.execute.FunctionService;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="function-service-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class FunctionServiceNamespaceTest  {

	@Test
	public void testFunctionsRegistered() throws Exception {
		assertEquals(2, FunctionService.getRegisteredFunctions().size());
		assertNotNull(FunctionService.getFunction("function1"));
		assertNotNull(FunctionService.getFunction("function2"));
	}

	@SuppressWarnings("serial")
	public static class Function1 extends FunctionAdapter {

		@Override
		public void execute(FunctionContext arg0) {
		}

		@Override
		public String getId() {
			return "function1";
		}

	}

	@SuppressWarnings("serial")
	public static class Function2 extends FunctionAdapter {

		@Override
		public void execute(FunctionContext arg0) {
		}

		@Override
		public String getId() {
			return "function2";
		}

	}

}
