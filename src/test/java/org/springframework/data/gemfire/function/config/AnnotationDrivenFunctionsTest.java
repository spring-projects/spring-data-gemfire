/*
 * Copyright 2002-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.config;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.function.annotation.Filter;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.data.gemfire.function.annotation.RegionData;
import org.springframework.stereotype.Component;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author David Turanski
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class AnnotationDrivenFunctionsTest {
	@Autowired
	ApplicationContext applicationContext;

	@Test
	public void testAnnotatedFunctions() {

		assertTrue(FunctionService.isRegistered("foo"));

		Function function = FunctionService.getFunction("foo");
		assertFalse(function.isHA());
		assertFalse(function.optimizeForWrite());
		assertFalse(function.hasResult());

		assertTrue(FunctionService.isRegistered("bar"));
		function = FunctionService.getFunction("bar");
		assertTrue(function.isHA());
		assertFalse(function.optimizeForWrite());
		assertTrue(function.hasResult());

		assertTrue(FunctionService.isRegistered("foo2"));
		function = FunctionService.getFunction("foo2");
		assertTrue(function.isHA());
		assertTrue(function.optimizeForWrite());
		assertTrue(function.hasResult());

		assertTrue(FunctionService.isRegistered("injectFilter"));
		function = FunctionService.getFunction("injectFilter");
		assertTrue(function.isHA());
		assertTrue(function.optimizeForWrite());
		assertTrue(function.hasResult());
	}

	@Component
	public static class FooFunction {
		@GemfireFunction
		public void foo() {
		}

		@GemfireFunction(HA = true, optimizeForWrite = false)
		public String bar() {
			return null;
		}
	}

	@Component
	public static class Foo2Function {
		@GemfireFunction(id = "foo2", HA = true, optimizeForWrite = true)
		public List<String> foo(Object someVal, @RegionData Map<?, ?> region, Object someOtherValue) {
			return null;
		}

		@GemfireFunction(id = "injectFilter", HA = true, optimizeForWrite = true)
		public List<String> injectFilter(@Filter Set<?> keySet) {
			return null;
		}
	}

}
