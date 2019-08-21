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
package org.springframework.data.gemfire.function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.cache.execute.RegionFunctionContext;
import org.apache.geode.cache.execute.ResultSender;

import org.junit.Test;

import org.springframework.data.gemfire.function.annotation.Filter;
import org.springframework.data.gemfire.function.annotation.RegionData;

/**
 * @author David Turanski
 * @author John Blum
 */
public class FunctionArgumentResolverTest {

	@Test
	public void testDefaultFunctionArgumentResolverWithNoArguments() {
		FunctionArgumentResolver functionArgumentResolver = new DefaultFunctionArgumentResolver();
		FunctionContext mockFunctionContext = mock(FunctionContext.class);

		when(mockFunctionContext.getArguments()).thenReturn(null);

		Object[] args = functionArgumentResolver.resolveFunctionArguments(mockFunctionContext);

		assertNotNull(args);
		assertEquals(0, args.length);
	}

	@Test
	public void testDefaultFunctionArgumentResolverWithArgumentArray() {
		FunctionArgumentResolver functionArgumentResolver = new DefaultFunctionArgumentResolver();
		FunctionContext functionContext = mock(FunctionContext.class);

		when(functionContext.getArguments()).thenReturn(new String[] { "one", "two", "three" });

		Object[] args = functionArgumentResolver.resolveFunctionArguments(functionContext);

		assertNotNull(args);
		assertFalse(args instanceof String[]);
		assertEquals(3, args.length);
		assertEquals("one", args[0]);
		assertEquals("two", args[1]);
		assertEquals("three", args[2]);
	}

	@Test
    public void testDefaultFunctionArgumentResolverWithSingleArgument() {
        FunctionArgumentResolver functionArgumentResolver = new DefaultFunctionArgumentResolver();
        FunctionContext functionContext = mock(FunctionContext.class);

        when(functionContext.getArguments()).thenReturn("test");

        Object[] args = functionArgumentResolver.resolveFunctionArguments(functionContext);

		assertNotNull(args);
        assertEquals(1, args.length);
        assertEquals("test", args[0]);
    }

    @Test
    public void testMethodWithNoSpecialArgs() throws SecurityException, NoSuchMethodException {
        RegionFunctionContext functionContext = mock(RegionFunctionContext.class);

        Method method = TestFunction.class.getDeclaredMethod("methodWithNoSpecialArgs", String.class, int.class,
                boolean.class);
        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{"hello", 0, false};
        when(functionContext.getArguments()).thenReturn(originalArgs);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(originalArgs.length, args.length);

        int i = 0;
        for (Object arg : args) {
            assertSame(originalArgs[i++], arg);
        }
    }

    @Test
    public void testMethodWithRegionType() throws SecurityException, NoSuchMethodException {
        RegionFunctionContext functionContext = mock(RegionFunctionContext.class);
        @SuppressWarnings("unchecked")
        Region<Object, Object> region = mock(Region.class);

        Method method = TestFunction.class.getDeclaredMethod("methodWithRegionType", String.class, Region.class);
        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{"hello"};
        when(functionContext.getArguments()).thenReturn(originalArgs);
        when(functionContext.getDataSet()).thenReturn(region);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(originalArgs.length + 1, args.length);

        int i = 0;
        for (Object arg : args) {
            if (i != 1) {
                assertSame(originalArgs[i++], arg);
            } else {
                assertSame(region, arg);
            }
        }
    }

    @Test
    public void testMethodWithOneArgRegionType() throws SecurityException, NoSuchMethodException {
        RegionFunctionContext functionContext = mock(RegionFunctionContext.class);
        @SuppressWarnings("unchecked")
        Region<Object, Object> region = mock(Region.class);

        Method method = TestFunction.class.getDeclaredMethod("methodWithOneArgRegionType", Region.class);
        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{};
        when(functionContext.getArguments()).thenReturn(originalArgs);
        when(functionContext.getDataSet()).thenReturn(region);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(1, args.length);
        assertSame(region, args[0]);
    }

    @Test
    public void testMethodWithAnnotatedRegion() throws SecurityException, NoSuchMethodException {
        RegionFunctionContext functionContext = mock(RegionFunctionContext.class);
        @SuppressWarnings("unchecked")
        Region<Object, Object> region = mock(Region.class);

        Method method = TestFunction.class.getDeclaredMethod("methodWithAnnotatedRegion", Region.class, String.class);
        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{"hello"};
        when(functionContext.getArguments()).thenReturn(originalArgs);
        when(functionContext.getDataSet()).thenReturn(region);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(2, args.length);
        assertSame(region, args[0]);
        assertSame(originalArgs[0], args[1]);
    }

    @Test
    public void testMethodWithFunctionContext() throws SecurityException, NoSuchMethodException {
        RegionFunctionContext functionContext = mock(RegionFunctionContext.class);
        @SuppressWarnings("unchecked")
        Region<Object, Object> region = mock(Region.class);

        Method method = TestFunction.class.getDeclaredMethod("methodWithFunctionContext", Map.class, String.class,
                FunctionContext.class);
        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{"hello"};
        when(functionContext.getArguments()).thenReturn(originalArgs);
        when(functionContext.getDataSet()).thenReturn(region);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(3, args.length);
        assertSame(region, args[0]);
        assertSame(originalArgs[0], args[1]);
        assertSame(functionContext, args[2]);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    @Test
    public void testMethodWithResultSender() throws SecurityException, NoSuchMethodException {
        RegionFunctionContext functionContext = mock(RegionFunctionContext.class);
        ResultSender resultSender = mock(ResultSender.class);
        Region<Object, Object> region = mock(Region.class);

        Method method = TestFunction.class.getDeclaredMethod("methodWithResultSender", Map.class, ResultSender.class);
        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{};
        when(functionContext.getArguments()).thenReturn(originalArgs);
        when(functionContext.getDataSet()).thenReturn(region);
        when(functionContext.getResultSender()).thenReturn(resultSender);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(2, args.length);
        assertSame(region, args[0]);
        assertSame(resultSender, args[1]);
    }


    @SuppressWarnings("unchecked")
    @Test
    public void testMethodWithFilterAndRegion() throws SecurityException, NoSuchMethodException {
        RegionFunctionContext functionContext = mock(RegionFunctionContext.class);
        Region<Object, Object> region = mock(Region.class);

        Method method = TestFunction.class.getDeclaredMethod("methodWithFilterAndRegion", Map.class, Set.class,
                Object.class);
        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{new Object()};
        when(functionContext.getArguments()).thenReturn(originalArgs);
        when(functionContext.getDataSet()).thenReturn(region);
        @SuppressWarnings("rawtypes")
        Set keys = new HashSet<String>();
        when(functionContext.getFilter()).thenReturn(keys);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(3, args.length);
        assertSame(region, args[0]);
        assertSame(originalArgs[0], args[2]);
        assertSame(keys, args[1]);
    }

	@Test(expected = IllegalStateException.class)
	public void testMethodWithMultipleRegionData() throws SecurityException, NoSuchMethodException {
		Method method = TestFunction.class.getDeclaredMethod("methodWithMultipleRegionData", Map.class, Map.class);
		new FunctionContextInjectingArgumentResolver(method);
	}

	@Test(expected = IllegalArgumentException.class)
    public void testMethodWithMultipleRegions() throws SecurityException, NoSuchMethodException {
        Method method = TestFunction.class.getDeclaredMethod("methodWithMultipleRegions", Region.class, Map.class);
		new FunctionContextInjectingArgumentResolver(method);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMethodWithInvalidTypeForAnnotation() throws SecurityException, NoSuchMethodException {
        Method method = TestFunction.class.getDeclaredMethod("methodWithInvalidTypeForAnnotation", Region.class);
		new FunctionContextInjectingArgumentResolver(method);
    }

    @Test(expected = IllegalStateException.class)
    public void testMethodWithMultipleFunctionContext() throws SecurityException, NoSuchMethodException {
        Method method = TestFunction.class.getDeclaredMethod("methodWithMultipleFunctionContext",
			FunctionContext.class, FunctionContext.class);
		new FunctionContextInjectingArgumentResolver(method);
    }

    @Test
	@SuppressWarnings("unchecked")
    public void testMethodWithFunctionContextAndResultSender() throws NoSuchMethodException {
        FunctionContext functionContext = mock(FunctionContext.class);
        ResultSender resultSender = mock(ResultSender.class);
        Method method = TestFunction.class.getDeclaredMethod("methodWithFunctionContextAndResultSender",
			FunctionContext.class, ResultSender.class);

        FunctionArgumentResolver far = new FunctionContextInjectingArgumentResolver(method);

        Object[] originalArgs = new Object[]{};
        when(functionContext.getArguments()).thenReturn(originalArgs);
        when(functionContext.getResultSender()).thenReturn(resultSender);

        Object[] args = far.resolveFunctionArguments(functionContext);

        assertEquals(2, args.length);
        assertSame(functionContext, args[0]);
        assertSame(resultSender, args[1]);
    }

	@SuppressWarnings("unused")
    static class TestFunction {

        public void methodWithNoSpecialArgs(String s1, int i1, boolean b1) {
        }

        public void methodWithRegionType(String s1, Region<?, ?> region) {
        }

        public void methodWithOneArgRegionType(Region<?, ?> region) {
        }

        public void methodWithAnnotatedRegion(@RegionData Region<?, ?> data, String s1) {
        }

        public void methodWithFunctionContext(@RegionData Map<?, ?> data, String s1, FunctionContext fc) {
        }

        public void methodWithResultSender(@RegionData Map<?, ?> data, ResultSender<?> resultSender) {
        }

        public void methodWithFilterAndRegion(@RegionData Map<String, Object> region, @Filter Set<String> keys, Object arg) {
        }

        //Invalid Method Signatures
        public void methodWithMultipleRegionData(@RegionData Map<?, ?> r1, @RegionData Map<?, ?> r2) {
        }

        public void methodWithMultipleRegions(Region<?, ?> r1, @RegionData Map<?, ?> r2) {
        }

        public void methodWithInvalidTypeForAnnotation(@Filter Region<?, ?> r1) {
        }

        public void methodWithMultipleFunctionContext(FunctionContext fc1, FunctionContext fc2) {
        }

        public void methodWithFunctionContextAndResultSender(FunctionContext fc1, ResultSender<?> rs) {

        }
    }

}
