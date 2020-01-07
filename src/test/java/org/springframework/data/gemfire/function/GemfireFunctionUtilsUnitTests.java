/*
 * Copyright 2018-2020 the original author or authors.
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

package org.springframework.data.gemfire.function;

import static org.assertj.core.api.Assertions.assertThat;

import java.lang.reflect.Method;
import java.util.Arrays;

import org.apache.geode.security.ResourcePermission;

import org.junit.Test;

import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;

/**
 * Unit tests for {@link GemfireFunctionUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.function.GemfireFunctionUtils
 * @since 2.1.0
 */
@SuppressWarnings("unused")
public class GemfireFunctionUtilsUnitTests {

	private final TestFunctions testFunctions = new TestFunctions();

	private ResourcePermission newResourcePermission(ResourcePermission.Resource resource,
			ResourcePermission.Operation operation, ResourcePermission.Target target, String key) {

		return new ResourcePermission(resource, operation, target.name(), key);
	}

	private ResourcePermission newResourcePermission(ResourcePermission.Resource resource,
			ResourcePermission.Operation operation, String target, String key) {

		return new ResourcePermission(resource, operation, target, key);
	}

	@Test
	public void gemfireFunctionAnnotatedMethodIsGemFireFunction() throws Exception {
		assertThat(GemfireFunctionUtils.isGemfireFunction(TestFunctions.class.getDeclaredMethod("testFunction")))
			.isTrue();
	}

	@Test
	public void nonGemfireFunctionAnnotatedMethodIsNotGemFireFunction() throws Exception {
		assertThat(GemfireFunctionUtils.isGemfireFunction(TestFunctions.class.getDeclaredMethod("nonFunction")))
			.isFalse();
	}

	@Test
	public void matchingGemFireFunctionReturnsTrue() throws Exception {
		assertThat(GemfireFunctionUtils.isMatchingGemfireFunction(
			TestFunctions.class.getDeclaredMethod("identifiedFunction"), "MyFunction")).isTrue();
	}

	@Test
	public void nonMatchingGemFireFunctionReturnsFalse() throws Exception {

		assertThat(GemfireFunctionUtils.isMatchingGemfireFunction(
			TestFunctions.class.getDeclaredMethod("identifiedFunction"), "YourFunction"))
				.isFalse();

		assertThat(GemfireFunctionUtils.isMatchingGemfireFunction(
			TestFunctions.class.getDeclaredMethod("nonFunction"), "nonFunction"))
				.isFalse();

		assertThat(GemfireFunctionUtils.isMatchingGemfireFunction(
			TestFunctions.class.getDeclaredMethod("requiredPermissionsFunction"), "MyFunction"))
				.isFalse();

		assertThat(GemfireFunctionUtils.isMatchingGemfireFunction(
			TestFunctions.class.getDeclaredMethod("testFunction"), "MockFunction"))
			.isFalse();
	}

	@Test
	public void resolvesFunctionIdFromGemfireFunctionIdAttribute() {

		AnnotationAttributes gemfireFunctionAttributes = new AnnotationAttributes();

		gemfireFunctionAttributes.put("id", "2");

		assertThat(GemfireFunctionUtils.resolveFunctionId(gemfireFunctionAttributes)).isEqualTo("2");
	}

	@Test
	public void unresolvableFunctionIdReturnsNull() {
		assertThat(GemfireFunctionUtils.resolveFunctionId(new AnnotationAttributes())).isNull();
	}

	@Test
	public void configureWithBatchSize() throws Exception {

		Method functionWithBatchSize = TestFunctions.class.getDeclaredMethod("functionWithBatchSize");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(functionWithBatchSize, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, functionWithBatchSize);

		GemfireFunctionUtils.configureBatchSize(this.testFunctions, functionWithBatchSize,
			gemfireFunctionAttributes, function);

		assertThat(function.getBatchSize()).isEqualTo(10);
	}

	@Test(expected = IllegalArgumentException.class)
	public void configureWithInvalidBatchSizeThrowsIllegalArgumentException() throws Exception {

		Method functionWithInvalidBatchSize =
			TestFunctions.class.getDeclaredMethod("functionWithInvalidBatchSize");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(functionWithInvalidBatchSize, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, functionWithInvalidBatchSize);

		try {
			GemfireFunctionUtils.configureBatchSize(this.testFunctions, functionWithInvalidBatchSize,
				gemfireFunctionAttributes, function);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("GemfireFunction.batchSize [-5] specified on [%1$s.%2$s] must be a non-negative value",
				testFunctions.getClass().getName(), functionWithInvalidBatchSize.getName());

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void configureWithNoBatchSize() throws Exception {

		Method testFunction = TestFunctions.class.getDeclaredMethod("testFunction");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(testFunction, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, testFunction);

		GemfireFunctionUtils.configureBatchSize(this.testFunctions, testFunction, gemfireFunctionAttributes, function);

		assertThat(function.getBatchSize()).isEqualTo(0);
	}

	@Test
	public void configureWithHighAvailability() throws Exception {

		Method functionWithHighAvailability = TestFunctions.class.getDeclaredMethod("functionWithHighAvailability");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(functionWithHighAvailability, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, functionWithHighAvailability);

		GemfireFunctionUtils.configureHighAvailability(gemfireFunctionAttributes, function);

		assertThat(function.isHA()).isTrue();
	}

	@Test
	public void configureWithNoHighAvailability() throws Exception {

		Method testFunction = TestFunctions.class.getDeclaredMethod("testFunction");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(testFunction, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, testFunction);

		GemfireFunctionUtils.configureHighAvailability(gemfireFunctionAttributes, function);

		assertThat(function.isHA()).isFalse();
	}

	@Test
	public void configureWithResult() throws Exception {

		Method functionWithResult = TestFunctions.class.getDeclaredMethod("functionWithResult");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(functionWithResult, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, functionWithResult);

		GemfireFunctionUtils.configureHasResult(gemfireFunctionAttributes, function);

		assertThat(function.hasResult()).isTrue();
	}

	@Test
	public void configureWithNoResult() throws Exception {

		Method testFunction = TestFunctions.class.getDeclaredMethod("testFunction");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(testFunction, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, testFunction);

		GemfireFunctionUtils.configureHasResult(gemfireFunctionAttributes, function);

		assertThat(function.hasResult()).isFalse();
	}

	@Test
	public void configureWithOptimizeForWrite() throws Exception {

		Method functionOptimizedForWrite = TestFunctions.class.getDeclaredMethod("functionOptimizedForWrite");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(functionOptimizedForWrite, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, functionOptimizedForWrite);

		GemfireFunctionUtils.configureOptimizeForWrite(gemfireFunctionAttributes, function);

		assertThat(function.optimizeForWrite()).isTrue();
	}

	@Test
	public void configureWithNoOptimizeForWrite() throws Exception {

		Method testFunction = TestFunctions.class.getDeclaredMethod("testFunction");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(testFunction, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, testFunction);

		GemfireFunctionUtils.configureOptimizeForWrite(gemfireFunctionAttributes, function);

		assertThat(function.optimizeForWrite()).isFalse();
	}

	@Test
	public void cofigureWithDefaultRequiredPermissions() throws Exception {

		Method testFunction = TestFunctions.class.getDeclaredMethod("testFunction");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(testFunction, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, testFunction);

		GemfireFunctionUtils.configureRequiredPermissions(gemfireFunctionAttributes, function);

		assertThat(function.getRequiredPermissions("test")).containsExactly(
			newResourcePermission(ResourcePermission.Resource.DATA, ResourcePermission.Operation.WRITE, (String) null, null)
		);
	}

	@Test
	public void configureWithRequiredPermissions() throws Exception {

		Method requiredPermissionsFunction = TestFunctions.class.getDeclaredMethod("requiredPermissionsFunction");

		AnnotationAttributes gemfireFunctionAttributes =
			GemfireFunctionUtils.getAnnotationAttributes(requiredPermissionsFunction, GemfireFunction.class);

		PojoFunctionWrapper function = new PojoFunctionWrapper(this.testFunctions, requiredPermissionsFunction);

		GemfireFunctionUtils.configureRequiredPermissions(gemfireFunctionAttributes, function);

		assertThat(function.getRequiredPermissions("Example")).containsExactly(
			newResourcePermission(ResourcePermission.Resource.CLUSTER, ResourcePermission.Operation.MANAGE, (String) null, null),
			newResourcePermission(ResourcePermission.Resource.DATA, ResourcePermission.Operation.MANAGE, "System", null),
			newResourcePermission(ResourcePermission.Resource.DATA, ResourcePermission.Operation.READ, ResourcePermission.Target.QUERY, "ALL"),
			newResourcePermission(ResourcePermission.Resource.DATA, ResourcePermission.Operation.WRITE, "Example", "123")
		);
	}

	public void testParseResourcePermissionWithInvalidArgumentThrowsIllegalArgumentException(
			String resourcePermissionString) {

		try {
			GemfireFunctionUtils.parseResourcePermission(resourcePermissionString);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("ResourcePermission [%s] is required", resourcePermissionString);
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseResourcePermissionWithNullStringThrowsIllegalArgumentException() {
		testParseResourcePermissionWithInvalidArgumentThrowsIllegalArgumentException(null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseResourcePermissionWithEmptyStringThrowsIllegalArgumentException() {
		testParseResourcePermissionWithInvalidArgumentThrowsIllegalArgumentException("");
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseResourcePermissionWithBlankStringThrowsIllegalArgumentException() {
		testParseResourcePermissionWithInvalidArgumentThrowsIllegalArgumentException("  ");
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseResourcePermissionWithInvalidResourceThrowsIllegalArgumentException() {

		try {
			GemfireFunctionUtils.parseResourcePermission("SOCKET:OPEN");
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("[SOCKET] is not a valid [%1$s] type; must be 1 of %2$s",
				ResourcePermission.Resource.class.getName(), Arrays.toString(ResourcePermission.Resource.values()));

			assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseResourcePermissionWithInvalidOperationThrowsIllegalArgumentException() {

		try {
			GemfireFunctionUtils.parseResourcePermission("DATA:COPY");
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("[COPY] is not a valid [%1$s] type; must be 1 of %2$s",
				ResourcePermission.Operation.class.getName(), Arrays.toString(ResourcePermission.Operation.values()));

			assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);

			throw expected;
		}
	}

	@Test
	public void parseResourcePermissionIsSuccessful() {

		ResourcePermission resourcePermission =
			GemfireFunctionUtils.parseResourcePermission("CLUSTER:MANAGE");

		assertThat(resourcePermission).isNotNull();
		assertThat(resourcePermission.getResource()).isEqualTo(ResourcePermission.Resource.CLUSTER);
		assertThat(resourcePermission.getOperation()).isEqualTo(ResourcePermission.Operation.MANAGE);
	}

	static class TestFunctions {

		@GemfireFunction(batchSize = 10)
		void functionWithBatchSize() { }

		@GemfireFunction(batchSize = -5)
		void functionWithInvalidBatchSize() { }

		@GemfireFunction(HA = true)
		void functionWithHighAvailability() { }

		@GemfireFunction(hasResult = true)
		void functionWithResult() { }

		@GemfireFunction(optimizeForWrite = true)
		void functionOptimizedForWrite() { }

		@GemfireFunction(id = "MyFunction")
		void identifiedFunction() { }

		void nonFunction() { }

		@GemfireFunction(requiredPermissions = {
			"CLUSTER:MANAGE",
			"DATA:MANAGE:System",
			"DATA:READ:QUERY:ALL",
			"DATA:WRITE:Example:123"
		})
		void requiredPermissionsFunction() { }

		@GemfireFunction
		void testFunction() { }

	}
}
