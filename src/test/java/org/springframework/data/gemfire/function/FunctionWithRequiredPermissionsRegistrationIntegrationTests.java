/*
 * Copyright 2018-2019 the original author or authors.
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

import java.lang.annotation.Annotation;

import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionService;
import org.apache.geode.security.ResourcePermission;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.data.gemfire.function.config.EnableGemfireFunctions;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests testing the registration of Apache Geode/Pivotal GemFire {@link Function Functions}
 * with required permissions specified using the {@link GemfireFunction} {@link Annotation}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.function.annotation.GemfireFunction
 * @see org.springframework.data.gemfire.function.config.EnableGemfireFunctions
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.1.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings({ "unchecked", "unused" })
public class FunctionWithRequiredPermissionsRegistrationIntegrationTests {

	@Test
	public void functionsRegisteredWithRequiredPermissionsSuccessfully() {

		Function function = FunctionService.getFunction("testFunctionWithRequiredPermissions");

		assertThat(function).isNotNull();
		assertThat(function.getRequiredPermissions("test")).containsExactly(
			new ResourcePermission(ResourcePermission.Resource.CLUSTER, ResourcePermission.Operation.MANAGE),
			new ResourcePermission(ResourcePermission.Resource.DATA, ResourcePermission.Operation.READ, "Example")
		);
	}

	@Configuration
	@EnableGemfireFunctions
	static class TestConfiguration {

		@Bean
		TestFunctions testFunctions() {
			return new TestFunctions();
		}
	}

	static class TestFunctions {

		@GemfireFunction(requiredPermissions = { "CLUSTER:MANAGE", "DATA:READ:Example" })
		public void testFunctionWithRequiredPermissions() { }

	}
}
