/*
 * Copyright 2002-2020 the original author or authors.
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
package org.springframework.data.gemfire.function.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.ResultSender;
import org.apache.geode.security.ResourcePermission;

/**
 *
 * Used to declare a concrete method as a Pivotal GemFire {@link Function} implementation.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.execute.Function
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface GemfireFunction {

	String DEFAULT_RESOURCE_PERMISSION = "DATA:WRITE";

	/**
	 * The {@link String name} of the registered {@link Function}.
	 *
	 * If not provided the simple {@link String method name} will be used.
	 *
	 * @return the {@link Function} {@link Function#getId() id}.
	 * @see org.apache.geode.cache.execute.Function#getId()
     */
	String id() default "";

	/**
	 * Controls the maximum number of results sent at one time.
	 *
	 * @return an integer value indicating the batch size, or the number of results sent at one time.
	 */
	int batchSize() default 0;

	/**
	 * Attribute used to configure whether the {@link Function} is HA (Highly Available).
	 *
	 * @return a boolean value configuring whether the defined {@link Function} is HA.
	 * @see org.apache.geode.cache.execute.Function#isHA()
	 */
	boolean HA() default false;

	/**
	 * Normally follows the method return type, i.e. {@literal false} if {@code void}, {@literal true} otherwise.
	 *
	 * This allows overriding a {@code void} method which uses the {@link ResultSender} directly.
	 *
	 * @return a boolean value indicating if the {@link Function} is expected to return a result.
	 * @see org.apache.geode.cache.execute.Function#hasResult()
	 */
	boolean hasResult() default false;

	/**
	 * Attribute to configure whether the {@link Function} is optimized for write operations.
	 *
	 * @return a boolean value indicating if the {@link Function} is configured for optimized write operations.
	 * @see org.apache.geode.cache.execute.Function#optimizeForWrite()
	 */
	boolean optimizeForWrite() default false;

	/**
	 * Returns the list of {@link ResourcePermission} required by this {@link Function}.
	 *
	 * By default, {@link Function Functions} require {@literal DATA:WRITE} permission.
	 */
	String[] requiredPermissions() default { DEFAULT_RESOURCE_PERMISSION };

}
