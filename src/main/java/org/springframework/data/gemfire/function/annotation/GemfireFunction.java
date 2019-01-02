/*
 * Copyright 2002-2019 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
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

/**
 * 
 * Used to declare a concrete method as a GemFire function implementation
 * 
 * @author David Turanski
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface GemfireFunction {

	/**
	 * The name of the registered function. If not provided the simple method name will be used
	 * @return the function id
     */
	String id() default "";

	/**
	 * Attribute to determine whether the GemFire Function is HA (Highly Available).
	 *
	 * @return a boolean value indicating whether the defined GemFire Function is HA.
	 */
	boolean HA() default false;

	/**
	 * Attribute to determine whether the GemFire Function is optimized for write operations.
	 *
	 * @return a boolean value indicating if the GemFire Function is configured for optimized write operations.
	 */
	boolean optimizeForWrite() default false;

	/**
	 * Controls the maximum number of results sent at one time.
	 *
	 * @return an integer value indicating the batch size, or the number of results sent at one time.
	 */
	int batchSize() default 0;

	/**
	 * Normally follows the method return type, i.e., false if void, true otherwise. This allows overriding
	 * a void method which uses the resultSender directly.
	 *
	 * @return a boolean value indicating if the GemFire Function is expected to return a result.
	 */
	boolean hasResult() default false;

}
