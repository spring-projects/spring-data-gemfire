/*
 * Copyright 2002-2012 the original author or authors.
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
package org.springframework.data.gemfire.function.config;

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
	 * is the function HA - highly available
	 */
	boolean HA() default false;
	/**
	 * is the function optimized for write operations
	 */
	boolean optimizeForWrite() default false;
}
