/*
 * Copyright 2002-2018 the original author or authors.
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
 * Annotation to declare an interface as a GemFire OnMembers Function Execution
 * @author David Turanski
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface OnMembers {

	/**
	 * The bean name of the POJO interface defining the GemFire Function executions.
	 *
	 * @return the bean name (id) of the POJO interface defining the GemFire Function executions.
	 */
	String id() default "";

	/**
	 * The GemFire Group to which the members must belong to target the Function execution.
	 *
	 * @return the name of the GemFire Group to which the members must belong for the targeted the Function execution.
	 */
	String groups() default "";

	/**
	 * Optional ResultCollector bean reference.
	 *
	 * @return an optional bean name of the ResultCollector to process the Function results.
	 */
	String resultCollector() default "";

}
