/*
 * Copyright 2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.listener.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The {@link ContinuousQuery} annotation to define a GemFire/Geode Continuous Query (CQ) on a POJO method
 * which handles all CQ events and errors.
 *
 * @author John Blum
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @since 2.0.0
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface ContinuousQuery {

	/**
	 * Determines whether the CQ is durable.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean durable() default false;

	/**
	 * {@link String Name} assigned to the registered CQ.
	 *
	 * Defaults to the fully-qualified method name.
	 */
	String name() default "";

	/**
	 * Defines the OQL query used by the CQ to determine CQ events.
	 */
	String query();

}
