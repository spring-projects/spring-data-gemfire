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
package org.springframework.data.gemfire.function.config;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Import;

/**
 * Enables classpath scanning for interfaces annotated as GemFire function executions (function invocations).
 * These include interfaces annotated with one of {code} @OnRegion, @OnServer, @OnServers, @OnMember, @OnMembers{code} 
 * 
 * @author David Turanski
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@Import(FunctionExecutionBeanDefinitionRegistrar.class)
public @interface EnableGemfireFunctionExecutions {
 
	/**
	 * Alias for the {@link #basePackages()} attribute. Allows for more concise annotation declarations e.g.:
	 * {@code @EnableGemfireRepositories("org.my.pkg")} instead of
	 * {@code @EnableGemfireRepositories(basePackages="org.my.pkg")}.
	 *
	 * @return an array of Strings indicating the names of packages to scan during component scanning.
	 * @see #basePackages()
	 */
	String[] value() default {};

	/**
	 * Base packages to scan for annotated components. {@link #value()} is an alias for (and mutually exclusive with) this
	 * attribute. Use {@link #basePackageClasses()} for a type-safe alternative to String-based package names.
	 *
	 * @return an array of Strings indicating the names of packages to scan during component scanning.
	 * @see #basePackageClasses()
	 */
	String[] basePackages() default {};

	/**
	 * Type-safe alternative to {@link #basePackages()} for specifying the packages to scan for annotated components.
	 * The package of each class specified will be scanned. Consider creating a special no-op marker class or interface
	 * in each package that serves no purpose other than being referenced by this attribute.
	 *
	 * @return an array of Class types used to get the packages to scan during component scanning.
	 * @see #basePackages()
	 */
	Class<?>[] basePackageClasses() default {};

	/**
	 * Specifies which types are eligible for component scanning. Further narrows the set of candidate components from
	 * everything in {@link #basePackages()} to everything in the base packages that matches the given filter or filters.
	 *
	 * @return an array of Filters indicating which types are eligible for component scanning.
	 * @see org.springframework.context.annotation.ComponentScan.Filter
	 */
	Filter[] includeFilters() default {};

	/**
	 * Specifies which types are not eligible for component scanning.
	 *
	 * @return an array of Filters indicating which types are not eligible for component scanning.
	 * @see org.springframework.context.annotation.ComponentScan.Filter
	 */
	Filter[] excludeFilters() default {};

}
