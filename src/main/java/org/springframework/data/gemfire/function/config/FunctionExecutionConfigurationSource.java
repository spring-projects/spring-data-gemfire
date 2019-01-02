/*
<<<<<<< Updated upstream
 * Copyright 2002-2019 the original author or authors.
=======
 * Copyright 2002-2019 the original author or authors.
>>>>>>> Stashed changes
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

import java.lang.annotation.Annotation;

import org.springframework.core.type.filter.TypeFilter;

/**
 * Interface for Function Execution configuration sources (e.g. {@link Annotation} or XML configuration)
 * to configure classpath scanning of annotated interfaces to implement proxies that invoke Functions.
 *
 * @author David Turanski
 * @author John Blum
 */
public interface FunctionExecutionConfigurationSource {

	/**
	 * Returns the base packages the repository interfaces shall be found under.
	 *
	 * @return must not be {@literal null}.
	 */
	Iterable<String> getBasePackages();

	/**
	 * Returns configured {@link TypeFilter}s
	 * @return include filters
	 */
	Iterable<TypeFilter> getIncludeFilters();

	/**
	 * Returns configured {@link TypeFilter}s
	 * @return exclude filters
	 */
	Iterable<TypeFilter> getExcludeFilters();

	/**
	 * Returns the actual source object that the configuration originated from. Will be used by the tooling to give visual
	 * feedback on where the repository instances actually come from.
	 *
	 * @return must not be {@literal null}.
	 */
	Object getSource();

}
