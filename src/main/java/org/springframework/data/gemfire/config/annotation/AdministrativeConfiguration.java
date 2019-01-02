/*
 * Copyright 2017-2019 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import org.apache.geode.cache.execute.Function;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.config.admin.functions.CreateIndexFunction;
import org.springframework.data.gemfire.config.admin.functions.CreateRegionFunction;
import org.springframework.data.gemfire.config.admin.functions.ListIndexesFunction;
import org.springframework.data.gemfire.function.config.EnableGemfireFunctions;

/**
 * The {@link AdministrativeConfiguration} class is a Spring {@link Configuration @Configuration} class that registers
 * SDG Administrative {@link Function Functions} used by SDG's {@link EnableClusterConfiguration} without HTTP enabled.
 *
 * Additionally, this class enable's SDG {@link Function} implementations so that the internal SDG administrative
 * {@link Function Functions} are properly created and registered in Apache Geode.
 *
 * @author John Blum
 * @see org.apache.geode.cache.execute.Function
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.config.admin.functions.CreateIndexFunction
 * @see org.springframework.data.gemfire.config.admin.functions.CreateRegionFunction
 * @see org.springframework.data.gemfire.config.admin.functions.ListIndexesFunction
 * @see org.springframework.data.gemfire.function.config.EnableGemfireFunctions
 * @since 2.0.3
 */
@Configuration
@EnableGemfireFunctions
@SuppressWarnings("unused")
public class AdministrativeConfiguration {

	@Bean
	public CreateIndexFunction createIndexFunction() {
		return new CreateIndexFunction();
	}

	@Bean
	public CreateRegionFunction createRegionFunction() {
		return new CreateRegionFunction();
	}

	@Bean
	public ListIndexesFunction listIndexFunction() {
		return new ListIndexesFunction();
	}
}
