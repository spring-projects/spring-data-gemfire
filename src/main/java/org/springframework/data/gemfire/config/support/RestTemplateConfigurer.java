/*
 * Copyright 2019 the original author or authors.
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
package org.springframework.data.gemfire.config.support;

import org.springframework.web.client.RestTemplate;

/**
 * Configurer for a {@link RestTemplate}.
 *
 * @author John Blum
 * @see org.springframework.web.client.RestTemplate
 * @since 2.3.0
 */
@FunctionalInterface
public interface RestTemplateConfigurer {

	/**
	 * User-defined method and contract for applying custom configuration to the given {@link RestTemplate}.
	 *
	 * @param restTemplate {@link RestTemplate} to customize the configuration for.
	 * @see org.springframework.web.client.RestTemplate
	 */
	void configure(RestTemplate restTemplate);

}
