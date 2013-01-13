/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.config;

import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.data.gemfire.function.config.FunctionExecutionBeanDefinitionParser;
import org.springframework.data.gemfire.repository.config.GemfireRepositoryConfigurationExtension;
import org.springframework.data.repository.config.RepositoryBeanDefinitionParser;
import org.springframework.data.repository.config.RepositoryConfigurationExtension;

/**
 * Namespace handler for GemFire definitions.
 * 
 * @author Costin Leau
 * @author David Turanski
 * @author Oliver Gierke
 */
class GemfireDataNamespaceHandler extends NamespaceHandlerSupport {

	@Override
	public void init() {
		// Repository namespace
		RepositoryConfigurationExtension extension = new GemfireRepositoryConfigurationExtension();
		registerBeanDefinitionParser("repositories", new RepositoryBeanDefinitionParser(extension));
		registerBeanDefinitionParser("function-executions", new FunctionExecutionBeanDefinitionParser());
	}
}