/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.data.gemfire.function.config.FunctionExecutionBeanDefinitionParser;
import org.springframework.data.gemfire.repository.config.GemfireRepositoryConfigurationExtension;
import org.springframework.data.repository.config.RepositoryBeanDefinitionParser;
import org.springframework.data.repository.config.RepositoryConfigurationExtension;

/**
 * Spring {@link org.springframework.beans.factory.xml.NamespaceHandler} for Spring Data GemFire
 * XML namespace (XSD) bean definitions.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author Oliver Gierke
 * @author John Blum
 */
@SuppressWarnings("unused")
class GemfireDataNamespaceHandler extends NamespaceHandlerSupport {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void init() {

		RepositoryConfigurationExtension extension = new GemfireRepositoryConfigurationExtension();

		registerBeanDefinitionParser("datasource", new GemfireDataSourceParser());
		registerBeanDefinitionParser("function-executions", new FunctionExecutionBeanDefinitionParser());
		registerBeanDefinitionParser("json-region-autoproxy", new GemfireRegionAutoProxyParser());
		registerBeanDefinitionParser("repositories", new RepositoryBeanDefinitionParser(extension));
		registerBeanDefinitionParser("snapshot-service", new SnapshotServiceParser());
	}
}
