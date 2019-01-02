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

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.w3c.dom.Element;

/**
 * Parser for a &lt;function-executions&gt; bean definition.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.xml.BeanDefinitionParser
 * @see org.springframework.beans.factory.xml.ParserContext
 * @see org.w3c.dom.Element
 */
public class FunctionExecutionBeanDefinitionParser implements BeanDefinitionParser {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.xml.BeanDefinitionParser#parse(org.w3c.dom.Element, org.springframework.beans.factory.xml.ParserContext)
	 */
	@Override
	public BeanDefinition parse(Element element, ParserContext parserContext) {
		new FunctionExecutionBeanDefinitionRegistrar().registerBeanDefinitions(element, parserContext);
		return null;
	}
}
