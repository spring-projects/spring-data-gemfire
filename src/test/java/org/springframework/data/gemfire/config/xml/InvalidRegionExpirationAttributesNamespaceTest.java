/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.Region;
import org.junit.Test;
import org.springframework.beans.factory.xml.XmlBeanDefinitionStoreException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.GenericXmlApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor;
import org.xml.sax.SAXParseException;

/**
 * Unit test testing the proper syntax for declaring "custom" expiration attributes on a {@link Region}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.support.GenericXmlApplicationContext
 * @see org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor
 * @see org.apache.geode.cache.ExpirationAttributes
 * @see org.apache.geode.cache.Region
 * @since 1.5.0
 */
public class InvalidRegionExpirationAttributesNamespaceTest {

	private String contextConfigLocation() {
		return getClass().getName().replaceAll("\\.", "/").concat("-context.xml");
	}

	private ConfigurableApplicationContext createApplicationContext() {
		return new GenericXmlApplicationContext();
	}

	private ConfigurableApplicationContext configureContext(ConfigurableApplicationContext applicationContext) {
		applicationContext.getBeanFactory().addBeanPostProcessor(new GemfireTestBeanPostProcessor());
		return applicationContext;
	}

	private ConfigurableApplicationContext initializeApplicationContext(ConfigurableApplicationContext applicationContext) {
		assertThat(applicationContext).isInstanceOf(GenericXmlApplicationContext.class);
		((GenericXmlApplicationContext) applicationContext).load(contextConfigLocation());
		applicationContext.registerShutdownHook();
		applicationContext.refresh();
		return applicationContext;
	}

	@Test(expected = XmlBeanDefinitionStoreException.class)
	public void testInvalidXmlSyntax() {
		try {
			initializeApplicationContext(configureContext(createApplicationContext()));
		}
		catch (XmlBeanDefinitionStoreException expected) {
			assertThat(expected).hasCauseInstanceOf(SAXParseException.class);
			throw expected;
		}
	}
}
