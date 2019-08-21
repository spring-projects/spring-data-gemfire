/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

import org.springframework.beans.factory.parsing.BeanDefinitionParsingException;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * The TemplateRegionDefinitionOrderErrorNamespaceTest class is a test suite of test cases testing the contract
 * and functionality of Region Templates, and specifically the correct order definition of Region Templates before
 * the Region bean definitions and concrete types that use those templates.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.5.0
 */
public class TemplateRegionDefinitionOrderErrorNamespaceTest {

	private String getConfigLocation() {
		return getClass().getName().replace(".", "/").concat("-context.xml");
	}

	@Test(expected = BeanDefinitionParsingException.class)
	public void testIncorrectTemplateRegionDefinitionOrder() throws Exception {

		try {
			new ClassPathXmlApplicationContext(getConfigLocation());
		}
		catch (BeanDefinitionParsingException expected) {

			assertThat(expected)
				.hasMessageContaining("The Region template [RegionTemplate] must be defined before the Region [TemplateBasedPartitionRegion] referring to the template");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}
}
