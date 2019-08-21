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

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import org.springframework.beans.factory.parsing.BeanDefinitionParsingException;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * The ClientRegionUsingDataPolicyAndShortcutTest class is a test suite of test case testing a client Region
 * bean definition with both data-policy and shortcut specified.
 *
 * @author John Blum
 * @see org.junit.Test
 * @since 1.3.3
 */
public class ClientRegionUsingDataPolicyAndShortcutTest {

	@Test(expected = BeanDefinitionParsingException.class)
	public void testClientRegionBeanDefinitionWithDataPolicyAndShortcut() {
		try {
			new ClassPathXmlApplicationContext(
				"/org/springframework/data/gemfire/config/xml/client-region-using-datapolicy-and-shortcut.xml");
		}
		catch (BeanDefinitionParsingException e) {
			assertTrue(e.getMessage().contains("Only one of [data-policy, shortcut] may be specified with element"));
			throw e;
		}
	}

}
