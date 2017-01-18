/*
 * Copyright 2010-2013 the original author or authors.
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.xml.XmlBeanDefinitionStoreException;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.xml.sax.SAXParseException;

/**
 * The SubRegionWithInvalidDataPolicyTest class is a test suite of test cases testing the data-policy and persistent
 * attributes settings are consistent for GemFire SubRegion bean definitions.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since GemFire 7.0.1
 * @since Spring Data GemFire 1.4.0
 */
public class SubRegionWithInvalidDataPolicyTest {

	@Test(expected = XmlBeanDefinitionStoreException.class)
	public void testSubRegionBeanDefinitionWithInconsistentDataPolicy() {
		try {
			new ClassPathXmlApplicationContext(
				"/org/springframework/data/gemfire/config/xml/subregion-with-invalid-datapolicy.xml");
		}
		catch (XmlBeanDefinitionStoreException expected) {
			assertTrue(expected.getCause() instanceof SAXParseException);
			assertTrue(expected.getCause().getMessage().contains("PERSISTENT_PARTITION"));

			throw expected;
		}
	}

	@Test(expected = BeanCreationException.class)
	public void testSubRegionBeanDefinitionWithInvalidDataPolicyPersistentSettings() {
		try {
			new ClassPathXmlApplicationContext(
				"/org/springframework/data/gemfire/config/xml/subregion-with-inconsistent-datapolicy-persistent-settings.xml");
		}
		catch (BeanCreationException expected) {
			assertTrue(expected.getMessage().contains("Error creating bean with name '/Parent/Child'"));
			assertTrue(expected.getCause() instanceof IllegalArgumentException);
			assertEquals("Data Policy [REPLICATE] is invalid when persistent is true.",
				expected.getCause().getMessage());

			throw expected;
		}
	}
}
