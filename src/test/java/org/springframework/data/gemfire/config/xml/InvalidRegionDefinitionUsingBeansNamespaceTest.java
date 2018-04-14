/*
 * Copyright 2010-2018 the original author or authors.
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
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.RegionFactoryBean;

/**
 * The InvalidRegionDefinitionUsingBeansNamespaceTest class is a test suite of test cases testing the definition of
 * invalid Region beans in a Spring context.  Specifically, this test class tests the specification of the Region's
 * DataPolicy on a nested RegionAttributesFactoryBean conflicting with the 'persistent' attribute configuration
 * on the containing RegionFactoryBean definition.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @since 1.6.0
 */
public class InvalidRegionDefinitionUsingBeansNamespaceTest {

	private static final String CONFIG_LOCATION =
		"org/springframework/data/gemfire/config/xml/InvalidDataPolicyPersistentAttributeSettingsBeansNamespaceTest.xml";

	@Test(expected = IllegalArgumentException.class)
	public void testInvalidDataPolicyPersistentAttributeSettings() {

		try {
			new ClassPathXmlApplicationContext(CONFIG_LOCATION);
		}
		catch (BeanCreationException expected) {

			assertTrue(expected.getCause() instanceof IllegalArgumentException);
			assertEquals("Data Policy [REPLICATE] is not valid when persistent is true",
				expected.getCause().getMessage());

			throw (IllegalArgumentException) expected.getCause();
		}
	}

	@SuppressWarnings("unused")
	public static final class TestRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> { }

}
