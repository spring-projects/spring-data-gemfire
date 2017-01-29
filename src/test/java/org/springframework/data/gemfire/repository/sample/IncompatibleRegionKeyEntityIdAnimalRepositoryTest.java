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

package org.springframework.data.gemfire.repository.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * The IncompatibleRegionKeyEntityIdAnimalRepositoryTest class is a test suite of test cases testing the functionality
 * behind PR #55 involving persisting application domain object/entities to multiple Regions in GemFire's Cache.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @since 1.4.0
 * @link https://github.com/spring-projects/spring-data-gemfire/pull/55
 */
public class IncompatibleRegionKeyEntityIdAnimalRepositoryTest {

	private static final String APPLICATION_CONTEXT_CONFIG_LOCATION = String.format("%1$s%2$s%1$s%3$s",
		File.separator, AnimalRepositoryTest.class.getPackage().getName().replace('.', File.separatorChar),
			String.format("%s-context.xml", IncompatibleRegionKeyEntityIdAnimalRepositoryTest.class.getSimpleName()));

	@Test(expected = IllegalArgumentException.class)
	public void storeAnimalHavingLongIdInRabbitsRegionWithStringKey() {
		try {
			ConfigurableApplicationContext applicationContext =
				new ClassPathXmlApplicationContext(APPLICATION_CONTEXT_CONFIG_LOCATION);

			applicationContext.getBean(RabbitRepository.class);
		}
		// NOTE the ClassCastException thrown from GemFire is unexpected; this is not correct and the identifying type
		// mismatch should be caught and handled by GemfireRepositoryFactory.getTemplate(..) method on line 129
		// (appropriately throwing an IllegalArgumentException) after satisfying the condition on line 128,
		// which always occurs with the @Region annotation set on the domain class/entity!
		catch (ClassCastException unexpected) {
			//unexpected.printStackTrace(System.err);
			//assertTrue(unexpected.getMessage().contains("key ( java.lang.Long ) does not satisfy keyConstraint ( java.lang.String )"));
			fail(unexpected.getMessage());
		}
		catch (BeanCreationException expected) {
			//expected.printStackTrace(System.err);
			assertTrue(expected.getCause() instanceof IllegalArgumentException);
			assertEquals(String.format("The Region referenced only supports keys of type [%1$s], but the entity to be stored has an id of type [%2$s]",
				String.class.getName(), Long.class.getName()), expected.getCause().getMessage());

			throw (IllegalArgumentException) expected.getCause();
		}
	}
}
