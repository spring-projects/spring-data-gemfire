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

package org.springframework.data.gemfire.repository.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * The PlantRepositoryTest class is a test suite of test cases testing the functionality behind PR #55 involving
 * persisting application domain object/entities to multiple Regions in GemFire's Cache.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @since 1.4.0
 * @link https://github.com/spring-projects/spring-data-gemfire/pull/55
 */
public class PlantRepositoryTest {

	private static final String APPLICATION_CONTEXT_CONFIG_LOCATION = String.format("%1$s%2$s%1$s%3$s",
		File.separator, PlantRepositoryTest.class.getPackage().getName().replace('.', File.separatorChar),
			"PlantRepositoryTest-context.xml");

	@Test(expected = IllegalArgumentException.class)
	public void storePlantHavingStringIdInPlantsRegionWithLongKey() {
		try {
			ConfigurableApplicationContext context =
				new ClassPathXmlApplicationContext(APPLICATION_CONTEXT_CONFIG_LOCATION);

			context.getBean(PlantRepository.class);
		}
		// NOTE technically, the IllegalArgumentException for incompatible Region 'Key' and Entity ID is thrown
		// when the Spring container starts up and the Repository beans are created.
		catch (BeanCreationException expected) {
			//expected.printStackTrace(System.err);
			assertTrue(expected.getCause() instanceof IllegalArgumentException);
			assertEquals(String.format("The Region referenced only supports keys of type [%1$s], but the entity to be stored has an id of type [%2$s]",
				Long.class.getName(), String.class.getName()), expected.getCause().getMessage());

			throw (IllegalArgumentException) expected.getCause();
		}
	}
}
