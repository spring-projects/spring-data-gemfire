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

package org.springframework.data.gemfire.mapping;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.mapping.model.GemfireSimpleTypeHolder;
import org.springframework.data.mapping.PersistentEntity;

/**
 * The GemfireMappingContextTest class is a test suite of test cases testing the contract and functionality
 * of the GemfireMappingContext class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
 * @since 1.6.3
 */
public class GemfireMappingContextTest {

	private GemfireMappingContext mappingContext = new GemfireMappingContext();

	@Test
	@SuppressWarnings("unchecked")
	public void getPersistentEntityForPerson() throws Exception {
		GemfirePersistentEntity<Person> personPersistentEntity = (GemfirePersistentEntity<Person>)
			mappingContext.getPersistentEntity(Person.class);

		assertThat(personPersistentEntity, is(notNullValue()));
		assertThat(personPersistentEntity.getRegionName(), is(equalTo("People")));

		GemfirePersistentProperty namePersistentProperty = personPersistentEntity.getPersistentProperty("name");

		assertThat(namePersistentProperty, is(notNullValue()));
		assertThat(namePersistentProperty.isEntity(), is(false));
		assertThat(namePersistentProperty.getName(), is(equalTo("name")));
		assertThat(namePersistentProperty.getOwner(), is(equalTo((PersistentEntity) personPersistentEntity)));
		assertThat(TestUtils.readField("simpleTypeHolder", namePersistentProperty), is(instanceOf(
			GemfireSimpleTypeHolder.class)));
	}

	@Test
	public void getPersistentEntityForBigDecimal() {
		assertThat(mappingContext.getPersistentEntity(BigDecimal.class), is(nullValue()));
	}

	@Test
	public void getPersistentEntityForBigInteger() {
		assertThat(mappingContext.getPersistentEntity(BigInteger.class), is(nullValue()));
	}

	@Region("People")
	class Person {

		private String name;

		public String getName() {
			return name;
		}

		public void setName(final String name) {
			this.name = name;
		}
	}

}
