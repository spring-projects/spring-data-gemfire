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

package org.springframework.data.gemfire.mapping;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;
import org.springframework.data.gemfire.mapping.annotation.Region;

import lombok.Data;

/**
 * Unit tests for {@link GemfireMappingContext} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
 * @since 1.6.3
 */
public class GemfireMappingContextUnitTests {

	private GemfireMappingContext mappingContext = new GemfireMappingContext();

	@Test
	@SuppressWarnings("unchecked")
	public void getPersistentEntityForPerson() throws Exception {

		GemfirePersistentEntity<Person> personPersistentEntity =
			(GemfirePersistentEntity<Person>) mappingContext.getPersistentEntity(Person.class);

		assertThat(personPersistentEntity).isNotNull();
		assertThat(personPersistentEntity.getRegionName()).isEqualTo("People");

		GemfirePersistentProperty namePersistentProperty = personPersistentEntity.getPersistentProperty("name");

		assertThat(namePersistentProperty).isNotNull();
		assertThat(namePersistentProperty.isEntity()).isFalse();
		assertThat(namePersistentProperty.getName()).isEqualTo("name");
		assertThat(namePersistentProperty.getOwner()).isEqualTo(personPersistentEntity);
	}

	@Test
	public void getPersistentEntityForBigDecimal() {
		assertThat(mappingContext.getPersistentEntity(BigDecimal.class)).isNull();
	}

	@Test
	public void getPersistentEntityForBigInteger() {
		assertThat(mappingContext.getPersistentEntity(BigInteger.class)).isNull();
	}

	@Data
	@Region("People")
	class Person {
		private String name;
	}
}
