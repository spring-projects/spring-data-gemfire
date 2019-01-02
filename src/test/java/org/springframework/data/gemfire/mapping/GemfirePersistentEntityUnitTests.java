/*
 * Copyright 2012-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.mapping;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.mapping.IdentifierAccessor;
import org.springframework.data.mapping.MappingException;
import org.springframework.data.util.ClassTypeInformation;

/**
 * Unit tests for {@link GemfirePersistentEntity}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @author Gregory Green
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
 */
public class GemfirePersistentEntityUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private GemfireMappingContext mappingContext = new GemfireMappingContext();

	protected IdentifierAccessor getIdentifierAccessor(Object domainObject) {
		return getMappingContextPersistentEntity(domainObject).getIdentifierAccessor(domainObject);
	}

	@SuppressWarnings("unchecked")
	protected <T> GemfirePersistentEntity<T> getMappingContextPersistentEntity(Object domainObject) {
		return this.<T>getMappingContextPersistentEntity((Class<T>) domainObject.getClass());
	}

	@SuppressWarnings("unchecked")
	protected <T> GemfirePersistentEntity<T> getMappingContextPersistentEntity(Class<T> type) {
		return (GemfirePersistentEntity<T>) this.mappingContext.getPersistentEntity(type);
	}

	protected <T> GemfirePersistentEntity<T> newPersistentEntity(Class<T> type) {
		return new GemfirePersistentEntity<>(ClassTypeInformation.from(type));
	}

	@Test
	public void defaultsRegionNameForNonRegionAnnotatedEntityToClassName() {
		assertThat(newPersistentEntity(NonRegionAnnotatedEntity.class).getRegionName())
			.isEqualTo(NonRegionAnnotatedEntity.class.getSimpleName());
	}

	@Test
	public void defaultsRegionNameForUnnamedRegionAnnotatedEntityToClassName() {
		assertThat(newPersistentEntity(UnnamedRegionAnnotatedEntity.class).getRegionName())
			.isEqualTo(UnnamedRegionAnnotatedEntity.class.getSimpleName());
	}

	@Test
	public void returnsGivenNameForNamedRegionAnnotatedEntityAsRegionName() {
		assertThat(newPersistentEntity(NamedRegionAnnotatedEntity.class).getRegionName()).isEqualTo("Foo");
	}

	@Test
	@SuppressWarnings("unchecked")
	public void bigDecimalPersistentPropertyIsNotAnEntity() {
		GemfirePersistentEntity<ExampleDomainObject> entity =
			getMappingContextPersistentEntity(ExampleDomainObject.class);

		assertThat(entity).isNotNull();
		assertThat(entity.getRegionName()).isEqualTo("Example");

		GemfirePersistentProperty currency = entity.getPersistentProperty("currency");

		assertThat(currency).isNotNull();
		assertThat(currency.isEntity()).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void bigIntegerPersistentPropertyIsNotAnEntity() {

		GemfirePersistentEntity<ExampleDomainObject> entity =
			getMappingContextPersistentEntity(ExampleDomainObject.class);

		assertThat(entity).isNotNull();
		assertThat(entity.getRegionName()).isEqualTo("Example");

		GemfirePersistentProperty bigNumber = entity.getPersistentProperty("bigNumber");

		assertThat(bigNumber).isNotNull();
		assertThat(bigNumber.isEntity()).isFalse();
	}

	/**
	 * <a href="https://jira.spring.io/browse/SGF-582">SGF-582</a>
	 */
	@Test
	public void identifierForNonIdAnnotatedEntityWithNoIdFieldOrPropertyIsNull() {

		IdentifierAccessor identifierAccessor = getIdentifierAccessor(new NonRegionAnnotatedEntity());

		assertThat(identifierAccessor.getIdentifier()).isNull();
	}

	/**
	 * <a href="https://jira.spring.io/browse/SGF-582">SGF-582</a>
	 */
	@Test
	public void identifierForNonIdAnnotatedEntityWithIdFieldIsNotNull() {

		IdentifierAccessor identifierAccessor = getIdentifierAccessor(new NonIdAnnotatedIdFieldEntity());

		assertThat(identifierAccessor.getIdentifier()).isEqualTo(123L);
	}

	/**
	 * <a href="https://jira.spring.io/browse/SGF-582">SGF-582</a>
	 */
	@Test
	public void identifierForNonIdAnnotatedEntityWithIdPropertyIsNotNull() {

		IdentifierAccessor identifierAccessor = getIdentifierAccessor(new NonIdAnnotatedIdGetterEntity());

		assertThat(identifierAccessor.getIdentifier()).isEqualTo(456L);
	}

	@Test
	public void identifierForIdAnnotatedFieldAndPropertyEntityShouldNotConflict() {

		IdentifierAccessor identifierAccessor = getIdentifierAccessor(new IdAnnotatedFieldAndPropertyEntity());

		assertThat(identifierAccessor.getIdentifier()).isEqualTo(1L);
	}

	@Test
	public void identifierForAmbiguousIdAnnotatedFieldAndIdAnnotatedPropertyEntityThrowsMappingException() {

		AmbiguousIdAnnotatedFieldAndIdAnnotatedPropertyEntity entity =
			new AmbiguousIdAnnotatedFieldAndIdAnnotatedPropertyEntity();

		String expectedMessage = String.format("Attempt to add explicit id property [ssn] but already have id property [id] registered as explicit;"
			+ " Please check your object [%s] mapping configuration", entity.getClass().getName());

		exception.expect(MappingException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(expectedMessage);

		getIdentifierAccessor(new AmbiguousIdAnnotatedFieldAndIdAnnotatedPropertyEntity());
	}

	@SuppressWarnings("unused")
	static class AmbiguousIdAnnotatedFieldAndIdAnnotatedPropertyEntity {

		@Id
		private Long id = 1L;

		@Id
		public String getSsn() {
			return "123456789";
		}
	}

	static class IdAnnotatedFieldAndPropertyEntity {

		@Id
		private Long id = 1L;

		@Id
		public Long getId() {
			return this.id;
		}
	}

	static class NonIdAnnotatedIdFieldEntity {
		private Long id = 123L;
	}

	static class NonIdAnnotatedIdGetterEntity {
		public Long getId() {
			return 456L;
		}
	}

	static class NonRegionAnnotatedEntity {
	}

	@Region("Foo")
	static class NamedRegionAnnotatedEntity {
	}

	@Region
	static class UnnamedRegionAnnotatedEntity {
	}

	@Region("Example")
	@SuppressWarnings("unused")
	static class ExampleDomainObject {

		private BigDecimal currency;

		private BigInteger bigNumber;

		public BigDecimal getCurrency() {
			return currency;
		}

		public BigInteger getBigNumber() {
			return bigNumber;
		}
	}
}
