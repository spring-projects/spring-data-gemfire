/*
 * Copyright 2012 the original author or authors.
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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.util.ClassTypeInformation;

/**
 * Unit tests for {@link GemfirePersistentEntity}.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
public class GemfirePersistentEntityUnitTests {

	protected MappingContext<GemfirePersistentEntity<?>, GemfirePersistentProperty> getMappingContext() {
		return new GemfireMappingContext();
	}

	@Test
	public void defaultsRegionNameToClassName() {
		GemfirePersistentEntity<UnannotatedRegion> entity = new GemfirePersistentEntity<UnannotatedRegion>(
				ClassTypeInformation.from(UnannotatedRegion.class));
		assertThat(entity.getRegionName(), is(UnannotatedRegion.class.getSimpleName()));
	}

	@Test
	public void defaultsAnnotatedRegionToCLassName() {
		GemfirePersistentEntity<UnnamedRegion> entity = new GemfirePersistentEntity<UnnamedRegion>(
				ClassTypeInformation.from(UnnamedRegion.class));
		assertThat(entity.getRegionName(), is(UnnamedRegion.class.getSimpleName()));
	}

	@Test
	public void readsRegionNameFromAnnotation() {

		GemfirePersistentEntity<AnnotatedRegion> entity = new GemfirePersistentEntity<AnnotatedRegion>(
				ClassTypeInformation.from(AnnotatedRegion.class));
		assertThat(entity.getRegionName(), is("Foo"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void bigDecimalPersistentPropertyIsNotEntity() {
		GemfirePersistentEntity<ExampleDomainObject> entity = (GemfirePersistentEntity<ExampleDomainObject>)
			getMappingContext().getPersistentEntity(ExampleDomainObject.class);

		assertThat(entity.getRegionName(), is(equalTo("Example")));

		GemfirePersistentProperty currency = entity.getPersistentProperty("currency");

		assertThat(currency, is(notNullValue()));
		assertThat(currency.isEntity(), is(false));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void bigIntegerPersistentPropertyIsNotEntity() {
		GemfirePersistentEntity<ExampleDomainObject> entity = (GemfirePersistentEntity<ExampleDomainObject>)
			getMappingContext().getPersistentEntity(ExampleDomainObject.class);

		assertThat(entity.getRegionName(), is(equalTo("Example")));

		GemfirePersistentProperty bigNumber = entity.getPersistentProperty("bigNumber");

		assertThat(bigNumber, is(notNullValue()));
		assertThat(bigNumber.isEntity(), is(false));
	}

	static class UnannotatedRegion {
	}

	@Region("Foo")
	static class AnnotatedRegion {
	}

	@Region
	static class UnnamedRegion {
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
