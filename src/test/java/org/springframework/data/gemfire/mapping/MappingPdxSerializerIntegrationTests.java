/*
 * Copyright 2012-2018 the original author or authors.
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

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.time.LocalDateTime;

import org.apache.geode.DataSerializable;
import org.apache.geode.Instantiator;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.core.convert.support.DefaultConversionService;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.annotation.Transient;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.repository.sample.Address;
import org.springframework.data.gemfire.repository.sample.Person;

import lombok.Getter;
import lombok.Setter;

/**
 * Integration tests for {@link MappingPdxSerializer}.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
public class MappingPdxSerializerIntegrationTests {

	static Cache cache;

	static Region<Object, Object> region;

	@BeforeClass
	public static void setUp() {

		MappingPdxSerializer serializer =
			new MappingPdxSerializer(new GemfireMappingContext(), new DefaultConversionService());

		cache = new CacheFactory()
			.set("name", MappingPdxSerializerIntegrationTests.class.getSimpleName())
			.set("log-level", "error")
			.setPdxSerializer(serializer)
			.setPdxPersistent(true)
			.create();

		region = cache.createRegionFactory()
			.setDataPolicy(DataPolicy.PARTITION)
			.create("TemporaryRegion");
	}

	@AfterClass
	@SuppressWarnings("all")
	public static void tearDown() {
		GemfireUtils.close(cache);
	}

	@Test
	public void handlesEntityWithReadOnlyProperty() {

		EntityWithReadOnlyProperty entity = new EntityWithReadOnlyProperty();

		entity.setName("ReadOnlyEntity");
		entity.setTimestamp(LocalDateTime.now());
		entity.processId = 123;

		region.put(100L, entity);

		Object target = region.get(100L);

		assertThat(target).isInstanceOf(EntityWithReadOnlyProperty.class);
		assertThat(target).isNotSameAs(entity);

		EntityWithReadOnlyProperty deserializedEntity = (EntityWithReadOnlyProperty) target;

		assertThat(deserializedEntity.getName()).isEqualTo(entity.getName());
		assertThat(deserializedEntity.getTimestamp()).isEqualTo(entity.getTimestamp());
		assertThat(deserializedEntity.getProcessId()).isNull();
	}

	@Test
	public void handlesEntityWithTransientProperty() {

		EntityWithTransientProperty entity = new EntityWithTransientProperty();

		entity.setName("TransientEntity");
		entity.setValueOne("testOne");
		entity.setValueTwo("testTwo");

		region.put(101L, entity);

		Object target = region.get(101L);

		assertThat(target).isInstanceOf(EntityWithTransientProperty.class);
		assertThat(target).isNotSameAs(entity);

		EntityWithTransientProperty deserializedEntity = (EntityWithTransientProperty) target;

		assertThat(deserializedEntity.getName()).isEqualTo(entity.getName());
		assertThat(deserializedEntity.getValueOne()).isNull();
		assertThat(deserializedEntity.getValueTwo()).isNull();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void serializesAndDeserializesEntityCorrectly() {

		Address address = new Address();

		address.street = "100 Main St.";
		address.city = "London";
		address.zipCode = "01234";

		Person person = new Person(1L, "Oliver", "Gierke");

		person.address = address;
		region.put(1L, person);

		Object result = region.get(1L);

		assertThat(result).isInstanceOf(Person.class);
		assertThat(result).isNotSameAs(person);

		Person reference = (Person) result;

		assertThat(reference.getFirstname()).isEqualTo(person.getFirstname());
		assertThat(reference.getLastname()).isEqualTo(person.getLastname());
		assertThat(reference.getAddress()).isEqualTo(person.getAddress());
	}

	@Test
	public void serializesAndDeserializesEntityWithDataSerializableProperty() {

		Address address = new Address();

		address.street = "100 Main St.";
		address.city = "London";
		address.zipCode = "01234";

		PersonWithDataSerializableProperty person =
			new PersonWithDataSerializableProperty(2L, "Oliver", "Gierke",
				new DataSerializableProperty("foo"));

		person.address = address;

		region.put(2L, person);

		Object result = region.get(2L);

		assertThat(result).isInstanceOf(PersonWithDataSerializableProperty.class);
		assertThat(result).isNotSameAs(person);

		PersonWithDataSerializableProperty reference = (PersonWithDataSerializableProperty) result;

		assertThat(reference.getFirstname()).isEqualTo(person.getFirstname());
		assertThat(reference.getLastname()).isEqualTo(person.getLastname());
		assertThat(reference.getAddress()).isEqualTo(person.getAddress());
		assertThat(reference.property.getValue()).isEqualTo("foo");
	}

	@SuppressWarnings({ "serial", "unused" })
	public static class PersonWithDataSerializableProperty extends Person {

		private DataSerializableProperty property;

		public PersonWithDataSerializableProperty(Long id, String firstname,
				String lastname, DataSerializableProperty property) {

			super(id, firstname, lastname);

			this.property = property;
		}

		public DataSerializableProperty getDataSerializableProperty() {
			return this.property;
		}

		public void setDataSerializableProperty(DataSerializableProperty property) {
			this.property = property;
		}
	}

	@SuppressWarnings("serial")
	public static class DataSerializableProperty implements DataSerializable {

		static {
			Instantiator.register(new Instantiator(DataSerializableProperty.class,101) {
				public DataSerializable newInstance() {
					return new DataSerializableProperty("");
				}
			});
		}

		private String value;

		public DataSerializableProperty(String value) {
			this.value = value;
		}


		@Override
		public void fromData(DataInput dataInput) throws IOException, ClassNotFoundException {
			this.value = dataInput.readUTF();

		}

		@Override
		public void toData(DataOutput dataOutput) throws IOException {
			dataOutput.writeUTF(this.value);
		}

		public String getValue() {
			return this.value;
		}
	}

	@Getter
	static class EntityWithReadOnlyProperty {

		@Setter
		LocalDateTime timestamp;

		@Setter
		String name;

		// TODO: if there is not setter, then effectively this field/property is read-only
		// and should not require the @ReadOnlyProperty
		@ReadOnlyProperty
		Object processId;
	}

	@Getter @Setter
	static class EntityWithTransientProperty {

		private String name;

		private transient Object valueOne;

		@Transient
		private Object valueTwo;
	}
}
