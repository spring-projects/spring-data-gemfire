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
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.Collections;

import com.gemstone.gemfire.DataSerializable;
import com.gemstone.gemfire.Instantiator;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.pdx.PdxReader;
import com.gemstone.gemfire.pdx.PdxSerializer;
import com.gemstone.gemfire.pdx.PdxWriter;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.annotation.Transient;
import org.springframework.data.gemfire.repository.sample.Address;
import org.springframework.data.gemfire.repository.sample.Person;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

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

		MappingPdxSerializer serializer = MappingPdxSerializer.newMappingPdxSerializer();

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

		if (cache != null) {
			cache.close();
		}
	}

	@After
	public void clearRegion() {
		region.removeAll(region.keySet());
	}

	@Test
	public void handlesEntityWithReadOnlyProperty() {

		EntityWithReadOnlyProperty entity = new EntityWithReadOnlyProperty();

		entity.setName("ReadOnlyEntity");
		entity.setTimestamp(System.currentTimeMillis());
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
		entity.setValue("test");

		region.put(101L, entity);

		Object target = region.get(101L);

		assertThat(target).isInstanceOf(EntityWithTransientProperty.class);
		assertThat(target).isNotSameAs(entity);

		EntityWithTransientProperty deserializedEntity = (EntityWithTransientProperty) target;

		assertThat(deserializedEntity.getName()).isEqualTo(entity.getName());
		assertThat(deserializedEntity.getValue()).isNull();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void serializesAndDeserializesEntity() {

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

	@Test
	public void serializationUsesCustomPropertyNameBasedPdxSerializer() throws IOException {

		PdxSerializer mockPasswordSerializer = mock(PdxSerializer.class);

		when(mockPasswordSerializer.toData(any(), any(PdxWriter.class))).thenAnswer(new Answer<Boolean>() {

			@Override
			public Boolean answer(InvocationOnMock invocation) throws Throwable {

				String password = invocation.getArgumentAt(0, String.class);

				PdxWriter pdxWriter = invocation.getArgumentAt(1, PdxWriter.class);

				pdxWriter.writeString("password", new BASE64Encoder().encode(password.getBytes()));

				return true;
			}
		});

		when(mockPasswordSerializer.fromData(any(Class.class), any(PdxReader.class))).thenAnswer(new Answer<String>() {

			@Override
			public String answer(InvocationOnMock invocation) throws Throwable {

				PdxReader pdxReader = invocation.getArgumentAt(1, PdxReader.class);

				return pdxReader.readString("password");
			}
		});

		User jonDoe = User.newUser("jdoe", "p@55w0rd!");

		assertThat(jonDoe).isNotNull();
		assertThat(jonDoe.getName()).isEqualTo("jdoe");
		assertThat(jonDoe.getPassword()).isEqualTo("p@55w0rd!");

		String passwordPropertyName = User.class.getName().concat(".password");

		((MappingPdxSerializer) ((Cache) region.getRegionService()).getPdxSerializer())
			.setCustomPdxSerializers(Collections.singletonMap(passwordPropertyName, mockPasswordSerializer));

		region.put(4L, jonDoe);

		Object result = region.get(4L);

		assertThat(result).isInstanceOf(User.class);
		assertThat(result).isNotSameAs(jonDoe);

		User jonDoeLoaded = (User) result;

		assertThat(jonDoeLoaded.getName()).isEqualTo(jonDoe.getName());
		assertThat(jonDoeLoaded.getPassword()).describedAs("Password was [%s]", jonDoeLoaded.getPassword())
			.isNotEqualTo(jonDoe.getPassword());
		assertThat(new String(new BASE64Decoder().decodeBuffer(jonDoeLoaded.getPassword()))).isEqualTo(jonDoe.getPassword());

		verify(mockPasswordSerializer, atLeastOnce()).toData(eq("p@55w0rd!"), isA(PdxWriter.class));

		verify(mockPasswordSerializer, times(1))
			.fromData(eq(String.class), isA(PdxReader.class));
	}

	@Test
	public void serializationUsesCustomPropertyTypeBasedPdxSerializer() {

		PdxSerializer mockCreditCardSerializer = mock(PdxSerializer.class);

		when(mockCreditCardSerializer.toData(any(), any(PdxWriter.class))).thenAnswer(new Answer<Boolean>() {

			@Override
			public Boolean answer(InvocationOnMock invocation) throws Throwable {

				CreditCard creditCard = invocation.getArgumentAt(0, CreditCard.class);

				PdxWriter pdxWriter = invocation.getArgumentAt(1, PdxWriter.class);

				pdxWriter.writeLong("creditCard.expirationDate", creditCard.getExpirationDate());
				pdxWriter.writeString("creditCard.number",
					new BASE64Encoder().encode(creditCard.getNumber().getBytes()));
				pdxWriter.writeString("creditCard.type", creditCard.getType().name());

				return true;
			}
		});

		when(mockCreditCardSerializer.fromData(any(Class.class), any(PdxReader.class))).thenAnswer(new Answer<CreditCard>() {

			@Override
			public CreditCard answer(InvocationOnMock invocation) throws Throwable {

				PdxReader pdxReader = invocation.getArgumentAt(1, PdxReader.class);

				Long creditCardExpirationDate = pdxReader.readLong("creditCard.expirationDate");

				String creditCardNumber =
					new String(new BASE64Decoder().decodeBuffer(pdxReader.readString("creditCard.number")));

				creditCardNumber = "xxxx-".concat(creditCardNumber.substring(creditCardNumber.length() - 4));

				CreditCard.Type creditCardType = CreditCard.Type.valueOf(pdxReader.readString("creditCard.type"));

				return CreditCard.of(creditCardExpirationDate, creditCardNumber, creditCardType);
			}
		});

		((MappingPdxSerializer) ((Cache) region.getRegionService()).getPdxSerializer())
			.setCustomPdxSerializers(Collections.singletonMap(CreditCard.class, mockCreditCardSerializer));

		CreditCard creditCard = CreditCard.of(System.currentTimeMillis(),
			"8842-6789-4186-7981", CreditCard.Type.VISA);

		Customer jonDoe = Customer.newCustomer(creditCard, "Jon Doe");

		region.put(8L, jonDoe);

		Object result = region.get(8L);

		assertThat(result).isInstanceOf(Customer.class);
		assertThat(result).isNotSameAs(jonDoe);

		Customer jonDoeLoaded = (Customer) result;

		assertThat(jonDoeLoaded.getName()).isEqualTo(jonDoe.getName());
		assertThat(jonDoeLoaded.getCreditCard()).isNotEqualTo(jonDoe.getCreditCard());
		assertThat(jonDoeLoaded.getCreditCard().getExpirationDate())
			.isEqualTo(jonDoe.getCreditCard().getExpirationDate());
		assertThat(jonDoeLoaded.getCreditCard().getNumber()).isEqualTo("xxxx-7981");
		assertThat(jonDoeLoaded.getCreditCard().getType()).isEqualTo(jonDoe.getCreditCard().getType());

		verify(mockCreditCardSerializer, atLeastOnce()).toData(eq(creditCard), isA(PdxWriter.class));

		verify(mockCreditCardSerializer, times(1))
			.fromData(eq(CreditCard.class), isA(PdxReader.class));
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
		Long timestamp;

		@Setter
		String name;

		// TODO: if there is no setter, then effectively this field/property is read-only
		// and should not require the @ReadOnlyProperty
		@ReadOnlyProperty
		Object processId;

	}

	@Getter @Setter
	static class EntityWithTransientProperty {

		private String name;

		@Transient
		private Object value;

	}

	@Data
	@AllArgsConstructor(staticName = "newUser")
	static class User {

		String name;
		String password;

		@SuppressWarnings("unused")
		User() {}

	}

	@Data
	@AllArgsConstructor(staticName = "newCustomer")
	static class Customer {

		CreditCard creditCard;
		String name;

		@SuppressWarnings("unused")
		Customer() {}

	}

	@Data
	@RequiredArgsConstructor(staticName = "of")
	static class CreditCard {

		@NonNull Long expirationDate;
		@NonNull String number;
		@NonNull Type type;

		enum Type {

			AMERICAN_EXPRESS,
			MASTER_CARD,
			VISA,

		}
	}
}
