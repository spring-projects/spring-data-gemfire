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

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.isA;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Collections;

import org.apache.geode.pdx.PdxReader;
import org.apache.geode.pdx.PdxSerializer;
import org.apache.geode.pdx.PdxWriter;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.convert.support.GenericConversionService;
import org.springframework.data.convert.EntityInstantiator;
import org.springframework.data.gemfire.repository.sample.Address;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.mapping.model.MappingException;
import org.springframework.data.mapping.model.ParameterValueProvider;

/**
 * Unit tests for {@link MappingPdxSerializer}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.core.convert.ConversionService
 * @see org.springframework.data.gemfire.mapping.MappingPdxSerializer
 * @see org.apache.geode.pdx.PdxReader
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.apache.geode.pdx.PdxWriter
 */
@RunWith(MockitoJUnitRunner.class)
public class MappingPdxSerializerUnitTests {

	ConversionService conversionService;

	GemfireMappingContext context;

	MappingPdxSerializer serializer;

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Mock
	EntityInstantiator mockInstantiator;

	@Mock
	PdxReader mockReader;

	@Mock
	PdxSerializer mockAddressSerializer;

	@Mock
	PdxWriter mockWriter;

	@Before
	public void setUp() {
		context = new GemfireMappingContext();
		conversionService = new GenericConversionService();
		serializer = new MappingPdxSerializer(context, conversionService);
		serializer.setCustomSerializers(Collections.singletonMap(Address.class, mockAddressSerializer));
	}

	@Test
	public void createFullyInitialized() {
		ConversionService mockConversionService = mock(ConversionService.class);
		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(mockMappingContext, mockConversionService);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isEqualTo(mockConversionService);
		assertThat(pdxSerializer.getMappingContext()).isEqualTo(mockMappingContext);
	}

	@Test
	public void createWithNullConversionService() {
		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(mockMappingContext, null);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isInstanceOf(ConversionService.class);
		assertThat(pdxSerializer.getMappingContext()).isEqualTo(mockMappingContext);
	}

	@Test
	public void createWithNullMappingContext() {
		ConversionService mockConversionService = mock(ConversionService.class);

		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(null, mockConversionService);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isEqualTo(mockConversionService);
		assertThat(pdxSerializer.getMappingContext()).isInstanceOf(GemfireMappingContext.class);
	}

	@Test
	public void createWithNullConversionServiceAndNullMappingContext() {
		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(null, null);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isInstanceOf(ConversionService.class);
		assertThat(pdxSerializer.getMappingContext()).isInstanceOf(GemfireMappingContext.class);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesRegisteredInstantiator() {
		Address address = new Address();
		address.city = "London";
		address.zipCode = "01234";

		Person person = new Person(1L, "Oliver", "Gierke");
		person.address = address;

		when(mockInstantiator.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class)))
			.thenReturn(person);

		serializer.setGemfireInstantiators(Collections.singletonMap(Person.class, mockInstantiator));

		serializer.fromData(Person.class, mockReader);

		GemfirePersistentEntity<?> persistentEntity = context.getPersistentEntity(Person.class)
			.orElseThrow(() -> newIllegalStateException("Unable to resolve PersistentEntity for type [%s]",
				Person.class.getName()));

		verify(mockInstantiator, times(1)).createInstance(eq(persistentEntity),
			any(ParameterValueProvider.class));
		verify(mockAddressSerializer, times(1)).fromData(eq(Address.class), any(PdxReader.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void fromDataMapsPdxDataToApplicationDomainObject() {
		Address expectedAddress = new Address();

		expectedAddress.city = "Portland";
		expectedAddress.zipCode = "12345";

		when(mockInstantiator.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class)))
			.thenReturn(new Person(null, null, null));
		when(mockReader.readField(eq("id"))).thenReturn(1L);
		when(mockReader.readField(eq("firstname"))).thenReturn("Jon");
		when(mockReader.readField(eq("lastname"))).thenReturn("Doe");
		when(mockAddressSerializer.fromData(eq(Address.class), eq(mockReader))).thenReturn(expectedAddress);

		serializer.setGemfireInstantiators(Collections.singletonMap(Person.class, mockInstantiator));

		Object obj = serializer.fromData(Person.class, mockReader);

		assertThat(obj).isInstanceOf(Person.class);

		Person jonDoe = (Person) obj;

		assertThat(jonDoe.getAddress()).isEqualTo(expectedAddress);
		assertThat(jonDoe.getId()).isEqualTo(1L);
		assertThat(jonDoe.getFirstname()).isEqualTo("Jon");
		assertThat(jonDoe.getLastname()).isEqualTo("Doe");

		verify(mockInstantiator, times(1)).createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class));
		verify(mockReader, times(1)).readField(eq("id"));
		verify(mockReader, times(1)).readField(eq("firstname"));
		verify(mockReader, times(1)).readField(eq("lastname"));
		verify(mockAddressSerializer, times(1)).fromData(eq(Address.class), eq(mockReader));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void fromDataHandlesExceptionProperly() {
		when(mockInstantiator.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class)))
			.thenReturn(new Person(null, null, null));

		when(mockReader.readField(eq("id"))).thenThrow(newIllegalArgumentException("test"));

		serializer.setGemfireInstantiators(Collections.singletonMap(Person.class, mockInstantiator));

		try {
			expectedException.expect(MappingException.class);
			expectedException.expectCause(isA(IllegalArgumentException.class));
			expectedException.expectMessage(String.format(
				"While setting value [null] of property [id] for entity of type [%s] from PDX", Person.class));

			serializer.fromData(Person.class, mockReader);
		}
		finally {
			verify(mockInstantiator, times(1))
				.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class));
			verify(mockReader, times(1)).readField(eq("id"));
		}
	}

	@Test
	public void toDataSerializesApplicationDomainObjectToPdx() {
		Address address = new Address();
		address.city = "Portland";
		address.zipCode = "12345";

		Person jonDoe = new Person(1L, "Jon", "Doe");
		jonDoe.address = address;

		serializer.setCustomSerializers(Collections.singletonMap(Address.class, mockAddressSerializer));

		assertThat(serializer.toData(jonDoe, mockWriter)).isTrue();

		verify(mockAddressSerializer, times(1)).toData(eq(address), eq(mockWriter));
		verify(mockWriter, times(1)).writeField(eq("id"), eq(1L), eq(Long.class));
		verify(mockWriter, times(1)).writeField(eq("firstname"), eq("Jon"), eq(String.class));
		verify(mockWriter, times(1)).writeField(eq("lastname"), eq("Doe"), eq(String.class));
		verify(mockWriter, times(1)).markIdentityField(eq("id"));
	}

	@Test
	public void toDataHandlesExceptionProperly() {
		Address address = new Address();
		address.city = "Portland";
		address.zipCode = "12345";

		Person jonDoe = new Person(1L, "Jon", "Doe");
		jonDoe.address = address;

		when(mockWriter.writeField(eq("address"), eq(address), eq(Address.class)))
			.thenThrow(newIllegalArgumentException("test"));

		try {
			expectedException.expect(MappingException.class);
			expectedException.expectCause(isA(IllegalArgumentException.class));
			expectedException.expectMessage(String.format(
				"While serializing entity property [address] value [Portland, 12345] of type [%s] to PDX",
					Person.class));

			new MappingPdxSerializer(context, conversionService).toData(jonDoe, mockWriter);
		}
		finally {
			verify(mockWriter, atMost(1)).writeField(eq("id"), eq(1L), eq(Long.class));
			verify(mockWriter, atMost(1)).writeField(eq("firstname"), eq("Jon"), eq(String.class));
			verify(mockWriter, atMost(1)).writeField(eq("lastname"), eq("Doe"), eq(String.class));
			verify(mockWriter, times(1)).writeField(eq("address"), eq(address), eq(Address.class));
			verify(mockWriter, never()).markIdentityField(anyString());
		}
	}
}
