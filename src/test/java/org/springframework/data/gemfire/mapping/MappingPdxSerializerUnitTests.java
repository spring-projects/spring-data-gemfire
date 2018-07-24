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
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.security.Principal;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import com.gemstone.gemfire.TestGemStoneGemFireType;

import org.apache.geode.pdx.PdxReader;
import org.apache.geode.pdx.PdxSerializer;
import org.apache.geode.pdx.PdxWriter;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.core.Ordered;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.convert.support.DefaultConversionService;
import org.springframework.core.convert.support.GenericConversionService;
import org.springframework.core.type.ClassMetadata;
import org.springframework.data.convert.EntityInstantiator;
import org.springframework.data.convert.EntityInstantiators;
import org.springframework.data.domain.Page;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.sample.Account;
import org.springframework.data.gemfire.repository.sample.Address;
import org.springframework.data.gemfire.repository.sample.Algorithm;
import org.springframework.data.gemfire.repository.sample.Animal;
import org.springframework.data.gemfire.repository.sample.Customer;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.Plant;
import org.springframework.data.gemfire.repository.sample.Programmer;
import org.springframework.data.gemfire.repository.sample.RootUser;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.gemfire.test.model.Gender;
import org.springframework.data.gemfire.test.support.MapBuilder;
import org.springframework.data.mapping.MappingException;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.model.ParameterValueProvider;

/**
 * Unit tests for {@link MappingPdxSerializer}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.core.convert.ConversionService
 * @see org.springframework.data.gemfire.mapping.MappingPdxSerializer
 * @see org.apache.geode.pdx.PdxReader
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.apache.geode.pdx.PdxWriter
 */
@RunWith(MockitoJUnitRunner.class)
public class MappingPdxSerializerUnitTests {

	private ConversionService conversionService;

	@Mock
	private EntityInstantiator mockEntityInstantiator;

	private GemfireMappingContext mappingContext;

	private MappingPdxSerializer pdxSerializer;

	@Mock
	private PdxReader mockReader;

	@Mock
	private PdxWriter mockWriter;

	@Before
	public void setUp() {

		this.conversionService = new GenericConversionService();
		this.mappingContext = new GemfireMappingContext();
		this.pdxSerializer = spy(new MappingPdxSerializer(this.mappingContext, this.conversionService));
	}

	private String toFullyQualifiedPropertyName(PersistentProperty<?> property) {
		return MappingPdxSerializer.PdxSerializerResolvers.toFullyQualifiedPropertyName(property);
	}

	@Test
	public void constructDefaultMappingPdxSerializer() {

		MappingPdxSerializer pdxSerializer = new MappingPdxSerializer();

		assertThat(pdxSerializer.getConversionService()).isInstanceOf(DefaultConversionService.class);
		assertThat(pdxSerializer.getCustomPdxSerializers()).isEmpty();
		assertThat(pdxSerializer.getEntityInstantiators()).isInstanceOf(EntityInstantiators.class);
		assertThat(pdxSerializer.getMappingContext()).isInstanceOf(GemfireMappingContext.class);
	}

	@Test
	public void constructMappingPdxSerializerWithProvidedMappingContextAndConversionService() {

		ConversionService mockConversionService = mock(ConversionService.class);

		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		MappingPdxSerializer pdxSerializer = new MappingPdxSerializer(mockMappingContext, mockConversionService);

		assertThat(pdxSerializer.getConversionService()).isEqualTo(mockConversionService);
		assertThat(pdxSerializer.getCustomPdxSerializers()).isEmpty();
		assertThat(pdxSerializer.getEntityInstantiators()).isInstanceOf(EntityInstantiators.class);
		assertThat(pdxSerializer.getMappingContext()).isEqualTo(mockMappingContext);
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructMappingPdxSerializerWithNullConversionService() {

		try {
			new MappingPdxSerializer(this.mappingContext, null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("ConversionService must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructMappingPdxSerializerWithNullMappingContext() {

		try {
			new MappingPdxSerializer(null, this.conversionService);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("MappingContext must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void createMappingPdxSerializer() {

		ConversionService mockConversionService = mock(ConversionService.class);

		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(mockMappingContext, mockConversionService);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isEqualTo(mockConversionService);
		assertThat(pdxSerializer.getMappingContext()).isEqualTo(mockMappingContext);
	}

	@Test
	public void createMappingPdxSerializerWithNullConversionService() {

		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(mockMappingContext, null);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isInstanceOf(DefaultConversionService.class);
		assertThat(pdxSerializer.getMappingContext()).isEqualTo(mockMappingContext);
	}

	@Test
	public void createMappingPdxSerializerWithNullMappingContext() {

		ConversionService mockConversionService = mock(ConversionService.class);

		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(null, mockConversionService);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isEqualTo(mockConversionService);
		assertThat(pdxSerializer.getMappingContext()).isInstanceOf(GemfireMappingContext.class);
	}

	@Test
	public void createMappingPdxSerializerWithNullConversionServiceAndNullMappingContext() {

		MappingPdxSerializer pdxSerializer = MappingPdxSerializer.create(null, null);

		assertThat(pdxSerializer).isNotNull();
		assertThat(pdxSerializer.getConversionService()).isInstanceOf(DefaultConversionService.class);
		assertThat(pdxSerializer.getMappingContext()).isInstanceOf(GemfireMappingContext.class);
	}

	@Test
	public void setCustomPdxSerializersWithMappingOfClassTypesToPdxSerializers() {

		Map<Class<?>, PdxSerializer> customPdxSerializers =
			Collections.singletonMap(Person.class, mock(PdxSerializer.class));

		this.pdxSerializer.setCustomPdxSerializers(customPdxSerializers);

		assertThat(this.pdxSerializer.getCustomPdxSerializers()).isEqualTo(customPdxSerializers);
	}

	@Test
	public void setCustomPdxSerializersIsNullSafe() {

		assertThat(this.pdxSerializer.getCustomPdxSerializers()).isEmpty();

		this.pdxSerializer.setCustomPdxSerializers(null);

		assertThat(this.pdxSerializer.getCustomPdxSerializers()).isEmpty();
	}

	@Test
	public void setEntityInstantiatorsWithNonNullEntityInstantiators() {

		EntityInstantiators mockEntityInstantiators = mock(EntityInstantiators.class);

		this.pdxSerializer.setEntityInstantiators(mockEntityInstantiators);

		assertThat(this.pdxSerializer.getEntityInstantiators()).isSameAs(mockEntityInstantiators);
	}

	@Test(expected = IllegalArgumentException.class)
	public void setEntityInstantiatorsWithNullEntityInstantiators() {

		try {
			this.pdxSerializer.setEntityInstantiators((EntityInstantiators) null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("EntityInstantiators must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void setEntityInstantiatorsWithNonNullMappingOfClassTypesToEntityInstantiators() {

		Map<Class<?>, EntityInstantiator> entityInstantiators =
			Collections.singletonMap(Person.class, mock(EntityInstantiator.class));

		this.pdxSerializer.setEntityInstantiators(entityInstantiators);

		assertThat(this.pdxSerializer.getEntityInstantiators()).isInstanceOf(EntityInstantiators.class);
	}

	@Test(expected = IllegalArgumentException.class)
	public void setEntityInstantiatorsWithNullMap() {

		try {
			this.pdxSerializer.setEntityInstantiators((Map<Class<?>, EntityInstantiator>) null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("CustomInstantiators must not be null!");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void isReadableWithNonTransientPropertyReturnsTrue() {

		GemfirePersistentProperty mockPersistentProperty = mock(GemfirePersistentProperty.class);

		when(mockPersistentProperty.isTransient()).thenReturn(false);

		assertThat(this.pdxSerializer.isReadable(mockPersistentProperty)).isTrue();

		verify(mockPersistentProperty, times(1)).isTransient();
	}

	@Test
	public void isReadableWithTransientPropertyReturnsFalse() {

		GemfirePersistentProperty mockPersistentProperty = mock(GemfirePersistentProperty.class);

		when(mockPersistentProperty.isTransient()).thenReturn(true);

		assertThat(this.pdxSerializer.isReadable(mockPersistentProperty)).isFalse();

		verify(mockPersistentProperty, times(1)).isTransient();
	}

	@Test
	public void isWritableWithWritablePropertyReturnsTrue() {

		GemfirePersistentEntity<?> mockEntity = mock(GemfirePersistentEntity.class);

		GemfirePersistentProperty mockProperty = mock(GemfirePersistentProperty.class);

		when(mockEntity.isConstructorArgument(any(GemfirePersistentProperty.class))).thenReturn(false);
		when(mockProperty.isTransient()).thenReturn(false);
		when(mockProperty.isWritable()).thenReturn(true);

		assertThat(this.pdxSerializer.isWritable(mockEntity, mockProperty)).isTrue();

		verify(mockEntity, times(1)).isConstructorArgument(eq(mockProperty));
		verify(mockProperty, times(1)).isWritable();
		verify(mockProperty, times(1)).isTransient();
	}

	@Test
	public void isWritableWithConstructorArgumentPropertyReturnsFalse() {

		GemfirePersistentEntity<?> mockEntity = mock(GemfirePersistentEntity.class);

		GemfirePersistentProperty mockProperty = mock(GemfirePersistentProperty.class);

		when(mockEntity.isConstructorArgument(any(GemfirePersistentProperty.class))).thenReturn(true);

		assertThat(this.pdxSerializer.isWritable(mockEntity, mockProperty)).isFalse();

		verify(mockEntity, times(1)).isConstructorArgument(eq(mockProperty));
		verify(mockProperty, never()).isWritable();
		verify(mockProperty, never()).isTransient();
	}

	@Test
	public void isWritableWithNonWritablePropertyReturnsFalse() {

		GemfirePersistentEntity<?> mockEntity = mock(GemfirePersistentEntity.class);

		GemfirePersistentProperty mockProperty = mock(GemfirePersistentProperty.class);

		when(mockEntity.isConstructorArgument(any(GemfirePersistentProperty.class))).thenReturn(false);
		when(mockProperty.isWritable()).thenReturn(false);

		assertThat(this.pdxSerializer.isWritable(mockEntity, mockProperty)).isFalse();

		verify(mockEntity, times(1)).isConstructorArgument(eq(mockProperty));
		verify(mockProperty, times(1)).isWritable();
		verify(mockProperty, never()).isTransient();
	}

	@Test
	public void isWritableWithTransientPropertyReturnsFalse() {

		GemfirePersistentEntity<?> mockEntity = mock(GemfirePersistentEntity.class);

		GemfirePersistentProperty mockProperty = mock(GemfirePersistentProperty.class);

		when(mockEntity.isConstructorArgument(any(GemfirePersistentProperty.class))).thenReturn(false);
		when(mockProperty.isTransient()).thenReturn(true);
		when(mockProperty.isWritable()).thenReturn(true);

		assertThat(this.pdxSerializer.isWritable(mockEntity, mockProperty)).isFalse();

		verify(mockEntity, times(1)).isConstructorArgument(eq(mockProperty));
		verify(mockProperty, times(1)).isWritable();
		verify(mockProperty, times(1)).isTransient();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveCustomPdxSerializerReturnsNull() {

		PersistentEntity personEntity = this.mappingContext.getPersistentEntity(Person.class);

		PersistentProperty addressProperty = personEntity.getPersistentProperty("address");

		assertThat(this.pdxSerializer.getCustomPdxSerializers()).isEmpty();
		assertThat(this.pdxSerializer.resolveCustomPdxSerializer(addressProperty)).isNull();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveCustomPdxSerializerReturnsPdxSerializerForProperty() {

		PdxSerializer mockNamedSerializer = mock(PdxSerializer.class);
		PdxSerializer mockPropertySerializer = mock(PdxSerializer.class);
		PdxSerializer mockTypedSerializer = mock(PdxSerializer.class);

		PersistentEntity personEntity = this.mappingContext.getPersistentEntity(Person.class);

		PersistentProperty addressProperty = personEntity.getPersistentProperty("address");

		this.pdxSerializer.setCustomPdxSerializers(MapBuilder.<Object, PdxSerializer>newMapBuilder()
			.put(addressProperty, mockPropertySerializer)
			.put(toFullyQualifiedPropertyName(addressProperty), mockNamedSerializer)
			.put(Address.class, mockTypedSerializer)
			.build());

		assertThat(this.pdxSerializer.resolveCustomPdxSerializer(addressProperty)).isEqualTo(mockPropertySerializer);
	}

	@Test
	@SuppressWarnings("all")
	public void resolveCustomPdxSerializerReturnsPdxSerializerForPropertyName() {

		PdxSerializer mockNamedSerializer = mock(PdxSerializer.class);
		PdxSerializer mockTypedSerializer = mock(PdxSerializer.class);

		PersistentEntity personEntity = this.mappingContext.getPersistentEntity(Person.class);

		PersistentProperty addressProperty = personEntity.getPersistentProperty("address");

		this.pdxSerializer.setCustomPdxSerializers(MapBuilder.<Object, PdxSerializer>newMapBuilder()
			.put(toFullyQualifiedPropertyName(addressProperty), mockNamedSerializer)
			.put(Address.class, mockTypedSerializer)
			.build());

		assertThat(this.pdxSerializer.resolveCustomPdxSerializer(addressProperty)).isEqualTo(mockNamedSerializer);
	}

	@Test
	@SuppressWarnings("all")
	public void resolveCustomPdxSerializerReturnsPdxSerializerForPropertyType() {

		PdxSerializer mockNamedSerializer = mock(PdxSerializer.class);
		PdxSerializer mockTypedSerializer = mock(PdxSerializer.class);

		Map<Object, PdxSerializer> customPdxSerializers = new HashMap<>();

		PersistentEntity personEntity = this.mappingContext.getPersistentEntity(Person.class);

		PersistentProperty addressProperty = personEntity.getPersistentProperty("address");

		customPdxSerializers.put("example.Type.address", mockNamedSerializer);
		customPdxSerializers.put(Address.class, mockTypedSerializer);

		this.pdxSerializer.setCustomPdxSerializers(customPdxSerializers);

		assertThat(this.pdxSerializer.resolveCustomPdxSerializer(addressProperty)).isEqualTo(mockTypedSerializer);
	}

	@Test
	public void resolveEntityInstantiatorForManagedPersistentEntityWithEntityInstantiator() {

		EntityInstantiator mockEntityInstantiator = mock(EntityInstantiator.class);

		PersistentEntity mockEntity = mock(PersistentEntity.class);

		when(mockEntity.getType()).thenReturn(Person.class);

		this.pdxSerializer.setEntityInstantiators(Collections.singletonMap(Person.class, mockEntityInstantiator));

		assertThat(this.pdxSerializer.resolveEntityInstantiator(mockEntity)).isEqualTo(mockEntityInstantiator);

		verify(mockEntity, atLeast(1)).getType();
		verifyZeroInteractions(mockEntityInstantiator);
	}

	@Test
	public void resolveEntityInstantiatorForNonManagedPersistentEntityWithNoEntityInstantiator() {

		EntityInstantiator mockEntityInstantiator = mock(EntityInstantiator.class);

		PersistentEntity mockEntity = mock(PersistentEntity.class);

		when(mockEntity.getType()).thenReturn(Address.class);

		this.pdxSerializer.setEntityInstantiators(Collections.singletonMap(Person.class, mockEntityInstantiator));

		assertThat(this.pdxSerializer.resolveEntityInstantiator(mockEntity)).isNotEqualTo(mockEntityInstantiator);

		verify(mockEntity, atLeast(1)).getType();
		verifyZeroInteractions(mockEntityInstantiator);
	}

	@Test
	public void resolveTypeWithNonNullType() {
		assertThat(this.pdxSerializer.resolveType("test")).isEqualTo(String.class);
	}

	@Test
	public void resolveTypeWithNullType() {
		assertThat(this.pdxSerializer.resolveType(null)).isNull();
	}

	@Test
	public void toFullyQualifiedPropertyName() {

		PersistentEntity mockEntity = mock(PersistentEntity.class);
		PersistentProperty mockProperty = mock(PersistentProperty.class);

		when(mockProperty.getName()).thenReturn("mockProperty");
		when(mockProperty.getOwner()).thenReturn(mockEntity);
		when(mockEntity.getType()).thenReturn(Person.class);

		assertThat(MappingPdxSerializer.PdxSerializerResolvers.toFullyQualifiedPropertyName(mockProperty))
			.isEqualTo(Person.class.getName().concat(".mockProperty"));

		verify(mockEntity, times(1)).getType();
		verify(mockProperty, times(1)).getName();
		verify(mockProperty, times(1)).getOwner();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void fromDataDeserializesPdxBytesAndMapsToEntity() {

		Address expectedAddress = new Address();

		expectedAddress.street = "100 Main St.";
		expectedAddress.city = "Portland";
		expectedAddress.zipCode = "12345";

		PdxSerializer mockAddressSerializer = mock(PdxSerializer.class);

		when(this.mockEntityInstantiator.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class)))
			.thenReturn(new Person(null, null, null));
		when(this.mockReader.readField(eq("id"))).thenReturn(1L);
		when(this.mockReader.readField(eq("firstname"))).thenReturn("Jon");
		when(this.mockReader.readField(eq("lastname"))).thenReturn("Doe");
		when(mockAddressSerializer.fromData(eq(Address.class), eq(this.mockReader))).thenReturn(expectedAddress);

		this.pdxSerializer.setCustomPdxSerializers(Collections.singletonMap(Address.class, mockAddressSerializer));
		this.pdxSerializer.setEntityInstantiators(Collections.singletonMap(Person.class, this.mockEntityInstantiator));
		this.pdxSerializer.setIncludeTypeFilters(type -> Address.class.isAssignableFrom(type));
		this.pdxSerializer.setIncludeTypeFilters(type -> Person.class.isAssignableFrom(type));

		Object obj = this.pdxSerializer.fromData(Person.class, this.mockReader);

		assertThat(obj).isInstanceOf(Person.class);

		Person jonDoe = (Person) obj;

		assertThat(jonDoe.getAddress()).isEqualTo(expectedAddress);
		assertThat(jonDoe.getId()).isEqualTo(1L);
		assertThat(jonDoe.getFirstname()).isEqualTo("Jon");
		assertThat(jonDoe.getLastname()).isEqualTo("Doe");

		verify(this.mockEntityInstantiator, times(1))
			.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class));
		verify(this.mockReader, times(1)).readField(eq("id"));
		verify(this.mockReader, times(1)).readField(eq("firstname"));
		verify(this.mockReader, times(1)).readField(eq("lastname"));
		verify(mockAddressSerializer, times(1))
			.fromData(eq(Address.class), eq(this.mockReader));
	}

	@Test(expected = MappingException.class)
	@SuppressWarnings("unchecked")
	public void fromDataHandlesException() {

		when(this.mockEntityInstantiator.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class)))
			.thenReturn(new Person(null, null, null));

		when(this.mockReader.readField(eq("id"))).thenThrow(newIllegalArgumentException("test"));

		try {
			this.pdxSerializer.setEntityInstantiators(Collections.singletonMap(Person.class, this.mockEntityInstantiator));
			this.pdxSerializer.setIncludeTypeFilters(type -> Person.class.equals(type));
			this.pdxSerializer.fromData(Person.class, this.mockReader);
		}
		catch (MappingException expected) {

			assertThat(expected).hasMessage("While setting value [null] of property [id] for entity of type [%s] from PDX", Person.class);
			assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
			assertThat(expected.getCause()).hasMessage("test");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {
			verify(this.mockEntityInstantiator, times(1))
				.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class));

			verify(this.mockReader, times(1)).readField(eq("id"));
		}
	}

	@Test
	@SuppressWarnings("unchecked")
	public void fromDataUsesRegisteredEntityInstantiator() {

		Address address = new Address();

		address.street = "100 Main St.";
		address.city = "London";
		address.zipCode = "01234";

		PdxSerializer mockAddressSerializer = mock(PdxSerializer.class);

		Person person = new Person(1L, "Oliver", "Gierke");

		person.address = address;

		when(this.mockEntityInstantiator.createInstance(any(GemfirePersistentEntity.class), any(ParameterValueProvider.class)))
			.thenReturn(person);

		this.pdxSerializer.setCustomPdxSerializers(Collections.singletonMap(Address.class, mockAddressSerializer));
		this.pdxSerializer.setEntityInstantiators(Collections.singletonMap(Person.class, this.mockEntityInstantiator));
		this.pdxSerializer.setIncludeTypeFilters(type -> Person.class.equals(type));
		this.pdxSerializer.fromData(Person.class, this.mockReader);

		GemfirePersistentEntity<?> persistentEntity =
			Optional.ofNullable(this.mappingContext.getPersistentEntity(Person.class)).orElse(null);

		verify(this.mockEntityInstantiator, times(1))
			.createInstance(eq(persistentEntity), any(ParameterValueProvider.class));

		verify(mockAddressSerializer, times(1))
			.fromData(eq(Address.class), any(PdxReader.class));
	}

	@Test
	public void fromDataWithTypeFilterAcceptsDeclaredEntityTypes() {

		this.pdxSerializer.setIncludeTypeFilters(type -> User.class.getPackage().equals(type.getPackage()));

		doReturn("test").when(this.pdxSerializer).doFromData(any(Class.class), any(PdxReader.class));

		assertThat(this.pdxSerializer.fromData(Account.class, this.mockReader)).isEqualTo("test");
		assertThat(this.pdxSerializer.fromData(Algorithm.class, this.mockReader)).isEqualTo("test");
		assertThat(this.pdxSerializer.fromData(Animal.class, this.mockReader)).isEqualTo("test");
		assertThat(this.pdxSerializer.fromData(Customer.class, this.mockReader)).isEqualTo("test");
		assertThat(this.pdxSerializer.fromData(Plant.class, this.mockReader)).isEqualTo("test");
		assertThat(this.pdxSerializer.fromData(Programmer.class, this.mockReader)).isEqualTo("test");
		assertThat(this.pdxSerializer.fromData(User.class, this.mockReader)).isEqualTo("test");
		assertThat(this.pdxSerializer.fromData(org.springframework.data.gemfire.test.model.Person.class, this.mockReader)).isNull();
		assertThat(this.pdxSerializer.fromData(null, this.mockReader)).isNull();

		verify(this.pdxSerializer, times(1)).doFromData(eq(Account.class), eq(this.mockReader));
		verify(this.pdxSerializer, times(1)).doFromData(eq(Algorithm.class), eq(this.mockReader));
		verify(this.pdxSerializer, times(1)).doFromData(eq(Animal.class), eq(this.mockReader));
		verify(this.pdxSerializer, times(1)).doFromData(eq(Customer.class), eq(this.mockReader));
		verify(this.pdxSerializer, times(1)).doFromData(eq(Plant.class), eq(this.mockReader));
		verify(this.pdxSerializer, times(1)).doFromData(eq(Programmer.class), eq(this.mockReader));
		verify(this.pdxSerializer, times(1)).doFromData(eq(User.class), eq(this.mockReader));
		verify(this.pdxSerializer, never()).doFromData(isNull(), eq(this.mockReader));
		verify(this.pdxSerializer, never()).doFromData(eq(org.springframework.data.gemfire.test.model.Person.class),
			eq(this.mockReader));
	}

	@Test
	public void fromDataWithTypeFilterAcceptsIncludedTypesOverridingExcludedTypes() {

		this.pdxSerializer.setIncludeTypeFilters(type -> type.getPackage().getName().startsWith("java.security"));

		doReturn("test").when(this.pdxSerializer).doFromData(any(Class.class), any(PdxReader.class));

		assertThat(this.pdxSerializer.fromData(Principal.class, this.mockReader)).isEqualTo("test");

		verify(this.pdxSerializer, times(1))
			.doFromData(eq(Principal.class), eq(this.mockReader));
	}

	@Test
	public void fromDataWithTypeFilterFiltersApacheGeodeTypesReturnsNull() {
		assertThat(this.pdxSerializer.fromData(org.apache.geode.cache.EntryEvent.class, this.mockReader)).isNull();
	}

	@Test
	public void fromDataWithTypeFilterFiltersGemStoneTypesReturnsNull() {
		assertThat(this.pdxSerializer.fromData(TestGemStoneGemFireType.class, this.mockReader)).isNull();
	}

	@Test
	public void fromDataWithTypeFilterFiltersJavaTypesReturnsNull() {
		assertThat(this.pdxSerializer.fromData(java.security.Principal.class, this.mockReader)).isNull();
	}

	@Test
	public void fromDataWithTypeFilterFiltersNullTypesReturnsNull() {
		assertThat(this.pdxSerializer.fromData(null, this.mockReader)).isNull();
	}

	@Test
	public void fromDataWithTypeFilterFiltersSpringFrameworkTypesReturnsNull() {

		assertThat(this.pdxSerializer.fromData(ClassMetadata.class, this.mockReader)).isNull();
		assertThat(this.pdxSerializer.fromData(Page.class, this.mockReader)).isNull();
		assertThat(this.pdxSerializer.fromData(GemfireTemplate.class, this.mockReader)).isNull();
	}

	@Test
	public void fromDataWithTypeFilterFiltersUndeclaredEntityTypesReturnsNull() {
		assertThat(this.pdxSerializer.fromData(ApplicationDomainType.class, this.mockReader)).isNull();
	}

	@Test
	public void toDataSerializesEntityToPdxBytes() {

		Address address = new Address();

		address.street = "100 Main St.";
		address.city = "Portland";
		address.zipCode = "12345";

		PdxSerializer mockAddressSerializer = mock(PdxSerializer.class);

		Person jonDoe = new Person(1L, "Jon", "Doe");

		jonDoe.address = address;

		this.pdxSerializer.setIncludeTypeFilters(type -> Address.class.equals(type));
		this.pdxSerializer.setIncludeTypeFilters(type -> Person.class.equals(type));
		this.pdxSerializer.setCustomPdxSerializers(Collections.singletonMap(Address.class, mockAddressSerializer));

		assertThat(this.pdxSerializer.toData(jonDoe, this.mockWriter)).isTrue();

		verify(mockAddressSerializer, times(1)).toData(eq(address), eq(this.mockWriter));

		verify(this.mockWriter, times(1))
			.writeField(eq("id"), eq(1L), eq(Long.class));

		verify(this.mockWriter, times(1))
			.writeField(eq("firstname"), eq("Jon"), eq(String.class));

		verify(this.mockWriter, times(1))
			.writeField(eq("lastname"), eq("Doe"), eq(String.class));

		verify(this.mockWriter, times(1)).markIdentityField(eq("id"));
	}

	@Test(expected = MappingException.class)
	public void toDataHandlesException() {

		Address address = new Address();

		address.street = "100 Main St.";
		address.city = "Portland";
		address.zipCode = "12345";

		Person jonDoe = new Person(1L, "Jon", "Doe");

		jonDoe.address = address;

		when(this.mockWriter.writeField(eq("address"), eq(address), eq(Address.class)))
			.thenThrow(newIllegalArgumentException("test"));

		try {
			this.pdxSerializer.setIncludeTypeFilters(type -> Address.class.equals(type));
			this.pdxSerializer.setIncludeTypeFilters(type -> Person.class.equals(type));
			this.pdxSerializer.setCustomPdxSerializers(Collections.emptyMap());
			this.pdxSerializer.toData(jonDoe, this.mockWriter);
		}
		catch (MappingException expected) {

			assertThat(expected).hasMessage("While serializing entity [%1$s] property [address]"
					+ " value [100 Main St. Portland, 12345] of type [%2$s] to PDX",
				Person.class.getName(), Address.class.getName());

			assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
			assertThat(expected.getCause()).hasMessage("test");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {

			verify(this.mockWriter, atMost(1))
				.writeField(eq("id"), eq(1L), eq(Long.class));

			verify(this.mockWriter, atMost(1))
				.writeField(eq("firstname"), eq("Jon"), eq(String.class));

			verify(this.mockWriter, atMost(1))
				.writeField(eq("lastname"), eq("Doe"), eq(String.class));

			verify(this.mockWriter, times(1))
				.writeField(eq("address"), eq(address), eq(Address.class));

			verify(this.mockWriter, never()).markIdentityField(anyString());
		}
	}

	@Test
	public void toDataAcceptsDeclaredEntityTypeReturnsTrue() {

		org.springframework.data.gemfire.test.model.Person jonDoe =
			new org.springframework.data.gemfire.test.model.Person("Jon", "Doe",
				null, Gender.MALE);

		this.pdxSerializer.setIncludeTypeFilters(type -> User.class.getPackage().equals(type.getPackage()));

		doReturn(true).when(this.pdxSerializer).doToData(any(), any(PdxWriter.class));

		assertThat(this.pdxSerializer.toData(new Account(1L), this.mockWriter)).isTrue();
		assertThat(this.pdxSerializer.toData(new Animal(), this.mockWriter)).isTrue();
		assertThat(this.pdxSerializer.toData(new Customer(1L), this.mockWriter)).isTrue();
		assertThat(this.pdxSerializer.toData(new Programmer("jxblum"), this.mockWriter)).isTrue();
		assertThat(this.pdxSerializer.toData(new Plant(), this.mockWriter)).isTrue();
		assertThat(this.pdxSerializer.toData(new RootUser("jxblum"), this.mockWriter)).isTrue();
		assertThat(this.pdxSerializer.toData(null, this.mockWriter)).isFalse();
		assertThat(this.pdxSerializer.toData(jonDoe, this.mockWriter)).isFalse();

		verify(this.pdxSerializer, times(1)).doToData(isA(Account.class), eq(this.mockWriter));
		verify(this.pdxSerializer, times(1)).doToData(isA(Animal.class), eq(this.mockWriter));
		verify(this.pdxSerializer, times(1)).doToData(isA(Customer.class), eq(this.mockWriter));
		verify(this.pdxSerializer, times(1)).doToData(isA(Plant.class), eq(this.mockWriter));
		verify(this.pdxSerializer, times(1)).doToData(isA(Programmer.class), eq(this.mockWriter));
		verify(this.pdxSerializer, times(1)).doToData(isA(RootUser.class), eq(this.mockWriter));
		verify(this.pdxSerializer, never()).doToData(isNull(), eq(this.mockWriter));
		verify(this.pdxSerializer, never()).doToData(isA(jonDoe.getClass()), eq(this.mockWriter));
	}

	@Test
	public void toDataAcceptsIncludedEntityTypesOverridingExcludedEntityTypes() {

		Ordered testOrdered = () -> 1;

		this.pdxSerializer.setIncludeTypeFilters(type -> type.getPackage().getName().startsWith("org.springframework"));

		doReturn(true).when(this.pdxSerializer).doToData(any(), any(PdxWriter.class));

		assertThat(this.pdxSerializer.toData(testOrdered, this.mockWriter)).isTrue();

		verify(this.pdxSerializer, times(1))
			.doToData(isA(Ordered.class), eq(this.mockWriter));
	}

	@Test
	public void toDataFiltersApacheGeodeTypesReturnsFalse() {
		assertThat(this.pdxSerializer.toData(mock(org.apache.geode.cache.EntryEvent.class), this.mockWriter)).isFalse();
	}

	@Test
	public void toDataFiltersGemStoneTypesReturnsFalse() {
		assertThat(this.pdxSerializer.toData(TestGemStoneGemFireType.class, this.mockWriter)).isFalse();
	}

	@Test
	public void toDataFiltersJavaTypesReturnsFalse() {
		assertThat(this.pdxSerializer.toData(java.security.Principal.class, this.mockWriter)).isFalse();
	}

	@Test
	public void toDataFiltersNullTypesReturnsFalse() {
		assertThat(this.pdxSerializer.toData(null, this.mockWriter)).isFalse();
	}

	@Test
	public void toDataFiltersSpringFrameworkTypesReturnsFalse() {

		assertThat(this.pdxSerializer.toData(ClassMetadata.class, this. mockWriter)).isFalse();
		assertThat(this.pdxSerializer.toData(Page.class, this.mockWriter)).isFalse();
		assertThat(this.pdxSerializer.toData(GemfireTemplate.class, this.mockWriter)).isFalse();
	}

	@Test
	public void toDataFiltersUndeclaredEntityTypeReturnsFalse() {
		assertThat(this.pdxSerializer.toData(new ApplicationDomainType(), this.mockWriter)).isFalse();
	}

	private static class ApplicationDomainType { }

}
