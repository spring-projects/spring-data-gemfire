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

import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.convert.support.GenericConversionService;
import org.springframework.data.convert.EntityInstantiator;
import org.springframework.data.gemfire.repository.sample.Address;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.mapping.model.ParameterValueProvider;

import com.gemstone.gemfire.pdx.PdxReader;
import com.gemstone.gemfire.pdx.PdxSerializer;

/**
 * Unit tests for {@link MappingPdxSerializer}.
 * 
 * @author Oliver Gierke
 */
@RunWith(MockitoJUnitRunner.class)
public class MappingPdxSerializerUnitTests {

	GemfireMappingContext context;
	ConversionService conversionService;
	MappingPdxSerializer serializer;

	@Mock
	EntityInstantiator instantiator;
	@Mock
	PdxReader reader;
	
	@Mock
	PdxSerializer addressSerializer;

	@Before
	public void setUp() {

		context = new GemfireMappingContext();
		conversionService = new GenericConversionService();
		serializer = new MappingPdxSerializer(context, conversionService);
		Map<Class<?>,PdxSerializer> customSerializers = new HashMap<Class<?>, PdxSerializer>();
		customSerializers.put(Address.class, addressSerializer);
		serializer.setCustomSerializers(customSerializers);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void usesRegisteredInstantiator() {
		Address address = new Address();
		address.zipCode = "01234";
		address.city = "London";

		Person person = new Person(1L, "Oliver", "Gierke");
		person.address = address;
		
		ParameterValueProvider<GemfirePersistentProperty> provider = any(ParameterValueProvider.class);
		GemfirePersistentEntity<?> entity = any(GemfirePersistentEntity.class);
		when(instantiator.createInstance(entity, provider)).thenReturn(person);

		Map<Class<?>, EntityInstantiator> instantiators = new HashMap<Class<?>, EntityInstantiator>();
		instantiators.put(Person.class, instantiator);

		serializer.setGemfireInstantiators(instantiators);
		serializer.fromData(Person.class, reader);

		verify(instantiator, times(1)).createInstance(eq(context.getPersistentEntity(Person.class)),
				any(ParameterValueProvider.class));
		verify(addressSerializer,times(1)).fromData(eq(Address.class), any(PdxReader.class));
	}
}
