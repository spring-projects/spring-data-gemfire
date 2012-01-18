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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.springframework.core.convert.support.DefaultConversionService;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.MappingPdxSerializer;
import org.springframework.data.gemfire.repository.sample.Address;
import org.springframework.data.gemfire.repository.sample.Person;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * Integration tests for {@link MappingPdxSerializer}.
 * 
 * @author Oliver Gierke
 */
public class MappingPdxSerializerIntegrationTest {

	Region<Object, Object> region;

	@Before
	public void setUp() {

		MappingPdxSerializer serializer = new MappingPdxSerializer(new GemfireMappingContext(),
				new DefaultConversionService());

		CacheFactory factory = new CacheFactory();
		factory.setPdxSerializer(serializer);
		factory.setPdxPersistent(true);

		Cache cache = factory.create();

		RegionFactory<Object, Object> regionFactory = cache.createRegionFactory();
		regionFactory.setDataPolicy(DataPolicy.PERSISTENT_REPLICATE);
		region = regionFactory.create("foo");
	}

	@Test
	public void serializeAndDeserializeCorrectly() {

		Address address = new Address();
		address.zipCode = "01234";
		address.city = "London";

		Person person = new Person(1L, "Oliver", "Gierke");
		person.address = address;

		region.put(1L, person);
		Object result = region.get(1L);

		assertThat(result instanceof Person, is(true));

		Person reference = person;
		assertThat(reference.getFirstname(), is(person.getFirstname()));
		assertThat(reference.getLastname(), is(person.getLastname()));
		assertThat(reference.address, is(person.address));
	}
}
