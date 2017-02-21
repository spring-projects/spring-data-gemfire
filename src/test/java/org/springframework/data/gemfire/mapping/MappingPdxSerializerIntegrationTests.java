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

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

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
import org.springframework.data.gemfire.repository.sample.Address;
import org.springframework.data.gemfire.repository.sample.Person;

/**
 * Integration tests for {@link MappingPdxSerializer}.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
public class MappingPdxSerializerIntegrationTests {

	static Region<Object, Object> region;

	static Cache cache;

	@BeforeClass
	public static void setUp() {
		MappingPdxSerializer serializer = new MappingPdxSerializer(new GemfireMappingContext(),
				new DefaultConversionService());

		cache = new CacheFactory()
			.set("name", MappingPdxSerializerIntegrationTests.class.getSimpleName())
			.set("mcast-port", "0")
			.set("log-level", "warning")
			.setPdxSerializer(serializer)
			.setPdxPersistent(true)
			.create();

		region = cache.createRegionFactory()
			.setDataPolicy(DataPolicy.PERSISTENT_REPLICATE)
			.create("foo");
	}

	@AfterClass
	@SuppressWarnings("all")
	public static void tearDown() {
		try {
			cache.close();
		}
		catch (Exception ignore) {
		}
		finally {
			for (String name : new File(".").list(new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.startsWith("BACKUP");
				}
			})) {
				new File(name).delete();
			}
		}
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


	@Test
	public void serializeAndDeserializeCorrectlyWithDataSerializable() {

		Address address = new Address();
		address.zipCode = "01234";
		address.city = "London";

		PersonWithDataSerializableProperty person =
			new PersonWithDataSerializableProperty(2L, "Oliver", "Gierke",
				new DataSerializableProperty("foo"));

		person.address = address;

		region.put(2L, person);
		Object result = region.get(2L);

		assertThat(result instanceof PersonWithDataSerializableProperty, is(true));

		PersonWithDataSerializableProperty reference = person;
		assertThat(reference.getFirstname(), is(person.getFirstname()));
		assertThat(reference.getLastname(), is(person.getLastname()));
		assertThat(reference.address, is(person.address));
		assertThat(reference.dsProperty.getValue(),is("foo"));
	}

	@SuppressWarnings("serial")
	public static class PersonWithDataSerializableProperty extends Person {

		private DataSerializableProperty dsProperty;

		public PersonWithDataSerializableProperty(Long id, String firstname,
				String lastname, DataSerializableProperty dsProperty) {
			super(id, firstname, lastname);
			this.dsProperty = dsProperty;
		}

		public DataSerializableProperty getDataSerializableProperty() {
			return this.dsProperty;
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
		public void fromData(DataInput dataInput) throws IOException,
				ClassNotFoundException {
			value = dataInput.readUTF();

		}

		@Override
		public void toData(DataOutput dataOutput) throws IOException {
			dataOutput.writeUTF(value);
		}

		public String getValue() {
			return this.value;
		}

	}
}
