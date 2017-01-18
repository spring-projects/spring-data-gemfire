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

package org.springframework.data.gemfire.function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.pdx.PdxInstance;
import org.apache.geode.pdx.PdxInstanceFactory;
import org.apache.geode.pdx.PdxReader;
import org.apache.geode.pdx.PdxSerializer;
import org.apache.geode.pdx.PdxWriter;
import org.apache.geode.pdx.internal.PdxInstanceEnum;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.fork.ServerProcess;
import org.springframework.data.gemfire.fork.SpringContainerProcess;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.data.gemfire.function.sample.ApplicationDomainFunctionExecutions;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * The ClientCacheFunctionExecutionWithPdxIntegrationTest class is a test suite of test cases testing Spring Data
 * GemFire's Function annotation support and interaction between a GemFire client and server Cache
 * when PDX is configured and read-serialized is set to true.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see SpringContainerProcess
 * @see org.springframework.data.gemfire.function.annotation.GemfireFunction
 * @see org.springframework.data.gemfire.function.sample.ApplicationDomainFunctionExecutions
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.pdx.PdxInstance
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.apache.geode.pdx.internal.PdxInstanceEnum
 * @since 1.5.2
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClientCacheFunctionExecutionWithPdxIntegrationTest extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper gemfireServer;

	@Autowired
	private ClientCache gemfireClientCache;

	@Autowired
	private ApplicationDomainFunctionExecutions functionExecutions;

	@BeforeClass
	public static void startGemFireServer() throws Exception {

		int availablePort = findAvailablePort();

		gemfireServer = run(ServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort),
			getServerContextXmlFileLocation(ClientCacheFunctionExecutionWithPdxIntegrationTest.class));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	private PdxInstance toPdxInstance(Map<String, Object> pdxData) {

		PdxInstanceFactory pdxInstanceFactory =
			gemfireClientCache.createPdxInstanceFactory(pdxData.get("@type").toString());

		for (Map.Entry<String, Object> entry : pdxData.entrySet()) {
			pdxInstanceFactory.writeObject(entry.getKey(), entry.getValue());
		}

		return pdxInstanceFactory.create();
	}

	@Test
	public void convertedFunctionArgumentTypes() {

		Class[] argumentTypes = functionExecutions.captureConvertedArgumentTypes("test", 1,
			Boolean.TRUE, new Person("Jon", "Doe"), Gender.MALE);

		assertNotNull(argumentTypes);
		assertEquals(5, argumentTypes.length);
		assertEquals(String.class, argumentTypes[0]);
		assertEquals(Integer.class, argumentTypes[1]);
		assertEquals(Boolean.class, argumentTypes[2]);
		assertEquals(Person.class, argumentTypes[3]);
		assertEquals(Gender.class, argumentTypes[4]);
	}

	@Test
	public void unconvertedFunctionArgumentTypes() {

		Class[] argumentTypes = functionExecutions.captureUnconvertedArgumentTypes("test", 2,
			Boolean.FALSE, new Person("Jane", "Doe"), Gender.FEMALE);

		assertNotNull(argumentTypes);
		assertEquals(5, argumentTypes.length);
		assertEquals(String.class, argumentTypes[0]);
		assertEquals(Integer.class, argumentTypes[1]);
		assertEquals(Boolean.class, argumentTypes[2]);
		assertTrue(PdxInstance.class.isAssignableFrom(argumentTypes[3]));
		assertEquals(PdxInstanceEnum.class, argumentTypes[4]);
	}

	@Test
	public void getAddressFieldValue() {
		assertEquals("Portland", functionExecutions.getAddressField(new Address(
			"100 Main St.", "Portland", "OR", "97205"), "city"));
	}

	@Test
	public void pdxDataFieldValue() {

		Map<String, Object> pdxData = new HashMap<String, Object>(3);

		pdxData.put("@type", "x.y.z.domain.MyApplicationDomainType");
		pdxData.put("booleanField", Boolean.TRUE);
		pdxData.put("integerField", 123);
		pdxData.put("stringField", "test");

		Integer value = (Integer) functionExecutions.getDataField(toPdxInstance(pdxData), "integerField");

		assertEquals(pdxData.get("integerField"), value);
	}

	public static class ApplicationDomainFunctions {

		private Class[] getArgumentTypes(final Object... arguments) {

			Class[] argumentTypes = new Class[arguments.length];
			int index = 0;

			for (Object argument : arguments) {
				argumentTypes[index] = arguments[index].getClass();
				index++;
			}

			return argumentTypes;
		}

		@GemfireFunction
		public Class[] captureConvertedArgumentTypes(String stringValue, Integer integerValue, Boolean booleanValue,
				Person person, Gender gender) {

			return getArgumentTypes(stringValue, integerValue, booleanValue, person, gender);
		}

		@GemfireFunction
		public Class[] captureUnconvertedArgumentTypes(String stringValue, Integer integerValue, Boolean booleanValue,
				Object domainObject, Object enumValue) {

			return getArgumentTypes(stringValue, integerValue, booleanValue, domainObject, enumValue);
		}

		@GemfireFunction
		public String getAddressField(PdxInstance address, String fieldName) {
			Assert.isTrue(Address.class.getName().equals(address.getClassName()), "Address is not the correct type");
			return String.valueOf(address.getField(fieldName));
		}

		@GemfireFunction
		public Object getDataField(PdxInstance data, String fieldName) {
			return data.getField(fieldName);
		}
	}

	public static class Address {

		private final String street;
		private final String city;
		private final String state; // refactor; use Enum!
		private final String zipCode;

		public Address(String street, String city, String state, String zipCode) {
			Assert.hasText("The Address 'street' must be specified", street);
			Assert.hasText("The Address 'city' must be specified", city);
			Assert.hasText("The Address 'state' must be specified", state);
			Assert.hasText("The Address 'zipCode' must be specified", zipCode);

			this.street = street;
			this.city = city;
			this.state = state;
			this.zipCode = zipCode;
		}

		public String getStreet() {
			return street;
		}

		public String getCity() {
			return city;
		}

		public String getState() {
			return state;
		}

		public String getZipCode() {
			return zipCode;
		}

		@Override
		public boolean equals(final Object obj) {

			if (obj == this) {
				return true;
			}

			if (!(obj instanceof Address)) {
				return false;
			}

			Address that = (Address) obj;

			return ObjectUtils.nullSafeEquals(this.getStreet(), that.getStreet())
				&& ObjectUtils.nullSafeEquals(this.getCity(), that.getCity())
				&& ObjectUtils.nullSafeEquals(this.getState(), that.getState())
				&& ObjectUtils.nullSafeEquals(this.getZipCode(), that.getZipCode());
		}

		@Override
		public int hashCode() {

			int hashValue = 17;

			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getStreet());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getCity());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getState());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getZipCode());

			return hashValue;
		}

		@Override
		public String toString() {
			return String.format("%1$s %2$s, %3$s %4$s", getStreet(), getCity(), getState(), getZipCode());
		}
	}

	public static enum Gender {
		FEMALE,
		MALE
	}

	public static class Person {

		private final String firstName;
		private final String lastName;

		public Person(String firstName, String lastName) {
			Assert.hasText(firstName, "The person's first name must be specified!");
			Assert.hasText(lastName, "The person's last name must be specified!");

			this.firstName = firstName;
			this.lastName = lastName;
		}

		public String getFirstName() {
			return firstName;
		}

		public String getLastName() {
			return lastName;
		}

		@Override
		public boolean equals(final Object obj) {

			if (obj == this) {
				return true;
			}

			if (!(obj instanceof Person)) {
				return false;
			}

			Person that = (Person) obj;

			return ObjectUtils.nullSafeEquals(this.getFirstName(), that.getFirstName())
				&& ObjectUtils.nullSafeEquals(this.getLastName(), that.getLastName());
		}

		@Override
		public int hashCode() {

			int hashValue = 17;

			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getFirstName());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getLastName());

			return hashValue;
		}

		@Override
		public String toString() {
			return String.format("%1$s %2$s", getFirstName(), getLastName());
		}
	}

	public static class ComposablePdxSerializer implements PdxSerializer {

		private final PdxSerializer[] pdxSerializers;

		private ComposablePdxSerializer(PdxSerializer[] pdxSerializers) {
			this.pdxSerializers = pdxSerializers;
		}

		public static PdxSerializer compose(PdxSerializer... pdxSerializers) {
			return (pdxSerializers == null ? null : (pdxSerializers.length == 1 ? pdxSerializers[0]
				: new ComposablePdxSerializer(pdxSerializers)));
		}

		@Override
		public boolean toData(Object obj, PdxWriter out) {
			for (PdxSerializer pdxSerializer : this.pdxSerializers) {
				if (pdxSerializer.toData(obj, out)) {
					return true;
				}
			}

			return false;
		}

		@Override
		public Object fromData(Class<?> type, final PdxReader in) {
			for (PdxSerializer pdxSerializer : this.pdxSerializers) {
				Object obj = pdxSerializer.fromData(type, in);

				if (obj != null) {
					return obj;
				}
			}

			return null;
		}
	}

	public static class ComposablePdxSerializerFactoryBean implements FactoryBean<PdxSerializer>, InitializingBean {

		private List<PdxSerializer> pdxSerializers = Collections.emptyList();

		private PdxSerializer pdxSerializer;

		public void setPdxSerializers(final List<PdxSerializer> pdxSerializers) {
			this.pdxSerializers = pdxSerializers;
		}

		@Override
		public void afterPropertiesSet() throws Exception {
			pdxSerializer = ComposablePdxSerializer.compose(pdxSerializers.toArray(
				new PdxSerializer[pdxSerializers.size()]));
		}

		@Override
		public PdxSerializer getObject() throws Exception {
			return pdxSerializer;
		}

		@Override
		public Class<?> getObjectType() {
			return (pdxSerializer != null ? pdxSerializer.getClass() : PdxSerializer.class);
		}

		@Override
		public boolean isSingleton() {
			return true;
		}
	}

	public static class AddressPdxSerializer implements PdxSerializer {

		@Override
		public boolean toData(Object obj, PdxWriter out) {

			if (obj instanceof Address) {
				Address address = (Address) obj;
				out.writeString("street", address.getStreet());
				out.writeString("city", address.getCity());
				out.writeString("state", address.getState());
				out.writeString("zipCode", address.getZipCode());
				return true;
			}

			return false;
		}

		@Override
		public Object fromData(Class<?> type, PdxReader in) {

			if (Address.class.isAssignableFrom(type)) {
				return new Address(in.readString("street"), in.readString("city"), in.readString("state"),
					in.readString("zipCode"));
			}

			return null;
		}
	}
	public static class PersonPdxSerializer implements PdxSerializer {

		@Override
		public boolean toData(Object obj, PdxWriter out) {

			if (obj instanceof Person) {
				Person person = (Person) obj;
				out.writeString("firstName", person.getFirstName());
				out.writeString("lastName", person.getLastName());
				return true;
			}

			return false;
		}

		@Override
		public Object fromData(Class<?> type, PdxReader in) {
			if (Person.class.isAssignableFrom(type)) {
				return new Person(in.readString("firstName"), in.readString("lastName"));
			}

			return null;
		}
	}
}
