/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
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
import static org.junit.Assert.assertNotSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.internal.cache.GemFireCacheImpl;
import org.apache.geode.pdx.PdxInstance;
import org.apache.geode.pdx.PdxInstanceFactory;
import org.apache.geode.pdx.PdxReader;
import org.apache.geode.pdx.PdxSerializer;
import org.apache.geode.pdx.PdxWriter;
import org.apache.geode.pdx.internal.PdxInstanceEnum;
import org.apache.geode.pdx.internal.PdxInstanceFactoryImpl;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * The PdxFunctionArgumentResolverTest class is a test suite of test cases testing the contract and functionality
 * of the PdxFunctionArgumentResolver class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.function.PdxFunctionArgumentResolver
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.execute.FunctionContext
 * @see org.apache.geode.pdx.PdxInstance
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.apache.geode.pdx.internal.PdxInstanceEnum
 * @since 1.5.2
 */
@SuppressWarnings("unused")
public class PdxFunctionArgumentResolverTest {

	private static Cache gemfireCache;

	private PdxFunctionArgumentResolver functionArgumentResolver;

	@BeforeClass
	public static void setupGemFire() {

		gemfireCache = new CacheFactory()
			.setPdxSerializer(new PersonPdxSerializer())
			.setPdxReadSerialized(true)
			.set("name", PdxFunctionArgumentResolverTest.class.getSimpleName())
			.set("log-level", "error")
			.create();
	}

	@AfterClass
	public static void tearDown() {
		gemfireCache.close();
		gemfireCache = null;
	}

	protected Method getMethod(final Class<?> type, final String methodName, final Class<?>... parameterTypes) {
		try {
			return type.getDeclaredMethod(methodName, parameterTypes);
		}
		catch (NoSuchMethodException e) {
			throw new RuntimeException(String.format(
				"Failed to get method (%1$s) with signature (%2$s) on Class type (%3$s)!", methodName,
					getMethodSignature(methodName, parameterTypes), type.getClass().getName()));
		}
	}

	protected Object getMethodSignature(final String methodName, final Class<?>... parameterTypes) {
		StringBuilder methodSignature = new StringBuilder(methodName);
		int count = 0;

		methodSignature.append("(");

		for (Class parameterType : parameterTypes) {
			methodSignature.append(count++ > 0 ? ", :" : ":").append(parameterType.getSimpleName());
		}

		methodSignature.append("):Void");

		return methodSignature.toString();
	}

	protected void assertArguments(final Object[] expectedArguments, final Object[] actualArguments) {
		assertNotNull(actualArguments);
		assertNotSame(expectedArguments, actualArguments);
		assertEquals(expectedArguments.length, actualArguments.length);

		for (int index = 0; index < expectedArguments.length; index++) {
			assertEquals(expectedArguments[index], actualArguments[index]);
		}
	}

	protected Person createPerson(final String firstName, final String lastName, final Gender gender) {
		return new Person(firstName, lastName, gender);
	}

	protected PdxInstance toPdxInstance(final Person person) {
		PdxInstanceFactory pdxInstanceFactory = gemfireCache.createPdxInstanceFactory(person.getClass().getName());
		pdxInstanceFactory.writeString("firstName", person.getFirstName());
		pdxInstanceFactory.writeString("lastName", person.getLastName());
		pdxInstanceFactory.writeObject("gender", person.getGender());
		return pdxInstanceFactory.create();
	}

	protected PdxInstance toPdxInstance(final Map<String, Object> objectData) {
		PdxInstanceFactory pdxInstanceFactory = gemfireCache.createPdxInstanceFactory(objectData.get("@type").toString());

		for (Map.Entry<String, Object> entry : objectData.entrySet()) {
			if (!"@type".equals(entry.getKey())) {
				pdxInstanceFactory.writeObject(entry.getKey(), entry.getValue());
			}
		}

		return pdxInstanceFactory.create();
	}

	protected PdxInstance toPdxInstance(final Enum enumeratedType) {
		return PdxInstanceFactoryImpl.createPdxEnum(enumeratedType.getClass().getName(), enumeratedType.name(),
			enumeratedType.ordinal(), (GemFireCacheImpl) gemfireCache);
	}

	@Test
	public void testResolveSimpleFunctionArguments() {
		functionArgumentResolver = new PdxFunctionArgumentResolver() {
			@Override public Method getFunctionAnnotatedMethod() {
				return getMethod(FunctionExecutions.class, "simpleMethod", Boolean.class, Character.class,
					Integer.class, Double.class, String.class);
			}
		};

		Object[] expectedArguments = { Boolean.TRUE, 'C', 123, Math.PI, "TEST" };

		FunctionContext mockFunctionContext = mock(FunctionContext.class, "testResolveSimpleFunctionArguments");

		when(mockFunctionContext.getArguments()).thenReturn(expectedArguments);

		Object[] actualArguments = functionArgumentResolver.resolveFunctionArguments(mockFunctionContext);

		assertArguments(expectedArguments, actualArguments);
	}

	@Test
	public void testResolveNonSerializedApplicationDomainTypeFunctionArguments() {
		functionArgumentResolver = new PdxFunctionArgumentResolver() {
			@Override public Method getFunctionAnnotatedMethod() {
				return getMethod(FunctionExecutions.class, "nonSerializedMethod", Boolean.class, Person.class,
					Character.class, Person.class, Integer.class, Double.class, Gender.class, String.class);
			}
		};

		Object[] expectedArguments = { Boolean.TRUE, createPerson("Jon", "Doe", Gender.MALE), 'C',
			createPerson("Jane", "Doe", Gender.FEMALE), 123, Math.PI, Gender.FEMALE, "test" };

		FunctionContext mockFunctionContext = mock(FunctionContext.class, "testResolveNonSerializedApplicationDomainTypeFunctionArguments");

		when(mockFunctionContext.getArguments()).thenReturn(expectedArguments);

		Object[] actualArguments = functionArgumentResolver.resolveFunctionArguments(mockFunctionContext);

		assertArguments(expectedArguments, actualArguments);
	}

	@Test
	public void testResolveSerializedApplicationDomainTypeFunctionArguments() {
		functionArgumentResolver = new PdxFunctionArgumentResolver() {
			@Override public Method getFunctionAnnotatedMethod() {
				return getMethod(FunctionExecutions.class, "serializedMethod", Boolean.class, Person.class,
					String.class, Gender.class);
			}
		};

		Person jackHandy = createPerson("Jack", "Handy", Gender.MALE);

		Object[] serializedArguments = { Boolean.TRUE, toPdxInstance(jackHandy), "test", toPdxInstance(Gender.MALE) };
		Object[] expectedArguments = { Boolean.TRUE, jackHandy, "test", Gender.MALE };

		FunctionContext mockFunctionContext = mock(FunctionContext.class, "testResolveSerializedApplicationDomainTypeFunctionArguments");

		when(mockFunctionContext.getArguments()).thenReturn(serializedArguments);

		Object[] actualArguments = functionArgumentResolver.resolveFunctionArguments(mockFunctionContext);

		assertArguments(expectedArguments, actualArguments);
	}

	@Test
	public void testResolveUnnecessaryDeserializationFunctionArguments() {
		functionArgumentResolver = new PdxFunctionArgumentResolver() {
			@Override public Method getFunctionAnnotatedMethod() {
				return getMethod(FunctionExecutions.class, "unnecessaryDeserializationMethod", Boolean.class,
					Object.class, String.class, PdxInstanceEnum.class);
			}
		};

		Person sandyHandy = createPerson("Sandy", "Handy", Gender.FEMALE);

		Object[] expectedArguments = { Boolean.TRUE, toPdxInstance(sandyHandy), "test", toPdxInstance(Gender.FEMALE) };

		FunctionContext mockFunctionContext = mock(FunctionContext.class, "testResolveUnnecessaryDeserializationFunctionArguments");

		when(mockFunctionContext.getArguments()).thenReturn(expectedArguments);

		Object[] actualArguments = functionArgumentResolver.resolveFunctionArguments(mockFunctionContext);

		assertArguments(expectedArguments, actualArguments);
	}

	@Test
	public void testResolveUnresolvableApplicationDomainTypeFunctionArguments() {
		functionArgumentResolver = new PdxFunctionArgumentResolver() {
			@Override public Method getFunctionAnnotatedMethod() {
				return getMethod(FunctionExecutions.class, "unresolvableMethod", String.class, Object.class);
			}
		};

		Map<String, Object> addressData = new HashMap<String, Object>(5);

		addressData.put("@type", "org.example.Address");
		addressData.put("street", "100 Main St.");
		addressData.put("city", "Portland");
		addressData.put("state", "OR");
		addressData.put("zip", "12345");

		Object[] expectedArguments = { "test", toPdxInstance(addressData) };

		FunctionContext mockFunctionContext = mock(FunctionContext.class, "testResolveUnresolvableApplicationDomainTypeFunctionArguments");

		when(mockFunctionContext.getArguments()).thenReturn(expectedArguments);

		Object[] actualArguments = functionArgumentResolver.resolveFunctionArguments(mockFunctionContext);

		assertArguments(expectedArguments, actualArguments);
	}

	public static interface FunctionExecutions {

		void simpleMethod(Boolean value1, Character value2, Integer value3, Double value4, String value5);

		void nonSerializedMethod(Boolean value1, Person person1, Character value2, Integer value3, Person person2, Double value4, Gender gender, String value5);

		void serializedMethod(Boolean value1, Person person, String value2, Gender gender);

		void unnecessaryDeserializationMethod(Boolean value1, Object person, String value2, PdxInstanceEnum gender);

		void unresolvableMethod(String value, Object pdxInstance);
	}

	public static enum Gender {
		FEMALE,
		MALE
	}

	public static class Person {

		private final Gender gender;

		private final String firstName;
		private final String lastName;

		public Person(final String firstName, final String lastName, final Gender gender) {
			Assert.hasText(firstName, "The person's first name must be specified!");
			Assert.hasText(lastName, "The person's last name must be specified!");
			Assert.notNull(gender, "The person's gender must be specified!");
			this.firstName = firstName;
			this.lastName = lastName;
			this.gender = gender;
		}

		public String getFirstName() {
			return firstName;
		}

		public String getLastName() {
			return lastName;
		}

		public Gender getGender() {
			return gender;
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
				&& ObjectUtils.nullSafeEquals(this.getLastName(), that.getLastName())
				&& ObjectUtils.nullSafeEquals(this.getGender(), that.getGender());
		}

		@Override
		public int hashCode() {
			int hashValue = 17;
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getFirstName());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getLastName());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getGender());
			return hashValue;
		}

		@Override
		public String toString() {
			return String.format("%1$s %2$s is a %3$s", getFirstName(), getLastName(), getGender());
		}
	}

	public static class PersonPdxSerializer implements PdxSerializer {

		@Override
		public boolean toData(final Object obj, final PdxWriter out) {
			if (obj instanceof Person) {
				Person person = (Person) obj;
				out.writeString("firstName", person.getFirstName());
				out.writeString("lastName", person.getLastName());
				out.writeObject("gender", person.getGender());
				return true;
			}

			return false;
		}

		@Override
		public Object fromData(final Class<?> type, final PdxReader in) {
			if (Person.class.isAssignableFrom(type)) {
				return new Person(in.readString("firstName"), in.readString("lastName"),
					(Gender) in.readObject("gender"));
			}

			return null;
		}
	}

}
