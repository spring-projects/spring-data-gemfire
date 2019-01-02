/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.search.lucene.support;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.search.lucene.support.PdxInstanceMethodInterceptor.newPdxInstanceMethodInterceptor;

import java.lang.reflect.Method;

import org.aopalliance.intercept.MethodInvocation;
import org.apache.geode.pdx.PdxInstance;
import org.apache.geode.pdx.WritablePdxInstance;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Unit tests for {@link PdxInstanceMethodInterceptor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.support.PdxInstanceMethodInterceptor
 * @since 1.1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class PdxInstanceMethodInterceptorUnitTests {

	@Mock
	private MethodInvocation mockMethodInvocation;

	@Mock
	private PdxInstance mockSource;

	@Mock
	private WritablePdxInstance mockNewSource;

	@SafeVarargs
	protected static <T> T[] asArray(T... array) {
		return array;
	}

	@Test
	public void newPdxInstanceMethodInterceptorWithValidObjectSourceIsSuccessful() {
		PdxInstanceMethodInterceptor methodInterceptor = newPdxInstanceMethodInterceptor((Object) mockSource);

		assertThat(methodInterceptor).isNotNull();
		assertThat(methodInterceptor.getSource()).isSameAs(mockSource);
	}

	@Test(expected = IllegalArgumentException.class)
	public void newPdxInstanceMethodInterceptorWithInvalidObjectSourceThrowsIllegalArgumentException() {
		try {
			newPdxInstanceMethodInterceptor(new Object());
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessageStartingWith(String.format(
				"Source [java.lang.Object] is not an instance of [%s]", PdxInstance.class.getName()));
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void newPdxInstanceMethodInterceptorWithPdxInstanceIsSuccessful() {
		PdxInstanceMethodInterceptor methodInterceptor = newPdxInstanceMethodInterceptor(mockSource);

		assertThat(methodInterceptor).isNotNull();
		assertThat(methodInterceptor.getSource()).isSameAs(mockSource);
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructPdxInstanceMethodInterceptorWithNullThrowsIllegalArgumentException() {
		try {
			new PdxInstanceMethodInterceptor(null);
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("Source must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void invokeObjectMethodIsHandled() throws Throwable {
		Person jonDoe = Person.newPerson("Jon", "Doe");
		Method toString = jonDoe.getClass().getMethod("toString");

		when(mockMethodInvocation.getMethod()).thenReturn(toString);
		when(mockMethodInvocation.proceed()).thenAnswer(invocationOnMock ->  toString.invoke(jonDoe));

		assertThat(newPdxInstanceMethodInterceptor(mockSource).invoke(mockMethodInvocation)).isEqualTo("Jon Doe");

		verify(mockMethodInvocation, times(1)).getMethod();
		verify(mockMethodInvocation, times(1)).proceed();
		verifyZeroInteractions(mockSource);
	}

	@Test
	public void invokeGetterOnSourceIsHandled() throws Throwable {
		Person jonDoe = Person.newPerson("Jon", "Doe");
		Method getFirstName = jonDoe.getClass().getMethod("getFirstName");

		when(mockMethodInvocation.getMethod()).thenReturn(getFirstName);
		when(mockSource.hasField(eq("firstName"))).thenReturn(true);
		when(mockSource.getField(eq("firstName"))).thenReturn(jonDoe.getFirstName());

		assertThat(newPdxInstanceMethodInterceptor(mockSource).invoke(mockMethodInvocation)).isEqualTo("Jon");

		verify(mockMethodInvocation, never()).proceed();
		verify(mockSource, times(1)).hasField(eq("firstName"));
		verify(mockSource, times(1)).getField(eq("firstName"));
		verifyNoMoreInteractions(mockSource);
	}

	@Test
	public void invokeSetterOnSourceIsHandled() throws Throwable {
		Person jonDoe = Person.newPerson("Jon", "Doe");
		Method setLastName = jonDoe.getClass().getMethod("setLastName", String.class);

		when(mockMethodInvocation.getMethod()).thenReturn(setLastName);
		when(mockMethodInvocation.getArguments()).thenReturn(asArray("Smith"));
		when(mockSource.hasField(eq("lastName"))).thenReturn(true);
		when(mockSource.createWriter()).thenReturn(mockNewSource);

		PdxInstanceMethodInterceptor methodInterceptor = newPdxInstanceMethodInterceptor(mockSource);

		assertThat(methodInterceptor.getSource()).isSameAs(mockSource);
		assertThat(methodInterceptor.invoke(mockMethodInvocation)).isNull();
		assertThat(methodInterceptor.getSource()).isEqualTo(mockNewSource);

		verify(mockMethodInvocation, times(1)).getMethod();
		verify(mockMethodInvocation, times(2)).getArguments();
		verify(mockSource, times(1)).hasField(eq("lastName"));
		verify(mockNewSource, times(1)).setField(eq("lastName"), eq("Smith"));
	}

	@Test(expected = IllegalStateException.class)
	public void invokeThrowsIllegalStateExceptionWhenPdxInstanceDoesNotHaveField() throws Throwable {
		Method getGender = Person.class.getMethod("getGender");

		when(mockMethodInvocation.getMethod()).thenReturn(getGender);
		when(mockSource.hasField(anyString())).thenReturn(false);

		try {
			newPdxInstanceMethodInterceptor(mockSource).invoke(mockMethodInvocation);
		}
		catch (IllegalStateException expected) {
			assertThat(expected).hasMessage("Source [%s] does not contain field with name [gender]", mockSource);
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockMethodInvocation, times(1)).getMethod();
			verify(mockSource, times(1)).hasField(eq("gender"));
			verifyNoMoreInteractions(mockSource);
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void invokeThrowsIllegalArgumentExceptionWhenMethodInvocationHasIncorrectNumberOfArguments()
			throws Throwable {

		Method setGender = Person.class.getMethod("setGender", Gender.class);

		when(mockMethodInvocation.getMethod()).thenReturn(setGender);
		when(mockSource.hasField(eq("gender"))).thenReturn(true);
		when(mockMethodInvocation.getArguments()).thenReturn(asArray(Gender.FEMALE, Gender.MALE));

		try {
			newPdxInstanceMethodInterceptor(mockSource).invoke(mockMethodInvocation);
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage(
				"Invoked setter method [setGender] must expect exactly 1 argument; Arguments were [[FEMALE, MALE]]");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockMethodInvocation, times(1)).getMethod();
			verify(mockMethodInvocation, times(2)).getArguments();
			verify(mockSource, times(1)).hasField(eq("gender"));
		}
	}

	@Test(expected = IllegalStateException.class)
	public void invokeThrowsIllegalArgumentExceptionWhenPdxInstanceHasNoWriter() throws Throwable {
		Method setGender = Person.class.getMethod("setGender", Gender.class);

		when(mockMethodInvocation.getMethod()).thenReturn(setGender);
		when(mockMethodInvocation.getArguments()).thenReturn(asArray(Gender.FEMALE));
		when(mockSource.hasField(eq("gender"))).thenReturn(true);
		when(mockSource.createWriter()).thenReturn(null);

		try {
			newPdxInstanceMethodInterceptor(mockSource).invoke(mockMethodInvocation);
		}
		catch (IllegalStateException expected) {
			assertThat(expected).hasMessage(
				"No writer for PdxInstance [%s] was found for setting field [gender] to value [FEMALE]", mockSource);
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockMethodInvocation, times(1)).getMethod();
			verify(mockMethodInvocation, times(2)).getArguments();
			verify(mockSource, times(1)).hasField(eq("gender"));
			verify(mockSource, times(1)).createWriter();
		}
	}

	enum Gender {
		FEMALE,
		MALE
	}

	@Data
	@RequiredArgsConstructor(staticName = "newPerson")
	@SuppressWarnings("unused")
	static class Person {

		Gender gender;

		@NonNull String firstName;
		@NonNull String lastName;

		/**
		 * @inheritDoc
		 */
		@Override
		public String toString() {
			return String.format("%1$s %2$s", getFirstName(), getLastName());
		}
	}
}
