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

package org.springframework.data.gemfire.mapping;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.PersistentPropertyAccessor;
import org.springframework.data.mapping.model.MappingException;

/**
 * The BeanPropertyAccessorTest class is a test suite of test cases testing the contract and functionality
 * of the BeanPropertyAccessor class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.mapping.BeanPropertyAccessor
 * @see org.springframework.data.mapping.PersistentProperty
 * @see org.springframework.data.mapping.PersistentPropertyAccessor
 * @since 1.7.0
 */
public class BeanPropertyAccessorTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Test
	public void createWithNullBean() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("Bean must not be null");
		BeanPropertyAccessor.create(null);
	}

	@Test
	public void setAndGetPropertyValueUsingFieldAccess() throws Exception {
		ExampleBean<String> bean = new ExampleBean<String>();

		PersistentProperty<?> mockPersistentProperty = mock(PersistentProperty.class, "MockPersistentProperty");

		when(mockPersistentProperty.usePropertyAccess()).thenReturn(false);
		when(mockPersistentProperty.getField()).thenReturn(ExampleBean.class.getDeclaredField("value"));
		when(mockPersistentProperty.getGetter()).thenReturn(null);
		when(mockPersistentProperty.getSetter()).thenReturn(null);

		PersistentPropertyAccessor propertyAccessor = BeanPropertyAccessor.create(bean);

		assertSame(bean, propertyAccessor.getBean());
		assertThat(bean.getValue(), is(nullValue()));

		propertyAccessor.setProperty(mockPersistentProperty, "mock");

		assertThat(bean.getValue(), is(equalTo("mock")));
		assertThat(String.valueOf(propertyAccessor.getProperty(mockPersistentProperty)), is(equalTo("mock")));

		when(mockPersistentProperty.usePropertyAccess()).thenReturn(true);
		propertyAccessor.setProperty(mockPersistentProperty, null);

		assertThat(bean.getValue(), is(nullValue()));
		assertThat(propertyAccessor.getProperty(mockPersistentProperty), is(nullValue()));

		verify(mockPersistentProperty, times(4)).usePropertyAccess();
		verify(mockPersistentProperty, times(4)).getField();
		verify(mockPersistentProperty, times(1)).getGetter();
		verify(mockPersistentProperty, times(1)).getSetter();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void setAndGetPropertyValueUsingPropertyAccess() throws Exception {
		ExampleBean<String> bean = new ExampleBean<String>();

		PersistentProperty<?> mockPersistentProperty = mock(PersistentProperty.class, "MockPersistentProperty");

		when(mockPersistentProperty.usePropertyAccess()).thenReturn(true);
		when(mockPersistentProperty.getSetter()).thenReturn(ExampleBean.class.getMethod("setValue", Object.class));
		when(mockPersistentProperty.getGetter()).thenReturn(ExampleBean.class.getMethod("getValue"));

		PersistentPropertyAccessor propertyAccessor = BeanPropertyAccessor.create(bean);

		assertSame(bean, propertyAccessor.getBean());
		assertThat(bean.getValue(), is(nullValue()));

		propertyAccessor.setProperty(mockPersistentProperty, "test");

		assertThat(bean.getValue(), is(equalTo("test")));
		assertThat(String.valueOf(propertyAccessor.getProperty(mockPersistentProperty)), is(equalTo("test")));

		propertyAccessor.setProperty(mockPersistentProperty, null);

		assertThat(bean.getValue(), is(nullValue()));
		assertThat(propertyAccessor.getProperty(mockPersistentProperty), is(nullValue()));

		verify(mockPersistentProperty, times(4)).usePropertyAccess();
		verify(mockPersistentProperty, never()).getField();
		verify(mockPersistentProperty, times(4)).getGetter();
		verify(mockPersistentProperty, times(4)).getSetter();
	}

	@Test
	public void setPropertyWithNonExistingField() throws Exception {
		ExampleBean<String> bean = new ExampleBean<String>();

		PersistentProperty<?> mockPersistentProperty = mock(PersistentProperty.class, "MockPersistentProperty");

		when(mockPersistentProperty.usePropertyAccess()).thenReturn(false);
		when(mockPersistentProperty.getField()).thenReturn(InvalidBean.class.getDeclaredField("temp"));

		PersistentPropertyAccessor propertyAccessor = BeanPropertyAccessor.create(bean);

		assertSame(bean, propertyAccessor.getBean());
		assertThat(bean.getValue(), is(nullValue()));

		expectedException.expect(MappingException.class);
		expectedException.expectCause(is(notNullValue(Throwable.class)));
		expectedException.expectMessage(Matchers.startsWith(String.format(
			"Failed to get value of property (%1$s)!", mockPersistentProperty)));

		try {
			propertyAccessor.getProperty(mockPersistentProperty);
		}
		finally {
			verify(mockPersistentProperty, times(1)).usePropertyAccess();
			verify(mockPersistentProperty, times(1)).getField();
			verify(mockPersistentProperty, never()).getGetter();
		}
	}

	@Test
	public void setPropertyWithNonExistingSetterMethod() throws Exception {
		ExampleBean<String> bean = new ExampleBean<String>();

		PersistentProperty<?> mockPersistentProperty = mock(PersistentProperty.class, "MockPersistentProperty");

		when(mockPersistentProperty.usePropertyAccess()).thenReturn(true);
		when(mockPersistentProperty.getSetter()).thenReturn(InvalidBean.class.getMethod("getTemp"));

		PersistentPropertyAccessor propertyAccessor = BeanPropertyAccessor.create(bean);

		assertSame(bean, propertyAccessor.getBean());
		assertThat(bean.getValue(), is(nullValue()));

		expectedException.expect(MappingException.class);
		expectedException.expectCause(is(notNullValue(Throwable.class)));
		expectedException.expectMessage(Matchers.startsWith(String.format(
			"Failed to set property (%1$s) to value (test)!", mockPersistentProperty)));

		try {
			propertyAccessor.setProperty(mockPersistentProperty, "test");
		}
		finally {
			assertThat(bean.getValue(), is(nullValue()));
			verify(mockPersistentProperty, times(1)).usePropertyAccess();
			verify(mockPersistentProperty, never()).getField();
			verify(mockPersistentProperty, times(2)).getSetter();
		}
	}

	public static class ExampleBean<T> {

		private T value;

		public T getValue() {
			return value;
		}

		public void setValue(final T value) {
			this.value = value;
		}

		@Override
		public String toString() {
			return String.format("{ @type = %1$s, value = %2$s }", getClass().getName(), getValue());
		}
	}

	@SuppressWarnings("unused")
	public static class InvalidBean {

		Object temp;

		public Object getTemp() {
			return temp;
		}

		public void setTemp(final Object temp) {
			this.temp = temp;
		}
	}

}
