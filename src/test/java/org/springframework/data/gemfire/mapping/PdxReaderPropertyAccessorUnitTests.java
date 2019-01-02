/*
 * Copyright 2012-2019 the original author or authors.
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

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.apache.geode.pdx.PdxReader;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.core.convert.TypeDescriptor;
import org.springframework.expression.TypedValue;

/**
 *
 * @author Oliver Gierke
 */
@RunWith(MockitoJUnitRunner.class)
public class PdxReaderPropertyAccessorUnitTests {

	@Mock
	PdxReader reader;

	@Test
	public void appliesToPdxReadersOnly() {
		List<Class<?>> classes = Arrays.asList(PdxReaderPropertyAccessor.INSTANCE.getSpecificTargetClasses());
		assertThat(classes, hasItem(PdxReader.class));
	}

	@Test
	public void canReadPropertyIfReaderHasField() {

		when(reader.hasField("key")).thenReturn(true);
		assertThat(PdxReaderPropertyAccessor.INSTANCE.canRead(null, reader, "key"), is(true));

		when(reader.hasField("key")).thenReturn(false);
		assertThat(PdxReaderPropertyAccessor.INSTANCE.canRead(null, reader, "key"), is(false));
	}

	@Test
	public void returnsTypedNullIfNullIsReadFromReader() {

		when(reader.readObject("key")).thenReturn(null);
		assertThat(PdxReaderPropertyAccessor.INSTANCE.read(null, reader, "key"), is(TypedValue.NULL));
	}

	@Test
	public void returnsTypeValueWithValueReadFromReader() {

		when(reader.readObject("key")).thenReturn("String");

		TypedValue result = PdxReaderPropertyAccessor.INSTANCE.read(null, reader, "key");

		assertThat(result.getTypeDescriptor(), is(TypeDescriptor.valueOf(String.class)));
		assertThat(result.getValue(), is((Object) "String"));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void doesNotSupportWrites() {

		assertThat(PdxReaderPropertyAccessor.INSTANCE.canWrite(null, null, null), is(false));
		PdxReaderPropertyAccessor.INSTANCE.write(null, null, null, reader);
	}
}
