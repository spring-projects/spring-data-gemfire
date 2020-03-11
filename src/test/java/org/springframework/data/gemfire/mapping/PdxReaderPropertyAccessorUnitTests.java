/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.mapping;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.apache.geode.pdx.PdxReader;

import org.springframework.core.convert.TypeDescriptor;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.TypedValue;

/**
 * Unit Tests for {@link PdxReaderPropertyAccessor}.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.mapping.PdxReaderPropertyAccessor
 */
@RunWith(MockitoJUnitRunner.class)
public class PdxReaderPropertyAccessorUnitTests {

	@Mock
	private EvaluationContext mockEvaluationContext;

	@Mock
	private PdxReader mockReader;

	@Test
	public void appliesToPdxReadersOnly() {

		List<Class<?>> classes = Arrays.asList(PdxReaderPropertyAccessor.INSTANCE.getSpecificTargetClasses());

		assertThat(classes).contains(PdxReader.class);
	}

	@Test
	public void canReadPropertyIfReaderHasField() {

		when(this.mockReader.hasField("key")).thenReturn(true);

		assertThat(PdxReaderPropertyAccessor.INSTANCE.canRead(this.mockEvaluationContext, this.mockReader, "key"))
			.isTrue();
	}

	@Test
	public void cannotReadPropertyWhenReaderDoesNotHaveField() {

		when(this.mockReader.hasField("key")).thenReturn(false);

		assertThat(PdxReaderPropertyAccessor.INSTANCE.canRead(this.mockEvaluationContext, this.mockReader, "key"))
			.isFalse();
	}

	@Test
	public void returnsTypedNullIfNullIsReadFromReader() {

		when(this.mockReader.readObject("key")).thenReturn(null);

		assertThat(PdxReaderPropertyAccessor.INSTANCE.read(this.mockEvaluationContext, this.mockReader, "key"))
			.isEqualTo(TypedValue.NULL);
	}

	@Test
	public void returnsTypeValueWithValueReadFromReader() {

		when(this.mockReader.readObject("key")).thenReturn("String");

		TypedValue result = PdxReaderPropertyAccessor.INSTANCE.read(this.mockEvaluationContext, this.mockReader, "key");

		assertThat(result.getTypeDescriptor()).isEqualTo(TypeDescriptor.valueOf(String.class));
		assertThat(result.getValue()).isEqualTo("String");
	}

	@SuppressWarnings("all")
	@Test(expected = UnsupportedOperationException.class)
	public void doesNotSupportWrites() {

		assertThat(PdxReaderPropertyAccessor.INSTANCE.canWrite(this.mockEvaluationContext, null, null)).isFalse();

		PdxReaderPropertyAccessor.INSTANCE.write(this.mockEvaluationContext, null, null, this.mockReader);
	}
}
