/*
 * Copyright 2017-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.repository.query;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.mockito.InOrder;
import org.springframework.data.repository.query.QueryMethod;

/**
 * Unit tests for {@link AbstractQueryPostProcessor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.repository.query.AbstractQueryPostProcessor
 * @since 1.0.0
 */
public class AbstractQueryPostProcessorUnitTests {

	@Test
	@SuppressWarnings("unchecked")
	public void processAfterReturnsCompositeQueryPostProcessorAndPostProcessesInOrder() {

		QueryMethod mockQueryMethod = mock(QueryMethod.class);

		String query = "SELECT * FROM /Test";

		AbstractQueryPostProcessor<?, String> mockQueryPostProcessorOne = mock(AbstractQueryPostProcessor.class);
		AbstractQueryPostProcessor<?, String> mockQueryPostProcessorTwo = mock(AbstractQueryPostProcessor.class);

		when(mockQueryPostProcessorOne.processAfter(any(QueryPostProcessor.class))).thenCallRealMethod();
		when(mockQueryPostProcessorOne.postProcess(any(QueryMethod.class), anyString(), anyVararg())).thenReturn(query);
		when(mockQueryPostProcessorTwo.postProcess(any(QueryMethod.class), anyString(), anyVararg())).thenReturn(query);

		QueryPostProcessor<?, String> composite = mockQueryPostProcessorOne.processAfter(mockQueryPostProcessorTwo);

		assertThat(composite).isNotNull();
		assertThat(composite).isNotSameAs(mockQueryPostProcessorOne);
		assertThat(composite).isNotSameAs(mockQueryPostProcessorTwo);
		assertThat(composite.postProcess(mockQueryMethod, query)).isEqualTo(query);

		InOrder inOrder = inOrder(mockQueryPostProcessorOne, mockQueryPostProcessorTwo);

		inOrder.verify(mockQueryPostProcessorTwo, times(1))
			.postProcess(eq(mockQueryMethod), eq(query), anyVararg());

		inOrder.verify(mockQueryPostProcessorOne, times(1))
			.postProcess(eq(mockQueryMethod), eq(query), anyVararg());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void processAfterReturnsThis() {

		AbstractQueryPostProcessor<?, ?> mockQueryPostProcessor = mock(AbstractQueryPostProcessor.class);

		when(mockQueryPostProcessor.processAfter(any(AbstractQueryPostProcessor.class))).thenCallRealMethod();

		assertThat(mockQueryPostProcessor.processAfter(null)).isSameAs(mockQueryPostProcessor);
	}
	@Test
	@SuppressWarnings("unchecked")
	public void processBeforeReturnsCompositeQueryPostProcessorAndPostProcessesInOrder() {

		QueryMethod mockQueryMethod = mock(QueryMethod.class);

		String query = "SELECT * FROM /Test";

		AbstractQueryPostProcessor<?, String> mockQueryPostProcessorOne = mock(AbstractQueryPostProcessor.class);
		AbstractQueryPostProcessor<?, String> mockQueryPostProcessorTwo = mock(AbstractQueryPostProcessor.class);

		when(mockQueryPostProcessorOne.processBefore(any(QueryPostProcessor.class))).thenCallRealMethod();
		when(mockQueryPostProcessorOne.postProcess(any(QueryMethod.class), anyString(), anyVararg())).thenReturn(query);
		when(mockQueryPostProcessorTwo.postProcess(any(QueryMethod.class), anyString(), anyVararg())).thenReturn(query);

		QueryPostProcessor<?, String> composite = mockQueryPostProcessorOne.processBefore(mockQueryPostProcessorTwo);

		assertThat(composite).isNotNull();
		assertThat(composite).isNotSameAs(mockQueryPostProcessorOne);
		assertThat(composite).isNotSameAs(mockQueryPostProcessorTwo);
		assertThat(composite.postProcess(mockQueryMethod, query)).isEqualTo(query);

		InOrder inOrder = inOrder(mockQueryPostProcessorOne, mockQueryPostProcessorTwo);

		inOrder.verify(mockQueryPostProcessorOne, times(1))
			.postProcess(eq(mockQueryMethod), eq(query), anyVararg());

		inOrder.verify(mockQueryPostProcessorTwo, times(1))
			.postProcess(eq(mockQueryMethod), eq(query), anyVararg());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void processBeforeReturnsThis() {

		AbstractQueryPostProcessor<?, ?> mockQueryPostProcessor = mock(AbstractQueryPostProcessor.class);

		when(mockQueryPostProcessor.processBefore(any(QueryPostProcessor.class))).thenCallRealMethod();

		assertThat(mockQueryPostProcessor.processBefore(null)).isSameAs(mockQueryPostProcessor);
	}
}
