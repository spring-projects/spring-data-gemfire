/*
 * Copyright 2019-2020 the original author or authors.
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
package org.springframework.data.gemfire.transaction.event;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Properties;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.TransactionEvent;
import org.apache.geode.cache.TransactionWriter;
import org.apache.geode.cache.TransactionWriterException;

/**
 * Unit Tests for {@link ComposableTransactionWriter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.apache.geode.cache.TransactionWriter
 * @see org.springframework.data.gemfire.transaction.event.ComposableTransactionWriter
 * @since 2.3.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ComposableTransactionWriterUnitTests {

	@Mock
	private TransactionEvent mockTransactionEvent;

	@Mock
	private TransactionWriter mockTransactionWriterOne;

	@Mock
	private TransactionWriter mockTransactionWriterTwo;

	@Test
	public void composeWithNullIsNullSafeAndReturnsNull() {
		assertThat(ComposableTransactionWriter.compose(null, null)).isNull();
	}

	@Test
	public void composeWithSingleNonNullTransactionWriterReturnsTransactionWriter() {

		assertThat(ComposableTransactionWriter.compose(this.mockTransactionWriterOne, null))
			.isEqualTo(this.mockTransactionWriterOne);

		assertThat(ComposableTransactionWriter.compose(null, this.mockTransactionWriterTwo))
			.isEqualTo(this.mockTransactionWriterTwo);
	}

	@Test
	public void composeWithTwoTransactionWritersReturnsComposite() {

		TransactionWriter compositeTransactionWriter =
			ComposableTransactionWriter.compose(this.mockTransactionWriterOne, this.mockTransactionWriterTwo);

		assertThat(compositeTransactionWriter).isInstanceOf(ComposableTransactionWriter.class);

		assertThat(((ComposableTransactionWriter) compositeTransactionWriter).getTransactionWriterOne())
			.isEqualTo(this.mockTransactionWriterOne);

		assertThat(((ComposableTransactionWriter) compositeTransactionWriter).getTransactionWriterTwo())
			.isEqualTo(this.mockTransactionWriterTwo);
	}

	@Test
	public void beforeCommitInvokesComposedTransactionWriters() throws Exception {

		ComposableTransactionWriter.compose(this.mockTransactionWriterOne, this.mockTransactionWriterTwo)
			.beforeCommit(this.mockTransactionEvent);

		verify(this.mockTransactionWriterOne, times(1))
			.beforeCommit(eq(this.mockTransactionEvent));

		verify(this.mockTransactionWriterTwo, times(1))
			.beforeCommit(eq(this.mockTransactionEvent));
	}

	@Test(expected = TransactionWriterException.class)
	public void beforeCommitWhenFirstTransactionWriterThrowsException()
			throws Exception {

		doThrow(new TransactionWriterException("TEST"))
			.when(this.mockTransactionWriterOne).beforeCommit(any(TransactionEvent.class));

		try {
			ComposableTransactionWriter.compose(this.mockTransactionWriterOne, this.mockTransactionWriterTwo)
				.beforeCommit(this.mockTransactionEvent);
		}
		catch (TransactionWriterException expected) {

			assertThat(expected).hasMessage("TEST");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {

			verify(this.mockTransactionWriterOne, times(1))
				.beforeCommit(eq(this.mockTransactionEvent));

			verify(this.mockTransactionWriterTwo, never()).beforeCommit(any(TransactionEvent.class));
		}
	}

	@Test
	public void closeCallsComposedTransactionWritersCloseMethod() {

		ComposableTransactionWriter.compose(this.mockTransactionWriterOne, this.mockTransactionWriterTwo).close();

		verify(this.mockTransactionWriterOne, times(1)).close();
		verify(this.mockTransactionWriterTwo, times(1)).close();
	}

	@Test
	public void initCallsComposedTransactionWritersInitMethod() {

		Properties testProperties = new Properties();

		ComposableTransactionWriter.compose(this.mockTransactionWriterOne, this.mockTransactionWriterTwo)
			.init(testProperties);

		verify(this.mockTransactionWriterOne, times(1)).init(eq(testProperties));
		verify(this.mockTransactionWriterTwo, times(1)).init(eq(testProperties));
	}

	@Test
	public void initializeCallsComposedTransactionWritersInitializeMethod() {

		Cache mockCache = mock(Cache.class);

		Properties testProperties = new Properties();

		ComposableTransactionWriter.compose(this.mockTransactionWriterOne, this.mockTransactionWriterTwo)
			.initialize(mockCache, testProperties);

		verify(this.mockTransactionWriterOne, times(1))
			.initialize(eq(mockCache), eq(testProperties));

		verify(this.mockTransactionWriterTwo, times(1))
			.initialize(eq(mockCache), eq(testProperties));
	}
}
