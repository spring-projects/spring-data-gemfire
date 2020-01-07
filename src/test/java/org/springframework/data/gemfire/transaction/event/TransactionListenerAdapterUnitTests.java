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
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.function.Consumer;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.apache.geode.cache.TransactionEvent;

import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationEventPublisher;

/**
 * Unit Tests for {@link TransactionListenerAdapter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.context.ApplicationEventPublisher
 * @see org.springframework.data.gemfire.transaction.event.TransactionListenerAdapter
 * @since 2.3.0
 */
@RunWith(MockitoJUnitRunner.class)
public class TransactionListenerAdapterUnitTests {

	@Mock
	private ApplicationEventPublisher mockApplicationEventPublisher;

	@Mock
	private TransactionEvent mockTransactionEvent;

	@Test
	public void constructTransactionListenerAdapterIsCorrect() {

		TransactionListenerAdapter listener = new TransactionListenerAdapter(this.mockApplicationEventPublisher);

		assertThat(listener).isNotNull();
		assertThat(listener.getApplicationEventPublisher()).isEqualTo(this.mockApplicationEventPublisher);
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructTransactionListenerAdapterWithNullApplicationEventPublisherThrowsIllegalArgumentException() {

		try {
			new TransactionListenerAdapter(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("ApplicationEventPublisher must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	private void invokingApplicationEventPublisherTest(Consumer<TransactionListenerAdapter> listenerConsumer) {

		doAnswer(invocation -> {

			ApplicationEvent applicationEvent = invocation.getArgument(0, ApplicationEvent.class);

			assertThat(applicationEvent).isInstanceOf(TransactionApplicationEvent.class);
			assertThat(applicationEvent.getSource()).isEqualTo(this.mockTransactionEvent);

			return null;

		}).when(this.mockApplicationEventPublisher).publishEvent(any(ApplicationEvent.class));

		TransactionListenerAdapter listener = new TransactionListenerAdapter(this.mockApplicationEventPublisher);

		listenerConsumer.accept(listener);

		verify(this.mockApplicationEventPublisher, times(1))
			.publishEvent(isA(TransactionApplicationEvent.class));
	}

	private void nonInvokingApplicationEventPublisherTest(Consumer<TransactionListenerAdapter> listenerConsumer) {

		TransactionListenerAdapter listener = new TransactionListenerAdapter(this.mockApplicationEventPublisher);

		listenerConsumer.accept(listener);

		verify(this.mockApplicationEventPublisher, never()).publishEvent(any(ApplicationEvent.class));
	}

	@Test
	public void beforeCommitDoesNotInvokeApplicationEventPublisher() {
		nonInvokingApplicationEventPublisherTest(listener -> listener.beforeCommit(this.mockTransactionEvent));
	}

	@Test
	public void afterCommitInvokesApplicationEventPublisher() {
		invokingApplicationEventPublisherTest(listener -> listener.afterCommit(this.mockTransactionEvent));
	}

	@Test
	public void afterFailedCommitDoesNotInvokeApplicationEventPublisher() {
		nonInvokingApplicationEventPublisherTest(listener -> listener.afterFailedCommit(this.mockTransactionEvent));
	}

	@Test
	public void afterRollbackInvokesApplicationEventPublisher() {
		invokingApplicationEventPublisherTest(listener -> listener.afterRollback(this.mockTransactionEvent));
	}
}
