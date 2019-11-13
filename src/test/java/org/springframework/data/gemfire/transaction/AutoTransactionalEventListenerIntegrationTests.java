/*
 * Copyright 2019 the original author or authors.
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
package org.springframework.data.gemfire.transaction;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.EventListener;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.runner.RunWith;

import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.TransactionEvent;
import org.apache.geode.cache.TransactionWriter;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer;
import org.springframework.data.gemfire.transaction.config.EnableGemfireCacheTransactions;
import org.springframework.data.gemfire.transaction.event.TransactionApplicationEvent;
import org.springframework.stereotype.Component;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * Integration Tests for the Spring {@link TransactionalEventListener} in the context of Apache Geode
 * cache transactions when auto-publishing of transaction events is enabled.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
 * @see org.springframework.data.gemfire.transaction.config.EnableGemfireCacheTransactions
 * @see org.springframework.data.gemfire.transaction.event.TransactionApplicationEvent
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.springframework.transaction.event.TransactionPhase
 * @see org.springframework.transaction.event.TransactionalEventListener
 * @since 2.3.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class AutoTransactionalEventListenerIntegrationTests extends AbstractTransactionalEventListenerIntegrationTests {

	@Autowired
	private TestTransactionEventListener transactionEventListener;

	@Autowired
	@Qualifier("MockTransactionWriter")
	private TransactionWriter mockTransactionWriter;

	@Override
	protected void assertTransactionEventListenerOnSuccess() throws Exception {

		assertThat(this.transactionEventListener.isBeforeCommitInvoked()).isFalse();

		assertThat(this.transactionEventListener.getAndClearTransactionPhases())
			.containsExactly(TransactionPhase.AFTER_COMMIT);

		verify(this.mockTransactionWriter, times(1)).beforeCommit(any(TransactionEvent.class));
	}

	@Override
	protected void assertTransactionEventListenerOnFailure() {

		assertThat(this.transactionEventListener.isBeforeCommitInvoked()).isFalse();

		assertThat(this.transactionEventListener.getAndClearTransactionPhases())
			.containsExactly(TransactionPhase.AFTER_ROLLBACK);
	}

	@PeerCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	@EnableEntityDefinedRegions(
		basePackageClasses = Customer.class,
		serverRegionShortcut = RegionShortcut.LOCAL
	)
	@EnableGemfireCacheTransactions(enableAutoTransactionEventPublishing = true)
	@Import(CustomerRepositoryConfiguration.class)
	static class TestConfiguration {

		@Bean
		CustomerService customerService(CustomerRepository customerRepository) {
			return new CustomerService(customerRepository);
		}

		@Bean
		TestTransactionEventListener testTransactionEventListener() {
			return new TestTransactionEventListener();
		}

		@Bean("MockTransactionWriter")
		TransactionWriter mockTransactionWriter() {
			return mock(TransactionWriter.class);
		}

		@Bean
		PeerCacheConfigurer transactionWriterRegisteringCacheConfigurer(
				@Qualifier("MockTransactionWriter") TransactionWriter transactionWriter) {

			return (beanName, bean) -> bean.setTransactionWriter(transactionWriter);
		}
	}

	@Component
	public static class TestTransactionEventListener implements EventListener {

		private AtomicBoolean beforeCommitInvoked = new AtomicBoolean(false);

		private List<TransactionPhase> transactionPhases = new ArrayList<>();

		public List<TransactionPhase> getAndClearTransactionPhases() {

			List<TransactionPhase> copy = new ArrayList<>(this.transactionPhases);

			this.transactionPhases.clear();

			return copy;
		}

		private boolean isBeforeCommitInvoked() {
			return this.beforeCommitInvoked.getAndSet(false);
		}

		private void handleTransactionEvent(TransactionApplicationEvent event, TransactionPhase transactionPhase) {

			Optional.ofNullable(event)
				.map(TransactionApplicationEvent::getSource)
				.filter(TransactionEvent.class::isInstance)
				.ifPresent(transactionEvent -> this.transactionPhases.add(transactionPhase));
		}

		@TransactionalEventListener(phase = TransactionPhase.BEFORE_COMMIT)
		public void handleTransactionBeforeCommit(TransactionApplicationEvent event) {
			this.beforeCommitInvoked.set(true);
			handleTransactionEvent(event, TransactionPhase.BEFORE_COMMIT);
		}

		@TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
		public void handleTransactionAfterCommit(TransactionApplicationEvent event) {
			handleTransactionEvent(event, TransactionPhase.AFTER_COMMIT);
		}

		@TransactionalEventListener(phase = TransactionPhase.AFTER_ROLLBACK)
		public void handleTransactionAfterRollback(TransactionApplicationEvent event) {
			handleTransactionEvent(event, TransactionPhase.AFTER_ROLLBACK);
		}
	}
}
