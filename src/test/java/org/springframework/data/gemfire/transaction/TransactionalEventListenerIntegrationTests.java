/*
 * Copyright 2018 the original author or authors.
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
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.client.ClientRegionShortcut;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.config.annotation.ClientCacheApplication;
import org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.gemfire.transaction.config.EnableGemfireCacheTransactions;
import org.springframework.data.gemfire.transaction.event.TransactionApplicationEvent;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import org.springframework.util.Assert;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Integration Tests for the Spring {@link TransactionalEventListener} in the context of Apache Geode
 * cache transactions.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ApplicationEvent
 * @see org.springframework.context.ApplicationEventPublisher
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
 * @see TransactionApplicationEvent
 * @see org.springframework.data.gemfire.transaction.config.EnableGemfireCacheTransactions
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.springframework.transaction.annotation.Transactional
 * @see org.springframework.transaction.event.TransactionPhase
 * @see org.springframework.transaction.event.TransactionalEventListener
 * @since 2.3.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class TransactionalEventListenerIntegrationTests {

	private static final String GEMFIRE_LOG_LEVEL = "error";

	@Autowired
	private CustomerService customerService;

	@Autowired
	private TestTransactionEventListener transactionEventListener;

	@Test
	public void successfulEntityTransactionTriggersCommitTransactionEvents() {

		Customer jonDoe = this.customerService.save(Customer.newCustomer(1L, "Jon Doe"));

		Customer jonDoeLoaded = this.customerService.findById(jonDoe.getId());

		assertThat(jonDoeLoaded).isEqualTo(jonDoe);

		assertThat(this.transactionEventListener.getAndClearTransactionDetails()).containsExactly("1");

		assertThat(this.transactionEventListener.getAndClearTransactionPhases())
			.containsExactly(TransactionPhase.BEFORE_COMMIT, TransactionPhase.AFTER_COMMIT);
	}

	@Test(expected = IllegalStateException.class)
	public void failingEntityTransactionTriggersRollbackTransactionEvent() {

		Customer janeDoe = Customer.newCustomer(2L, "Jane Doe");

		try {
			this.customerService.saveFailsAndRollsback(janeDoe);
		}
		catch (IllegalStateException expected) {

			assertThat(this.transactionEventListener.getAndClearTransactionDetails()).containsExactly("2");

			assertThat(this.transactionEventListener.getAndClearTransactionPhases())
				.containsExactly(TransactionPhase.AFTER_ROLLBACK);

			assertThat(expected).hasMessage("TEST");
			assertThat(expected).hasNoCause();

			try {
				this.customerService.findById(janeDoe.getId());
			}
			catch (IllegalStateException alsoExpected) {

				assertThat(alsoExpected).hasMessage("No Customer having ID [2] was found");
				assertThat(alsoExpected).hasNoCause();

				throw alsoExpected;
			}
		}
	}

	@ClientCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	@EnableEntityDefinedRegions(
		basePackageClasses = Customer.class,
		clientRegionShortcut = ClientRegionShortcut.LOCAL
	)
	@EnableGemfireCacheTransactions
	static class TestConfiguration {

		@Bean
		GemfireRepositoryFactoryBean<CustomerRepository, Customer, Long> customerRepositoryFactoryBean() {

			GemfireRepositoryFactoryBean<CustomerRepository, Customer, Long> customerRepositoryFactoryBean
				= new GemfireRepositoryFactoryBean<>(CustomerRepository.class);

			customerRepositoryFactoryBean.setGemfireMappingContext(new GemfireMappingContext());

			return customerRepositoryFactoryBean;
		}

		@Bean
		CustomerService customerService(ApplicationEventPublisher eventPublisher,
				CustomerRepository customerRepository) {

			return new CustomerService(eventPublisher, customerRepository);
		}

		@Bean
		TestTransactionEventListener testTransactionEventListener() {
			return new TestTransactionEventListener();
		}
	}

	@Data
	@EqualsAndHashCode
	@Region("Customers")
	@RequiredArgsConstructor(staticName = "newCustomer")
	static class Customer implements Serializable {

		@Id @NonNull
		private Long id;

		@NonNull
		private String name;

	}

	public interface CustomerRepository extends CrudRepository<Customer, Long> { }

	@Service
	public static class CustomerService {

		private ApplicationEventPublisher eventPublisher;

		private final CustomerRepository customerRepository;

		public CustomerService(ApplicationEventPublisher eventPublisher, CustomerRepository customerRepository) {

			Assert.notNull(eventPublisher, "ApplicationEventPublisher is required");
			Assert.notNull(customerRepository, "CustomerRepository is required");

			this.eventPublisher = eventPublisher;
			this.customerRepository = customerRepository;
		}

		public Customer findById(Long id) {
			return this.customerRepository.findById(id)
				.orElseThrow(() -> newIllegalStateException("No Customer having ID [%d] was found", id));
		}

		@Transactional
		public Customer save(Customer customer) {

			customer = this.customerRepository.save(customer);

			this.eventPublisher.publishEvent(new TransactionApplicationEvent(this,
				String.valueOf(customer.getId())));

			return customer;
		}

		@Transactional
		public Customer saveFailsAndRollsback(Customer customer) {

			this.customerRepository.save(customer);

			this.eventPublisher.publishEvent(new TransactionApplicationEvent(this,
				String.valueOf(customer.getId())));

			throw newIllegalStateException("TEST");
		}
	}

	@Component
	public static class TestTransactionEventListener {

		private final List<TransactionPhase> transactionPhases = new ArrayList<>();

		private final Set<String> transactionDetails = new HashSet<>();

		public Set<String> getAndClearTransactionDetails() {

			Set<String> copy = new HashSet<>(this.transactionDetails);

			this.transactionDetails.clear();

			return copy;
		}

		public List<TransactionPhase> getAndClearTransactionPhases() {

			List<TransactionPhase> copy = new ArrayList<>(this.transactionPhases);

			this.transactionPhases.clear();

			return copy;
		}

		private void appendTransactionPhase(TransactionPhase transactionPhase) {
			Optional.ofNullable(transactionPhase).ifPresent(this.transactionPhases::add);
		}

		private void extractTransactionDetails(ApplicationEvent event) {

			Optional.ofNullable(event)
				.filter(TransactionApplicationEvent.class::isInstance)
				.map(TransactionApplicationEvent.class::cast)
				.flatMap(TransactionApplicationEvent::getDetails)
				.ifPresent(this.transactionDetails::add);
		}

		@TransactionalEventListener(phase = TransactionPhase.BEFORE_COMMIT)
		public void handleTransactionBeforeCommit(ApplicationEvent event) {
			appendTransactionPhase(TransactionPhase.BEFORE_COMMIT);
			extractTransactionDetails(event);
		}

		@TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
		public void handleTransactionAfterCommit(ApplicationEvent event) {
			appendTransactionPhase(TransactionPhase.AFTER_COMMIT);
			extractTransactionDetails(event);
		}

		@TransactionalEventListener(phase = TransactionPhase.AFTER_ROLLBACK)
		public void handleTransactionAfterRollback(ApplicationEvent event) {
			appendTransactionPhase(TransactionPhase.AFTER_ROLLBACK);
			extractTransactionDetails(event);
		}
	}
}
