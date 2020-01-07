/*
 * Copyright 2018-2020 the original author or authors.
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
import java.util.function.Function;

import javax.transaction.Transactional;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

import org.apache.geode.cache.CacheTransactionManager;
import org.apache.geode.cache.CommitConflictException;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.config.annotation.ClientCacheApplication;
import org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.gemfire.transaction.config.EnableGemfireCacheTransactions;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.Assert;

/**
 * Integration tests asserting the proper configuration and behavior of Apache Geode/Pivotal GemFire
 * cache Transactions inside a Spring application context when using SDG to configure
 * the {@link CacheTransactionManager}.
 *
 * Specifically, this test asserts that 2 concurrent threads modifying the same entity inside a cache transaction
 * leads to a {@link CommitConflictException}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @see org.junit.Test
 * @see org.apache.geode.cache.CacheTransactionManager
 * @see org.apache.geode.cache.CommitConflictException
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
 * @see org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions
 * @see org.springframework.data.gemfire.repository.config.EnableGemfireRepositories
 * @see org.springframework.data.gemfire.transaction.config.EnableGemfireCacheTransactions
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class CommitConflictExceptionTransactionalIntegrationTests {

	@Autowired
	private CustomerService customerService;

	@Test
	public void concurrentTransactionalThreadsCauseCommitConflictException() throws Throwable {
		TestFramework.runOnce(new TransactionalCommitConflictMultithreadedTestCase(this.customerService));
	}

	static class TransactionalCommitConflictMultithreadedTestCase extends MultithreadedTestCase {

		private final CustomerService customerService;

		TransactionalCommitConflictMultithreadedTestCase(CustomerService customerService) {

			Assert.notNull(customerService, "CustomerService is required");

			this.customerService = customerService;
		}

		@Override
		public void initialize() {

			super.initialize();

			Customer jonDoe = this.customerService.save(Customer.newCustomer(1L, "Jon Doe"));
			Customer jonDoeLoaded = this.customerService.findById(jonDoe.getId());

			assertThat(jonDoeLoaded).isEqualTo(jonDoe);
		}

		public void thread1() {

			assertTick(0);

			Thread.currentThread().setName("Customer Processing Thread One");

			this.customerService.process(1L, customer -> {

				assertThat(customer.getId()).isEqualTo(1L);
				assertThat(customer.getName()).isEqualTo("Jon Doe");

				customer.setName("Pie Doe");

				waitForTick(2);
				assertTick(2);

				return customer;

			}, Function.identity());
		}

		public void thread2() {

			assertTick(0);

			Thread.currentThread().setName("Customer Processing Thread Two");

			waitForTick(1);
			assertTick(1);

			try {

				this.customerService.process(1L, customer -> {

					assertThat(customer.getId()).isEqualTo(1L);
					assertThat(customer.getName()).isEqualTo("Jon Doe");

					customer.setName("Sour Doe");

					waitForTick(3);
					assertTick(3);

					return customer;

				}, Function.identity());

				fail("Expected CommitConflictException!");

			}
			catch (RuntimeException expected) {
				assertThat(expected).isInstanceOf(GemfireTransactionCommitException.class);
				assertThat(expected).hasCauseInstanceOf(CommitConflictException.class);
			}
		}

		@Override
		public void finish() {

			Customer customer = this.customerService.findById(1L);

			assertThat(customer).isNotNull();
			assertThat(customer.getId()).isEqualTo(1L);
			assertThat(customer.getName()).isEqualTo("Pie Doe");
		}
	}

	@ClientCacheApplication(logLevel = "error")
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
		CustomerService customerService(CustomerRepository customerRepository) {
			return new CustomerService(customerRepository);
		}
	}

	@Data
	@EqualsAndHashCode
	@Region("Customers")
	@RequiredArgsConstructor(staticName = "newCustomer")
	static class Customer implements Serializable {

		@NonNull @Id
		private Long id;

		@NonNull
		private String name;

	}

	public interface CustomerRepository extends CrudRepository<Customer, Long> { }

	@Service
	public static class CustomerService {

		private final CustomerRepository customerRepository;

		public CustomerService(CustomerRepository customerRepository) {

			Assert.notNull(customerRepository, "CustomerRepository is required");

			this.customerRepository = customerRepository;
		}

		protected Customer findById(Long id) {

			Assert.notNull(id, "ID is required");

			return this.customerRepository.findById(id)
				.orElseThrow(() -> newIllegalStateException("No Customer with ID [%d] was found", id));
		}

		@Transactional
		public Customer process(Long id, Function<Customer, Customer> beforeSave,
				Function<Customer, Customer> afterSave) {

			return afterSave.apply(save(beforeSave.apply(findById(id))));
		}

		protected Customer save(Customer customer) {

			Assert.notNull(customer, "Customer is required");

			return this.customerRepository.save(customer);
		}
	}
}
