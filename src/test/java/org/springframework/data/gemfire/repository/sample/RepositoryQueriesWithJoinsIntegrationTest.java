/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.repository.sample;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The RepositoryQueriesWithJoinsTest class is a test suite of test cases testing the use of JOINS between 2 Regions
 * in GemFire OQL queries (SELECT statements).
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.0.0
 */
@ContextConfiguration("repositoryQueriesWithJoins.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class RepositoryQueriesWithJoinsIntegrationTest {

	private static final AtomicLong ID_SEQUENCE = new AtomicLong(0L);

	@Autowired
	private AccountRepository accountRepo;

	@Autowired
	private CustomerRepository customerRepo;

	protected Account newAccount(Customer customer, String number) {
		Account account = new Account(ID_SEQUENCE.incrementAndGet(), customer);
		account.setNumber(number);
		return account;
	}

	protected Customer newCustomer(String firstName, String lastName) {
		Customer customer = new Customer(firstName, lastName);
		customer.setId(ID_SEQUENCE.incrementAndGet());
		return customer;
	}

	@Test
	public void joinQueriesWork() {
		Customer jonDoe = customerRepo.save(newCustomer("Jon", "Doe"));
		Customer janeDoe = customerRepo.save(newCustomer("Jane", "Doe"));
		Customer jackHandy = customerRepo.save(newCustomer("Jack", "Handy"));

		Account jonAccountOne = accountRepo.save(newAccount(jonDoe, "1"));
		Account jonAccountTwo = accountRepo.save(newAccount(jonDoe, "2"));
		Account janeAccount = accountRepo.save(newAccount(janeDoe, "1"));

		List<Customer> actualCustomersWithAccounts = customerRepo.findCustomersWithAccounts();
		List<Customer> expectedCustomersWithAccounts = Arrays.asList(jonDoe, janeDoe);

		assertThat(actualCustomersWithAccounts).isNotNull();
		assertThat(actualCustomersWithAccounts).hasSize(2);
		assertThat(actualCustomersWithAccounts).containsAll(expectedCustomersWithAccounts);
	}
}
