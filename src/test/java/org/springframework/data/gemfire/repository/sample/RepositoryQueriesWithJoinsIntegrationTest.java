/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.repository.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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

	private static final AtomicLong ID_SEQUENCE = new AtomicLong(0l);

	@Autowired
	private AccountRepository accountRepo;

	@Autowired
	private CustomerRepository customerRepo;

	protected Account createAccount(final Customer customer, final String number) {
		Account account = new Account(ID_SEQUENCE.incrementAndGet(), customer);
		account.setNumber(number);
		return account;
	}

	protected Customer createCustomer(final String firstName, final String lastName) {
		Customer customer = new Customer(firstName, lastName);
		customer.setId(ID_SEQUENCE.incrementAndGet());
		return customer;
	}

	@Test
	public void testJoinQueries() {
		Customer jonDoe = customerRepo.save(createCustomer("Jon", "Doe"));
		Customer janeDoe = customerRepo.save(createCustomer("Jane", "Doe"));
		Customer jackHandy = customerRepo.save(createCustomer("Jack", "Handy"));

		Account jonAccountOne = accountRepo.save(createAccount(jonDoe, "1"));
		Account jonAccountTwo = accountRepo.save(createAccount(jonDoe, "2"));
		Account janeAccount = accountRepo.save(createAccount(janeDoe, "1"));

		List<Customer> actualCustomersWithAccounts = customerRepo.findCustomersWithAccounts();
		List<Customer> expectedCustomersWithAccounts = Arrays.asList(jonDoe, janeDoe);

		assertNotNull(actualCustomersWithAccounts);
		assertFalse(String.format("Expected Customers (%1$s)!", expectedCustomersWithAccounts),
			actualCustomersWithAccounts.isEmpty());
		assertEquals(String.format("Expected Customers (%1$s); but was (%2$s)!", expectedCustomersWithAccounts, actualCustomersWithAccounts),
			2, actualCustomersWithAccounts.size());
		assertTrue(String.format("Expected Customers (%1$s); but was (%2$s)!", expectedCustomersWithAccounts, actualCustomersWithAccounts),
			actualCustomersWithAccounts.containsAll(expectedCustomersWithAccounts));
	}

}
