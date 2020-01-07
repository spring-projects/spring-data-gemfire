/*
 * Copyright 2010-2020 the original author or authors.
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

import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.gemfire.mapping.annotation.ReplicateRegion;
import org.springframework.util.Assert;

/**
 * The Account class is an abstract data type (ADT) for modeling customer accounts.
 *
 * @author John Blum
 * @see Region
 * @since 1.0.0
 */
@ReplicateRegion("Accounts")
@SuppressWarnings("unused")
public class Account {

	@Id
	private Long id;

	private Long customerId;

	private String number;

	public Account(final Long customerId) {
		Assert.notNull(customerId, "The Customer ID to which this Account is associated cannot be null!");
		this.customerId = customerId;
	}

	public Account(final Customer customer) {
		this(customer.getId());
	}

	public Account(final Long customerId, final String number) {
		this(customerId);
		this.number = number;
	}

	public Account(final Customer customer, final String number) {
		this(customer);
		this.number = number;
	}

	public Account(final Long accountId, final Long customerId) {
		this(customerId);
		this.id = accountId;
	}

	public Account(final Long accountId, final Customer customer) {
		this(customer);
		this.id = accountId;
	}

	public Long getId() {
		return id;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public Long getCustomerId() {
		return customerId;
	}

	public void setCustomerId(final Long customerId) {
		this.customerId = customerId;
	}

	public String getNumber() {
		return number;
	}

	public void setNumber(final String number) {
		this.number = number;
	}

	@Override
	public String toString() {
		return String.format("Customer (%1$d) Account (%2$d) #(%3$s)", getCustomerId(), getId(), getNumber());
	}

}
