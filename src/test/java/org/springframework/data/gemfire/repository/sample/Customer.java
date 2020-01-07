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
import org.springframework.util.ObjectUtils;

import lombok.Data;

/**
 * The Customer class is a class abstraction modeling a Customer.
 *
 * @author John Blum
 * @see org.springframework.data.annotation.Id
 * @see Region
 * @since 1.0.0
 */
@Data
@ReplicateRegion("Customers")
@SuppressWarnings("unused")
public class Customer {

	@Id
	private Long id;

	private String firstName;
	private String lastName;

	public Customer() {
	}

	public Customer(Long id) {
		this.id = id;
	}

	public Customer(String firstName, String lastName) {
		this.firstName = firstName;
		this.lastName = lastName;
	}

	public String getName() {
		return String.format("%1$s %2$s", getFirstName(), getLastName());
	}

	protected static boolean equalsIgnoreNull(final Object obj1, final Object obj2) {
		return (obj1 == null ? obj2 == null : obj1.equals(obj2));
	}

	@Override
	public boolean equals(final Object obj) {

		if (this == obj) {
			return true;
		}

		if (!(obj instanceof Customer)) {
			return false;
		}

		Customer that = (Customer) obj;

		return equalsIgnoreNull(this.getId(), that.getId())
			&& ObjectUtils.nullSafeEquals(this.getFirstName(), that.getFirstName())
			&& ObjectUtils.nullSafeEquals(this.getLastName(), that.getLastName());
	}

	protected static int hashCodeIgnoreNull(Object obj) {
		return (obj != null ? obj.hashCode() : 0);
	}

	@Override
	public int hashCode() {
		int hashValue = 17;
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getId());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getFirstName());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getLastName());
		return hashValue;
	}

	@Override
	public String toString() {
		return getName();
	}
}
