/*
 * Copyright 2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
import org.springframework.util.ObjectUtils;

/**
 *
 * @author Oliver Gierke
 */
@Region("address")
public class Address {

	public String street;
	public String city;

	@Id
	public String zipCode;

	@Override
	public boolean equals(Object obj) {

		if (this == obj) {
			return true;
		}

		if (!(obj instanceof Address)) {
			return false;
		}

		Address that = (Address) obj;

		return ObjectUtils.nullSafeEquals(this.street, that.street)
			&& ObjectUtils.nullSafeEquals(this.city, that.city)
			&& ObjectUtils.nullSafeEquals(this.zipCode, that.zipCode);
	}

	@Override
	public int hashCode() {

		int hashValue = 17;

		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(this.street);
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(this.city);
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(this.zipCode);

		return hashValue;
	}

	@Override
	public String toString() {
		return String.format("%1$s %2$s, %3$s", this.street, this.city, this.zipCode);
	}
}
