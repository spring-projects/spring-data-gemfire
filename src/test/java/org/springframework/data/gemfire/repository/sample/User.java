/*
 * Copyright 2010-2013 the original author or authors.
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

import java.util.Calendar;

import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.mapping.Region;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * The User class represents an authorized user of a service or computer system, etc.
 * <p/>
 * @author John Blum
 * @since 1.3.3 (Spring Data GemFire)
 * @since 7.0.1 (GemFire)
 */
@SuppressWarnings("unused")
@Region("Users")
public class User {

	private Boolean active = true;

	private Calendar since;

	private String email;

	@Id
	private final String username;

	public User(final String username) {
		Assert.hasText(username, "The username is required!");
		this.username = username;
	}

	public Boolean getActive() {
		return active;
	}

	public boolean isActive() {
		return Boolean.TRUE.equals(getActive());
	}

	public void setActive(final Boolean active) {
		this.active = Boolean.TRUE.equals(active);
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(final String email) {
		this.email = email;
	}

	public Calendar getSince() {
		return since;
	}

	public void setSince(final Calendar since) {
		this.since = since;
	}

	public String getUsername() {
		return username;
	}

	private static boolean equalsIgnoreNull(final Object obj1, final Object obj2) {
		return (obj1 == null ? obj2 == null : obj1.equals(obj2));
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == this) {
			return true;
		}

		if (!(obj instanceof User)) {
			return false;
		}

		User that = (User) obj;

		return this.getUsername().equals(that.getUsername())
			&& ObjectUtils.nullSafeEquals(this.getEmail(), that.getEmail());
	}

	@Override
	public int hashCode() {
		int hashValue = 17;
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getEmail());
		hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(getUsername());
		return hashValue;
	}

	@Override
	public String toString() {
		return getUsername();
	}

}
